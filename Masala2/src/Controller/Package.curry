module Controller.Package
  ( mainPackageController, newPackageForm, editPackageForm
  , addMaintainerPackageForm, addMaintainerPackageFormAdmin ) where

import Data.List ( last, sortBy )

import Data.Maybe
import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.Roles
import Config.UserProcesses
import Controller.Version   ( showVersionController )
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import View.EntitiesToHtml
import View.Package
import View.Version
import Database.CDBI.Connection

type NewPackage = (String,Bool)

--- Choose the controller for a Package entity according to the URL parameter.
--- In addition to the standard URL parameters, this controller implements
--- also the following URLs:
--- * `...?Package/Version/PACKAGE/latest`: show the latest version of PACKAGE
--- * `...?Package/Version/PACKAGE/VERSION`: show version VERSION of PACKAGE
mainPackageController :: Controller
mainPackageController = do
  args <- getControllerParams
  case args of
    [] -> allPackagesController
    ["list"] -> allPackagesController
    ["new"] -> newPackageController
    ["show",s] -> controllerOnKey s showPackageController
    ["edit",s] -> controllerOnKey s editPackageController
    ["addmaintainer",s] -> controllerOnKey s addMaintainerPackageController
    ["toggleabandoned",s] -> controllerOnKey s toggleAbandonedPackageController
    ["delete",s]    -> controllerOnKey s deletePackageController
    ["destroy",s]   -> controllerOnKey s destroyPackageController
    ["Version",p,v] -> if v == "latest"
                         then getPackageWithName p >>=
                                maybe displayUrlError showPackageController
                         else getPackageVersionByName p v >>=
                                maybe displayUrlError showVersionController
    _ -> displayUrlError

--- Shows a form to create a new Package entity.
newPackageController :: Controller
newPackageController =
  checkAuthorization (packageOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore newPackageStore sinfo ("",False)
        return [formElem newPackageForm])

--- A WUI form to create a new Package entity.
--- The default values for the fields are stored in 'newPackageStore'.
newPackageForm :: HtmlFormDef (UserSessionInfo,WuiStore NewPackage)
newPackageForm =
  pwui2FormDef "Controller.Package.newPackageForm" newPackageStore
   (\_ -> wPackage)
   (\_ entity ->
     checkAuthorization (packageOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createPackageT entity))
         (\newentity ->
           do setPageMessage "New Package created"
              nextInProcessOr (redirectController (showRoute newentity))
               Nothing)))
   (\sinfo ->
     let phantom = failed :: Package
     in renderWUI sinfo "Create new Package" "Create" (listRoute phantom) ())

--- The data stored for executing the "new entity" WUI form.
newPackageStore :: SessionStore (UserSessionInfo,WuiStore NewPackage)
newPackageStore = sessionStore "newPackageStore"

--- Transaction to persist a new Package entity to the database.
createPackageT :: NewPackage -> DBAction Package
createPackageT (name,abandoned) =
  do newentity <- newPackage name abandoned
     return newentity

--- Shows a form to edit the given Package entity.
editPackageController :: Package -> Controller
editPackageController packageToEdit =
  checkAuthorization (packageOperationAllowed (UpdateEntity packageToEdit))
   $ (\sinfo ->
     do setParWuiStore editPackageStore (sinfo,packageToEdit) packageToEdit
        return [formElem editPackageForm])

--- A WUI form to edit a Package entity.
--- The default values for the fields are stored in 'editPackageStore'.
editPackageForm :: HtmlFormDef ((UserSessionInfo,Package),WuiStore Package)
editPackageForm =
  pwui2FormDef "Controller.Package.editPackageForm" editPackageStore
   (\(_,package) -> wPackageType package)
   (\_ entity@packageToEdit ->
     checkAuthorization (packageOperationAllowed (UpdateEntity packageToEdit))
      (\_ ->
        transactionController (runT (updatePackageT entity))
         (const
           (do setPageMessage "Package updated"
               nextInProcessOr (redirectController (showRoute packageToEdit))
                Nothing))))
   (\(sinfo,entity) ->
     renderWUI sinfo "Edit Package" "Change" (listRoute entity) ())

--- The data stored for executing the edit WUI form.
editPackageStore :: SessionStore ((UserSessionInfo,Package),WuiStore Package)
editPackageStore = sessionStore "editPackageStore"

--- Transaction to persist modifications of a given Package entity
--- to the database.
updatePackageT :: Package -> DBAction ()
updatePackageT package = updatePackage package

--- A controller to toggle the abandoned status of the given Package entity.
toggleAbandonedPackageController :: Package -> Controller
toggleAbandonedPackageController pkg =
  checkAuthorization (packageOperationAllowed (UpdateEntity pkg)) $ \_ -> do
    let newpkg = setPackageAbandoned pkg (not (packageAbandoned pkg))
    transactionController (runT (updatePackage newpkg))
      (const $ do
         setPageMessage "Abandoned status changed"
         nextInProcessOr (redirectController (showRoute pkg)) Nothing)

--- Deletes a given Package entity (after asking for confirmation)
--- and proceeds with the list controller.
deletePackageController :: Package -> Controller
deletePackageController package =
  checkAuthorization (packageOperationAllowed (DeleteEntity package))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat ["Really delete entity \"",packageToShortView package,"\"?"]))

--- Deletes a given Package entity
--- and proceeds with the list controller.
destroyPackageController :: Package -> Controller
destroyPackageController package =
  checkAuthorization (packageOperationAllowed (DeleteEntity package))
   $ (\_ ->
     transactionController (runT (deletePackageT package))
      (const
        (do setPageMessage "Package deleted"
            redirectController (listRoute package))))

--- Transaction to delete a given Package entity.
deletePackageT :: Package -> DBAction ()
deletePackageT package = deletePackage package

--- Lists all Package entities with buttons to show, delete,
--- or edit an entity.
listPackageController :: Controller
listPackageController =
  checkAuthorization (packageOperationAllowed ListEntities)
   $ (\sinfo ->
     do packages <- runQ queryAllPackages
        return (listPackageView sinfo packages))

--- Lists all Package entities.
allPackagesController :: Controller
allPackagesController =
  checkAuthorization (packageOperationAllowed ListEntities)
   $ (\sinfo ->
     do packages <- runQ queryAllPackages
        return (allPackagesView sinfo "Index of all packages" packages))

--- Shows a Package entity.
showPackageController :: Package -> Controller
showPackageController package =
  checkAuthorization (packageOperationAllowed (ShowEntity package)) $ \_ -> do
    versions <- getPackageVersions package
    if null versions
      then displayError "Package has no versions!"
      else showVersionController (last (sortBy leqVersion versions))

addMaintainerPackageController :: Package -> Controller
addMaintainerPackageController package = 
  checkAuthorization (packageOperationAllowed (UpdateEntity package))
    $ (\sinfo ->
        if isAdminSession sinfo 
          then do 
            users <- getNonMaintainersOfPackage package
            let userNames = map userLoginName $ filter (\user -> userRole user /= roleInvalid) users
            setParWuiStore addMaintainerPackageStoreAdmin (sinfo, package, userNames) (package,[])
            return [formElem addMaintainerPackageFormAdmin]
          else do 
            setParWuiStore addMaintainerPackageStore (sinfo, package) (package, "")
            return [formElem addMaintainerPackageForm]
    )

addMaintainerPackageStoreAdmin :: SessionStore ((UserSessionInfo,Package,[String]),WuiStore (Package,[String]))
addMaintainerPackageStoreAdmin = sessionStore "addMaintainerPackageStoreAdmin"

addMaintainerPackageStore :: SessionStore ((UserSessionInfo,Package),WuiStore (Package,String))
addMaintainerPackageStore = sessionStore "addMaintainerPackageStore"

addMaintainerPackageFormAdmin :: HtmlFormDef ((UserSessionInfo,Package,[String]),WuiStore (Package,[String]))
addMaintainerPackageFormAdmin =
  pwui2FormDef "Controller.Package.addMaintainerPackageFormAdmin" addMaintainerPackageStoreAdmin
    (\(_,package,userNames) -> wAddMaintainerTypeAdmin package userNames)
    (\_ (package, maintainerNames) -> do
      maintainers <- mapM getUserByName maintainerNames
      let newMaintainerKeys = map userKey (catMaybes maintainers)
      let dbactions = map (flip newMaintainer (packageKey package)) newMaintainerKeys
      case dbactions of 
        [] -> redirectController (showRoute package)
        _ -> do 
          result <- runT $ foldl1 (>+) dbactions
          case result of 
            Left err -> do
              setPageMessage "Something went wrong, please try again"
              redirectController (entityRoute "addmaintainer" package)
            Right _ -> do
              setPageMessage "Maintainers added successfully"
              redirectController (showRoute package)
    )
    (\(sinfo,entity,_) ->
      renderWUI sinfo "Please choose the Users you want to add as Maintainers"
        "Add Maintainers" (showRoute entity) ())

addMaintainerPackageForm :: HtmlFormDef ((UserSessionInfo,Package),WuiStore (Package, String))
addMaintainerPackageForm =
  pwui2FormDef "Controller.Package.addMaintainerPackageForm" addMaintainerPackageStore
    (\(_,package) -> wAddMaintainerType package)
    (\_ (package, maintainerName) -> do
      userResult <- getUserByName maintainerName
      case userResult of 
        Nothing -> do
          setPageMessage "User with that name does not exist"
          redirectController (entityRoute "addmaintainer" package)
        Just user -> do 
          isMaintainer <- checkIfMaintainer package user
          case isMaintainer of 
            False -> do 
              result <- runT (newMaintainer (userKey user) (packageKey package))
              case result of
                Left err -> do
                  setPageMessage "Something went wrong, please try again"
                  redirectController (entityRoute "addmaintainer" package)
                Right _ -> do 
                  setPageMessage "Maintainer successfully added"
                  redirectController (showRoute package)
            True -> do 
              setPageMessage "User is already a maintainer"
              redirectController (entityRoute "addmaintainer" package)
    )
    (\(sinfo,entity) ->
      renderWUI sinfo "Please type in the name of the User you want to add as Maintainer"
        "Add Maintainer" (showRoute entity) ())