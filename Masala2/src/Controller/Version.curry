module Controller.Version
  ( mainVersionController, newVersionForm, editVersionForm
  , showVersionController, deleteVersionT ) where

import Control.Monad        ( unless )
import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.UserProcesses
import Controller.Category
import Controller.Mail
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import System.PackageHelpers ( publishPackageVersion )
import System.PreludeHelpers
import View.EntitiesToHtml
import View.Version
import Database.CDBI.Connection

type NewVersion = (String
                  ,Bool
                  ,Bool
                  ,String
                  ,String
                  ,Int
                  ,ClockTime
                  ,Bool
                  ,User
                  ,Package
                  ,[Package]
                  ,[CurryModule])

--- Choose the controller for a Version entity according to the URL parameter.
mainVersionController :: Controller
mainVersionController = do
  args <- getControllerParams
  case args of
    [] -> listVersionController
    ["list"] -> listVersionController
    ["unpublished"] -> allUnpublishedController
    ["new"] -> newVersionController
    ["shows",s] -> controllerOnKey s showStandardVersionController
    ["show",s] -> controllerOnKey s showVersionController
    ["edit",s] -> controllerOnKey s editVersionController
    ["toggledepr",s]   -> controllerOnKey s toggleDeprecatedVersionController
    ["togglepublic",s] -> controllerOnKey s togglePublishedVersionController
    ["requestpublish",s] -> controllerOnKey s requestPublishVersionController
    ["delete",s]       -> controllerOnKey s deleteVersionController
    ["destroy",s]      -> controllerOnKey s destroyVersionController
    _                  -> displayUrlError

--- Shows a form to create a new Version entity.
newVersionController :: Controller
newVersionController =
  checkAuthorization (versionOperationAllowed NewEntity)
   $ (\sinfo ->
     do allUsers <- runQ queryAllUsers
        allPackages <- runQ queryAllPackages
        --allPackages <- runQ queryAllPackages
        allCurryModules <- runQ queryAllCurryModules
        ctime <- getClockTime
        setParWuiStore newVersionStore
         (sinfo,allUsers,allPackages,allPackages,allCurryModules)
         (""
         ,False
         ,False
         ,""
         ,""
         ,0
         ,ctime
         ,False
         ,head allUsers
         ,head allPackages
         ,[]
         ,[])
        return [formElem newVersionForm])

--- A WUI form to create a new Version entity.
--- The default values for the fields are stored in 'newVersionStore'.
newVersionForm
  :: HtmlFormDef ((UserSessionInfo,[User],[Package],[Package],[CurryModule])
                 ,WuiStore NewVersion)
newVersionForm =
  pwui2FormDef "Controller.Version.newVersionForm" newVersionStore
   (\(_,possibleUsers,possibleVersioningPackages,possibleDependingPackages
     ,possibleCurryModules) ->
     wVersion possibleUsers possibleVersioningPackages possibleDependingPackages
      possibleCurryModules)
   (\_ entity ->
     checkAuthorization (versionOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createVersionT entity))
         (\newentity ->
           do setPageMessage "New Version created"
              nextInProcessOr (redirectController (showRoute newentity))
               Nothing)))
   (\(sinfo,_,_,_,_) ->
     let phantom = failed :: Version
     in renderWUI sinfo "Create new Version" "Create" (listRoute phantom) ())

--- The data stored for executing the "new entity" WUI form.
newVersionStore
  :: SessionStore ((UserSessionInfo,[User],[Package],[Package],[CurryModule])
                  ,WuiStore NewVersion)
newVersionStore = sessionStore "newVersionStore"

--- Transaction to persist a new Version entity to the database.
createVersionT :: NewVersion -> DBAction Version
createVersionT
    (version,published,tested,description,jobStatus,downloads,uploadDate
    ,deprecated,user,package,dependingpackages,curryModules) =
  do newentity <- newVersionWithPackageVersioningKeyWithUserUploadKey version
                  published tested description jobStatus downloads uploadDate
                  deprecated (packageKey package) (userKey user)
     addDepending dependingpackages newentity
     addExporting curryModules newentity
     return newentity

--- Shows a form to edit the given Version entity.
editVersionController :: Version -> Controller
editVersionController versionToEdit =
  checkAuthorization (versionOperationAllowed (UpdateEntity versionToEdit))
   $ (\sinfo ->
     do allUsers <- runQ queryAllUsers
        allPackages <- runQ queryAllPackages
        --allPackages <- runQ queryAllPackages
        allCurryModules <- runQ queryAllCurryModules
        uploadUser <- runJustT (getUploadUser versionToEdit)
        versioningPackage <- runJustT (getVersioningPackage versionToEdit)
        dependingPackages <- runJustT
                              (getDependingVersionPackages versionToEdit)
        exportingCurryModules <- runJustT
                                  (getExportingVersionCurryModules
                                    versionToEdit)
        setParWuiStore editVersionStore
         (sinfo
         ,versionToEdit
         ,uploadUser
         ,versioningPackage
         ,allUsers
         ,allPackages
         ,allPackages
         ,allCurryModules)
         (versionToEdit,dependingPackages,exportingCurryModules)
        return [formElem editVersionForm])

--- A WUI form to edit a Version entity.
--- The default values for the fields are stored in 'editVersionStore'.
editVersionForm
  :: HtmlFormDef ((UserSessionInfo
                  ,Version
                  ,User
                  ,Package
                  ,[User]
                  ,[Package]
                  ,[Package]
                  ,[CurryModule])
                 ,WuiStore (Version,[Package],[CurryModule]))
editVersionForm =
  pwui2FormDef "Controller.Version.editVersionForm" editVersionStore
   (\(_,version,relatedUser,relatedPackage,possibleUsers
     ,possibleVersioningPackages
     ,possibleDependingPackages,possibleCurryModules) ->
     wVersionType version relatedUser relatedPackage possibleUsers
      possibleVersioningPackages
      possibleDependingPackages
      possibleCurryModules)
   (\_ entity@(versionToEdit,_,_) ->
     checkAuthorization (versionOperationAllowed (UpdateEntity versionToEdit))
      (\_ ->
        transactionController (runT (updateVersionT entity))
         (const
           (do setPageMessage "Version updated"
               nextInProcessOr (redirectController (showRoute versionToEdit))
                Nothing))))
   (\(sinfo,entity,_,_,_,_,_,_) ->
     renderWUI sinfo "Edit Version" "Change" (listRoute entity) ())

--- The data stored for executing the edit WUI form.
editVersionStore
  :: SessionStore ((UserSessionInfo
                   ,Version
                   ,User
                   ,Package
                   ,[User]
                   ,[Package]
                   ,[Package]
                   ,[CurryModule])
                  ,WuiStore (Version,[Package],[CurryModule]))
editVersionStore = sessionStore "editVersionStore"

--- Transaction to persist modifications of a given Version entity
--- to the database.
updateVersionT :: (Version,[Package],[CurryModule]) -> DBAction ()
updateVersionT (version,packagesDepending,curryModulesExporting) =
  do updateVersion version
     oldDependingPackages <- getDependingVersionPackages version
     removeDepending oldDependingPackages version
     oldExportingCurryModules <- getExportingVersionCurryModules version
     removeExporting oldExportingCurryModules version
     addDepending packagesDepending version
     addExporting curryModulesExporting version

--- A controller to toggle the deprecated status of the given Version entity.
toggleDeprecatedVersionController :: Version -> Controller
toggleDeprecatedVersionController vers =
  checkAuthorization (versionOperationAllowed (UpdateEntity vers)) $ \_ -> do
    let newvers = setVersionDeprecated vers (not (versionDeprecated vers))
    transactionController (runT (updateVersion newvers))
      (const $ do
         setPageMessage "Deprecated status changed"
         nextInProcessOr (redirectController (showRoute vers)) Nothing)

--- A controller to toggle the published status of the given Version entity.
togglePublishedVersionController :: Version -> Controller
togglePublishedVersionController vers = do
  pkg <- runJustT (getVersioningPackage vers)
  checkAuthorization (adminOrMaintainer pkg) $ \sinfo -> do
    let newvers = setVersionPublished vers True
    if isNotTrustedUserSession sinfo
      then displayError "Operation not allowed"
      else publishPackageVersion (packageName pkg) (versionVersion vers) >>=
        either
          (\e -> do
             setPageMessage $ "Package version could not be published: " ++ e
             nextInProcessOr (redirectController (showRoute vers)) Nothing )
          (\o ->
             transactionController (runT (updateVersion newvers)) $ \_ -> do
               setPageMessage $
                 "Package version has been scheduled for publishing: " ++ o
               nextInProcessOr (redirectController (showRoute vers)) Nothing )

--- A controller to request the publication of the given Version entity.
requestPublishVersionController :: Version -> Controller
requestPublishVersionController vers = do
  pkg <- runJustT (getVersioningPackage vers)
  checkAuthorization (adminOrMaintainer pkg) $ \_ -> do
    let subj = "Please publish my package " ++ packageName pkg ++ "-" ++
               versionVersion vers
        cnts = subj ++ " or change my role to a \"trusted user\" so that " ++
               "I can publish this package by myself."
    (adminMailController subj cnts)

--- Deletes a given Version entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteVersionController :: Version -> Controller
deleteVersionController version =
  checkAuthorization (versionOperationAllowed (DeleteEntity version))
   $ (\sinfo ->
     confirmDeletionPageWithCancelURL sinfo
      (concat ["Really delete version \"",versionToShortView version,"\"?"])
      (showRoute version))

--- Deletes a given Version entity
--- and proceeds with the list controller.
destroyVersionController :: Version -> Controller
destroyVersionController version =
  checkAuthorization (versionOperationAllowed (DeleteEntity version))
   $ (\_ ->
     transactionController (runT (deleteVersionT version))
      (const
        (do setPageMessage "Version deleted"
            redirectController (listRoute (failed::Package)))))

--- Transaction to delete a given Version entity together with its relations
--- to dependencies, exported modules, and categaries.
deleteVersionT :: Version -> DBAction ()
deleteVersionT version =
  do oldDependingPackages <- getDependingVersionPackages version
     removeDepending oldDependingPackages version
     oldExportingCurryModules <- getExportingVersionCurryModules version
     removeExporting oldExportingCurryModules version
     versioningPackage <- getVersioningPackage version
     cats <- getPackageVersionCategories versioningPackage version
     mapM_ (\cat -> removeCategorizes [version] cat) cats
     deleteVersion version

--- Lists all private versions of packages.
allUnpublishedController :: Controller
allUnpublishedController =
  checkAuthorization (packageOperationAllowed ListEntities)
   $ (\sinfo ->
     do versions <- getUnpublishedVersions
        pkgversions <- mapM (\v -> runJustT (getVersioningPackage v) >>= \p -> return (p,v)) versions
        return (allVersionsView sinfo "All unpublished package versions" pkgversions))

--- Lists all Version entities with buttons to show, delete,
--- or edit an entity.
listVersionController :: Controller
listVersionController =
  checkAuthorization (versionOperationAllowed ListEntities)
   $ (\sinfo ->
     do versions <- runQ queryAllVersions
        pkgs <- runQ (mapM getVersioningPackage versions)
        return (listVersionView sinfo (zip pkgs versions)))

--- Shows a Version entity in the standard Spicey-generated layout.
showStandardVersionController :: Version -> Controller
showStandardVersionController version =
  checkAuthorization (versionOperationAllowed (ShowEntity version))
   $ (\sinfo ->
     do uploadUser <- runJustT (getUploadUser version)
        versioningPackage <- runJustT (getVersioningPackage version)
        dependingPackages <- runJustT (getDependingVersionPackages version)
        exportingCurryModules <- runJustT
                                  (getExportingVersionCurryModules version)
        return
         (showStandardVersionView sinfo version uploadUser versioningPackage
           dependingPackages
           exportingCurryModules))

--- Shows a Version entity.
showVersionController :: Version -> Controller
showVersionController version =
  checkAuthorization (versionOperationAllowed (ShowEntity version))
   $ (\sinfo ->
     do uploadUser <- runJustT (getUploadUser version)
        package <- runJustT (getVersioningPackage version)
        allversions <- getPackageVersions package
        dependingPackages <- runJustT (getDependingVersionPackages version)
        exportingCurryModules <- runJustT (getExportingVersionCurryModules version)
        cats <- runQ $ getPackageVersionCategories package version
        maintainers <- getPackageMaintainers package
        currentUser <- case userLoginOfSession sinfo of 
          Nothing -> return Nothing
          Just (loginName, _) -> getUserByName loginName
        watchesPackage <- case currentUser of 
          Nothing -> return False
          Just user -> checkUserWatches user package
        return
         (showVersionView sinfo version package uploadUser maintainers cats
            allversions dependingPackages exportingCurryModules watchesPackage currentUser))

--- Associates given entities with the Version entity
--- with respect to the `Depending` relation.
addDepending :: [Package] -> Version -> DBAction ()
addDepending packages version =
  mapM_ (\t -> newDepending (versionKey version) (packageKey t)) packages

--- Associates given entities with the Version entity
--- with respect to the `Exporting` relation.
addExporting :: [CurryModule] -> Version -> DBAction ()
addExporting curryModules version =
  mapM_ (\t -> newExporting (versionKey version) (curryModuleKey t))
   curryModules

--- Removes association to the given entities with the Version entity
--- with respect to the `Depending` relation.
removeDepending :: [Package] -> Version -> DBAction ()
removeDepending packages version =
  mapM_ (\t -> deleteDepending (versionKey version) (packageKey t)) packages

--- Removes association to the given entities with the Version entity
--- with respect to the `Exporting` relation.
removeExporting :: [CurryModule] -> Version -> DBAction ()
removeExporting curryModules version =
  mapM_ (\t -> deleteExporting (versionKey version) (curryModuleKey t))
   curryModules

--- Gets the associated User entity for a given Version entity.
getUploadUser :: Version -> DBAction User
getUploadUser vUser = getUser (versionUserUploadKey vUser)

--- Gets the associated Package entity for a given Version entity.
getVersioningPackage :: Version -> DBAction Package
getVersioningPackage vPackage =
  getPackage (versionPackageVersioningKey vPackage)
