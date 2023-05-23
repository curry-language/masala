module Controller.Version
  ( mainVersionController, newVersionForm, editVersionForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Masala2
import Config.EntityRoutes
import Config.UserProcesses
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
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
mainVersionController =
  do args <- getControllerParams
     case args of
       [] -> listVersionController
       ["list"] -> listVersionController
       ["new"] -> newVersionController
       ["show",s] -> controllerOnKey s showVersionController
       ["edit",s] -> controllerOnKey s editVersionController
       ["delete",s] -> controllerOnKey s deleteVersionController
       ["destroy",s] -> controllerOnKey s destroyVersionController
       _ -> displayUrlError

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
         (nextInProcessOr (redirectController "?Version/list") Nothing)))
   (\(sinfo,_,_,_,_) ->
     renderWUI sinfo "Create new Version" "Create" "?Version/list" ())

--- The data stored for executing the "new entity" WUI form.
newVersionStore
  :: SessionStore ((UserSessionInfo,[User],[Package],[Package],[CurryModule])
                  ,WuiStore NewVersion)
newVersionStore = sessionStore "newVersionStore"

--- Transaction to persist a new Version entity to the database.
createVersionT :: NewVersion -> DBAction ()
createVersionT
    (version,published,tested,description,jobStatus,downloads,uploadDate
    ,deprecated,user,package,dependingpackages,curryModules) =
  newVersionWithPackageVersioningKeyWithUserUploadKey version published tested
   description
   jobStatus
   downloads
   uploadDate
   deprecated
   (packageKey package)
   (userKey user)
   >>= (\newentity ->
     addDepending dependingpackages newentity
      >> (addExporting curryModules newentity >> return ()))

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
        dependingPackages <- runJustT (getVersionPackages versionToEdit)
        exportingCurryModules <- runJustT
                                  (getVersionCurryModules versionToEdit)
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
         (nextInProcessOr (redirectController "?Version/list") Nothing)))
   (\(sinfo,_,_,_,_,_,_,_) ->
     renderWUI sinfo "Edit Version" "Change" "?Version/list" ())

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
  updateVersion version
   >> ((getVersionPackages version
         >>= (\oldDependingPackages ->
           removeDepending oldDependingPackages version))
        >> ((getVersionCurryModules version
              >>= (\oldExportingCurryModules ->
                removeExporting oldExportingCurryModules version))
             >> (addDepending packagesDepending version
                  >> addExporting curryModulesExporting version)))

--- Deletes a given Version entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteVersionController :: Version -> Controller
deleteVersionController version =
  checkAuthorization (versionOperationAllowed (DeleteEntity version))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat ["Really delete entity \"",versionToShortView version,"\"?"]))

--- Deletes a given Version entity
--- and proceeds with the list controller.
destroyVersionController :: Version -> Controller
destroyVersionController version =
  checkAuthorization (versionOperationAllowed (DeleteEntity version))
   $ (\_ ->
     transactionController (runT (deleteVersionT version))
      (redirectController "?Version/list"))

--- Transaction to delete a given Version entity.
deleteVersionT :: Version -> DBAction ()
deleteVersionT version =
  (getVersionPackages version
    >>= (\oldDependingPackages ->
      removeDepending oldDependingPackages version))
   >> ((getVersionCurryModules version
         >>= (\oldExportingCurryModules ->
           removeExporting oldExportingCurryModules version))
        >> deleteVersion version)

--- Lists all Version entities with buttons to show, delete,
--- or edit an entity.
listVersionController :: Controller
listVersionController =
  checkAuthorization (versionOperationAllowed ListEntities)
   $ (\sinfo ->
     do versions <- runQ queryAllVersions
        pkgs <- runQ (mapM getVersioningPackage versions)
        return (listVersionView sinfo (zip pkgs versions)))

--- Shows a Version entity.
showVersionController :: Version -> Controller
showVersionController version =
  checkAuthorization (versionOperationAllowed (ShowEntity version))
   $ (\sinfo ->
     do uploadUser <- runJustT (getUploadUser version)
        versioningPackage <- runJustT (getVersioningPackage version)
        dependingPackages <- runJustT (getVersionPackages version)
        exportingCurryModules <- runJustT (getVersionCurryModules version)
        return
         (showVersionView sinfo version uploadUser versioningPackage
           dependingPackages
           exportingCurryModules))

--- Associates given entities with the Version entity.
addDepending :: [Package] -> Version -> DBAction ()
addDepending packages version =
  mapM_ (\t -> newDepending (versionKey version) (packageKey t)) packages

--- Associates given entities with the Version entity.
addExporting :: [CurryModule] -> Version -> DBAction ()
addExporting curryModules version =
  mapM_ (\t -> newExporting (versionKey version) (curryModuleKey t))
   curryModules

--- Removes association to the given entities with the Version entity.
removeDepending :: [Package] -> Version -> DBAction ()
removeDepending packages version =
  mapM_ (\t -> deleteDepending (versionKey version) (packageKey t)) packages

--- Removes association to the given entities with the Version entity.
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
