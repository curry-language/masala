module Controller.User ( mainUserController, newUserForm, editUserForm, editUserFormAdmin, editPasswordForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Config.EntityRoutes
import Config.UserProcesses
import Config.Roles
import Model.Queries
import System.SessionInfo
import System.Authentication
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import System.PreludeHelpers
import View.EntitiesToHtml
import View.Package (allPackagesView)
import View.User
import Database.CDBI.Connection

type NewUser = (String
               ,String
               ,String
               ,String
               ,String
               ,String
               ,String
               ,Maybe ClockTime
               ,[Package]
               ,[Package])

--- Choose the controller for a User entity according to the URL parameter.
mainUserController :: Controller
mainUserController = do
  args <- getControllerParams
  case args of
    [] -> listUserController
    ["list"] -> listUserController
    ["new"] -> newUserController
    ["show",s] -> controllerOnKey s showUserController
    ["show",s,listName] -> controllerOnKey s (showPackagesController listName)
    ["profile"] -> controllerOnCurrentUser showUserController
    ["editprofile"] -> controllerOnCurrentUser editUserController
    ["watching"] -> controllerOnCurrentUser (showPackagesController "Watching")
    ["maintaining"] -> controllerOnCurrentUser
                         (showPackagesController "Maintaining")
    ["password"] -> controllerOnCurrentUser editPasswordController
    ["edit",s] -> controllerOnKey s editUserController
    ["edit",s,"Password"] -> controllerOnKey s editPasswordController
    ["delete",s] -> controllerOnKey s deleteUserController
    ["destroy",s] -> controllerOnKey s destroyUserController
    ["togglewatching",s,pkg] -> controllerOnKey s
                                  (toggleWatchingUserController pkg)
    _ -> displayUrlError

--- Shows a form to create a new User entity.
newUserController :: Controller
newUserController =
  checkAuthorization (userOperationAllowed NewEntity)
   $ (\sinfo ->
     do allPackages <- runQ queryAllPackages
        --allPackages <- runQ queryAllPackages
        --ctime <- getClockTime
        setParWuiStore newUserStore (sinfo,allPackages,allPackages)
         ("","","","","","","",Nothing,[],[])
        return [formElem newUserForm])

--- A WUI form to create a new User entity.
--- The default values for the fields are stored in 'newUserStore'.
newUserForm
  :: HtmlFormDef ((UserSessionInfo,[Package],[Package]),WuiStore NewUser)
newUserForm =
  pwui2FormDef "Controller.User.newUserForm" newUserStore
   (\(_,possibleMaintainerPackages,possibleWatchingPackages) ->
     wUser possibleMaintainerPackages possibleWatchingPackages)
   (\_ entity ->
     checkAuthorization (userOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createUserT entity))
         (\newentity ->
           do setPageMessage "New User created"
              nextInProcessOr (redirectController (showRoute newentity))
               Nothing)))
   (\(sinfo,_,_) ->
     let phantom = failed :: User
     in renderWUI sinfo "Create new User" "Create" (listRoute phantom) ())

--- The data stored for executing the "new entity" WUI form.
newUserStore
  :: SessionStore ((UserSessionInfo,[Package],[Package]),WuiStore NewUser)
newUserStore = sessionStore "newUserStore"

--- Transaction to persist a new User entity to the database.
createUserT :: NewUser -> DBAction User
createUserT
    (loginName,publicName,email,publicEmail,role,password,token,lastLogin
    ,maintainerpackages,watchingpackages) =
  do newentity <- newUser loginName publicName email publicEmail role password
                  token lastLogin
     addMaintainer maintainerpackages newentity
     addWatching watchingpackages newentity
     return newentity

--- Shows a form to edit the given User entity.
editUserController :: User -> Controller
editUserController userToEdit =
  checkAuthorization (userOperationAllowed (UpdateEntity userToEdit))
   $ (\sinfo ->
     do setParWuiStore editUserStore
         (sinfo,userToEdit)
         userToEdit
        if isAdminSession sinfo
          then return [formElem editUserFormAdmin]
          else return [formElem editUserForm])

--- A WUI form to edit a User entity.
--- The default values for the fields are stored in 'editUserStore'.
editUserForm :: HtmlFormDef ((UserSessionInfo,User), WuiStore User)
editUserForm =
  pwui2FormDef "Controller.User.editUserForm" editUserStore
   (\(_,user) -> wUserEditType user)
   (\_ userToEdit ->
     checkAuthorization (userOperationAllowed (UpdateEntity userToEdit))
      (\_ -> do
        pnameAvailable <- checkPublicNameAvailable (userPublicName userToEdit)
        if pnameAvailable
          then transactionController (runT (updateUser userToEdit)) $ const $ do
            setPageMessage "User profile updated"
            nextInProcessOr (redirectController (showRoute userToEdit))
                            Nothing
          else displayError "Public user name is already used!"))
   (\(sinfo,entity) ->
     renderWUIWithText sinfo "Edit User Profile" "Change"
                       [par [htxt explain]] (showRoute entity))
 where
  explain = unlines
    [ "You can update some data of your profile below."
    , "The public name and, if provided, the public email is shown"
    , "with your maintained packages so that other users might contact you."
    , "Note that your login name and email address cannnot be changed."
    , "If you want to change them, please contact the Masala administrator."
    ]

editUserFormAdmin :: HtmlFormDef ((UserSessionInfo,User), WuiStore User)
editUserFormAdmin =
  pwui2FormDef "Controller.User.editUserFormAdmin" editUserStore
   (\(_,user) -> wUserEditTypeAdmin user)
   (\_ userToEdit ->
     checkAuthorization (userOperationAllowed (UpdateEntity userToEdit))
      (\_ -> do
        pnameAvailable <- checkPublicNameAvailable (userPublicName userToEdit)
        if pnameAvailable
          then transactionController (runT (updateUser userToEdit)) $ const $ do
            setPageMessage "User profile updated"
            nextInProcessOr (redirectController (showRoute userToEdit))
                            Nothing
          else displayError "Public user name is already used!"))
   (\(sinfo,entity) ->
     renderWUI sinfo "Edit User Profile" "Change" (listRoute entity) ())

--- The data stored for executing the edit WUI form.
editUserStore
  :: SessionStore ((UserSessionInfo,User)
                  ,WuiStore User)
editUserStore = sessionStore "editUserStore"

{-        
--- Shows a form to edit the given User entity.
editUserController :: User -> Controller
editUserController userToEdit =
  checkAuthorization (userOperationAllowed (UpdateEntity userToEdit))
   $ (\sinfo ->
     do allPackages <- runQ queryAllPackages
        --allPackages <- runQ queryAllPackages
        maintainerPackages <- runJustT (getMaintainerUserPackages userToEdit)
        watchingPackages <- runJustT (getWatchingUserPackages userToEdit)
        setParWuiStore editUserStore
         (sinfo,userToEdit,allPackages,allPackages)
         (userToEdit,maintainerPackages,watchingPackages)
        return [formElem editUserForm])

--- A WUI form to edit a User entity.
--- The default values for the fields are stored in 'editUserStore'.
editUserForm
  :: HtmlFormDef ((UserSessionInfo,User,[Package],[Package])
                 ,WuiStore (User,[Package],[Package]))
editUserForm =
  pwui2FormDef "Controller.User.editUserForm" editUserStore
   (\(_,user,possibleMaintainerPackages,possibleWatchingPackages) ->
     wUserTypeEdit user possibleMaintainerPackages possibleWatchingPackages)
   (\_ entity@(userToEdit,_,_) ->
     checkAuthorization (userOperationAllowed (UpdateEntity userToEdit))
      (\_ ->
        transactionController (runT (updateUserT entity))
         (const
           (do setPageMessage "User updated"
               nextInProcessOr (redirectController (showRoute userToEdit))
                Nothing))))
   (\(sinfo,entity,_,_) ->
     renderWUI sinfo "Edit User" "Change" (listRoute entity) ())

--- The data stored for executing the edit WUI form.
editUserStore
  :: SessionStore ((UserSessionInfo,User,[Package],[Package])
                  ,WuiStore (User,[Package],[Package]))
editUserStore = sessionStore "editUserStore"
-}

--- Shows a form to edit the password of the given User entity.
editPasswordController :: User -> Controller
editPasswordController userToEdit =
  checkAuthorization (userOperationAllowed (UpdateEntity userToEdit))
   $ (\sinfo ->
     do setParWuiStore editPasswordStore
         (sinfo,userToEdit)
         (userToEdit,"","","")
        return [formElem editPasswordForm])

--- A WUI form to edit the password of a User entity.
--- The default values for the fields are stored in 'editUserStore'.
editPasswordForm
  :: HtmlFormDef ((UserSessionInfo,User),WuiStore (User,String,String,String))
editPasswordForm =
  pwui2FormDef "Controller.User.editPasswordForm" editPasswordStore
   (\(_, _) ->
     wPasswordEdit)
   (\_ (user,oldPasswd,newPasswd,_) ->
     checkAuthorization (userOperationAllowed (UpdateEntity user))
      (\_ -> do
              let loginName = userLoginName user
              cryptedOldPasswd <- getUserHash loginName oldPasswd
              cryptedNewPasswd <- getUserHash loginName newPasswd
              let correctOldPassword = cryptedOldPasswd == userPassword user
              if correctOldPassword 
                then do
                  let user' = setUserPassword user cryptedNewPasswd
                  transactionController (runT (updateUser user'))
                    (const
                      (do setPageMessage "Password updated"
                          nextInProcessOr (redirectController (showRoute user))
                            Nothing))
                else displayError "Old password is not correct."
      )
    )
   (\(sinfo,user) ->
     renderWUI sinfo "Edit Password" "Change" (showRoute user) ())

--- The data stored for executing the edit WUI form.
-- old password, new password, new password (again)
editPasswordStore
  :: SessionStore ((UserSessionInfo,User),WuiStore (User,String,String,String))
editPasswordStore = sessionStore "editPasswordStore"

--- Transaction to persist modifications of a given User entity
--- to the database.
updateUserT :: (User,[Package],[Package]) -> DBAction ()
updateUserT (user,packagesMaintainer,packagesWatching) =
  do updateUser user
     oldMaintainerPackages <- getMaintainerUserPackages user
     removeMaintainer oldMaintainerPackages user
     oldWatchingPackages <- getWatchingUserPackages user
     removeWatching oldWatchingPackages user
     addMaintainer packagesMaintainer user
     addWatching packagesWatching user

--- Deletes a given User entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteUserController :: User -> Controller
deleteUserController user =
  checkAuthorization (userOperationAllowed (DeleteEntity user))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat ["Really delete user \"", userToShortView user, "\" and ",
               " and all its maintainer roles?"]))

--- Deletes a given User entity
--- and proceeds with the list controller.
destroyUserController :: User -> Controller
destroyUserController user =
  checkAuthorization (userOperationAllowed (DeleteEntity user))
   $ (\_ ->
     transactionController (runT (deleteUserT user))
      (const
        (do setPageMessage "User deleted"
            redirectController (listRoute user))))

--- Transaction to delete a given User entity.
deleteUserT :: User -> DBAction ()
deleteUserT user = do
  oldMaintainerPackages <- getMaintainerUserPackages user
  removeMaintainer oldMaintainerPackages user
  oldWatchingPackages <- getWatchingUserPackages user
  removeWatching oldWatchingPackages user
  tokens <- queryCondValidationToken
              (\t -> validationTokenUserValidatingKey t == userKey user)
  mapM deleteValidationToken tokens
  deleteUser user

--- Lists all User entities with buttons to show, delete,
--- or edit an entity.
listUserController :: Controller
listUserController =
  checkAuthorization (userOperationAllowed ListEntities)
   $ (\sinfo ->
     do users <- runQ queryAllUsers
        return (listUserView sinfo users))

--- Shows a User entity.
showUserController :: User -> Controller
showUserController user =
  checkAuthorization (userOperationAllowed (ShowEntity user))
   $ (\sinfo ->
      if isAdminSession sinfo
        then return
          (showUserViewAdmin sinfo user)
        else 
          if loggedInAsUserSession user sinfo
            then return
              (showUserView sinfo user)
            else return
              (showUserViewLess sinfo user)
        )

controllerOnCurrentUser :: (User -> Controller) -> Controller
controllerOnCurrentUser usercontroller = do
  loginInfo <- getSessionLogin
  case loginInfo of 
    Nothing -> displayUrlError
    Just (loginName, _) -> do
      maybeuser <- getUserByName loginName
      case maybeuser of 
        Nothing -> displayError ("User " ++ loginName ++ " does not exist")
        Just user -> usercontroller user

showProfileController :: Controller
showProfileController = do
  loginInfo <- getSessionLogin
  case loginInfo of 
    Nothing -> displayUrlError
    Just (loginName, _) -> do
      maybeuser <- getUserByName loginName
      case maybeuser of 
        Nothing -> displayError ("User " ++ loginName ++ " does not exist")
        Just user -> showUserController user

showPackagesController :: String -> User -> Controller
showPackagesController listName user = do
  checkAuthorization (userOperationAllowed (ShowEntity user))
    $ (\sinfo -> do
        packages <- case listName of
          "Maintaining" -> runQ (getMaintainerUserPackages user)
          "Watching" -> runQ (getWatchingUserPackages user)
          _ -> return []
        case listName of 
          "Maintaining" -> return $ allPackagesView sinfo "Maintained Packages" packages
          "Watching" -> return $ allPackagesView sinfo "Watched Packages" packages
          _ -> displayUrlError
      )

toggleWatchingUserController :: String -> User -> Controller
toggleWatchingUserController pkg user = do 
  checkAuthorization (userOperationAllowed (UpdateEntity user))
    $ (\_ -> do
      case readPackageKey pkg of 
        Nothing -> displayError "Package does not exist!"
        Just pkgKey -> do
          packageResult <- runT $ getPackage pkgKey
          case packageResult of 
            Left err -> displayError $ "Some error occured while getting package (" ++
                                       show err ++ ")"
            Right package -> do
              watchesPackage <- checkUserWatches user package
              case watchesPackage of 
                False -> do 
                  runT $ newWatching (userKey user) pkgKey
                  setPageMessage "Now watching package"
                True -> do
                  runT $ deleteWatching (userKey user) pkgKey
                  setPageMessage "Not watching package anymore"
              redirectController (showRoute package)
    )
{-
showUserController :: User -> Controller
showUserController user =
  checkAuthorization (userOperationAllowed (ShowEntity user))
   $ (\sinfo ->
     do maintainerPackages <- runJustT (getMaintainerUserPackages user)
        watchingPackages <- runJustT (getWatchingUserPackages user)
        return
         (showUserView sinfo user maintainerPackages watchingPackages))
-}

--- Associates given entities with the User entity
--- with respect to the `Maintainer` relation.
addMaintainer :: [Package] -> User -> DBAction ()
addMaintainer packages user =
  mapM_ (\t -> newMaintainer (userKey user) (packageKey t)) packages

--- Associates given entities with the User entity
--- with respect to the `Watching` relation.
addWatching :: [Package] -> User -> DBAction ()
addWatching packages user =
  mapM_ (\t -> newWatching (userKey user) (packageKey t)) packages

--- Removes association to the given entities with the User entity
--- with respect to the `Maintainer` relation.
removeMaintainer :: [Package] -> User -> DBAction ()
removeMaintainer packages user =
  mapM_ (\t -> deleteMaintainer (userKey user) (packageKey t)) packages

--- Removes association to the given entities with the User entity
--- with respect to the `Watching` relation.
removeWatching :: [Package] -> User -> DBAction ()
removeWatching packages user =
  mapM_ (\t -> deleteWatching (userKey user) (packageKey t)) packages
