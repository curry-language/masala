module Controller.User ( mainUserController, newUserForm, editUserForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Config.EntityRoutes
import Config.UserProcesses
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import System.PreludeHelpers
import View.EntitiesToHtml
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
mainUserController =
  do args <- getControllerParams
     case args of
       [] -> listUserController
       ["list"] -> listUserController
       ["new"] -> newUserController
       ["show",s] -> controllerOnKey s showUserController
       ["edit",s] -> controllerOnKey s editUserController
       ["delete",s] -> controllerOnKey s deleteUserController
       ["destroy",s] -> controllerOnKey s destroyUserController
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
      (concat ["Really delete entity \"",userToShortView user,"\"?"]))

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
deleteUserT user =
  do oldMaintainerPackages <- getMaintainerUserPackages user
     removeMaintainer oldMaintainerPackages user
     oldWatchingPackages <- getWatchingUserPackages user
     removeWatching oldWatchingPackages user
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
     do maintainerPackages <- runJustT (getMaintainerUserPackages user)
        watchingPackages <- runJustT (getWatchingUserPackages user)
        return
         (showUserView sinfo user maintainerPackages watchingPackages))

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
