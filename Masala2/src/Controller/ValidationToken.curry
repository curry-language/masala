module Controller.ValidationToken
  ( mainValidationTokenController, newValidationTokenForm
  , editValidationTokenForm ) where

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
import View.ValidationToken
import Database.CDBI.Connection

type NewValidationToken = (String,ClockTime,User)

--- Choose the controller for a ValidationToken entity according to the URL parameter.
mainValidationTokenController :: Controller
mainValidationTokenController =
  do args <- getControllerParams
     case args of
       [] -> listValidationTokenController
       ["list"] -> listValidationTokenController
       ["new"] -> newValidationTokenController
       ["show",s] -> controllerOnKey s showValidationTokenController
       ["edit",s] -> controllerOnKey s editValidationTokenController
       ["delete",s] -> controllerOnKey s deleteValidationTokenController
       ["destroy",s] -> controllerOnKey s destroyValidationTokenController
       _ -> displayUrlError

--- Shows a form to create a new ValidationToken entity.
newValidationTokenController :: Controller
newValidationTokenController =
  checkAuthorization (validationTokenOperationAllowed NewEntity)
   $ (\sinfo ->
     do allUsers <- runQ queryAllUsers
        ctime <- getClockTime
        if null allUsers
           then return
                 [h2
                   [htxt
                     "Some related entities are required but not yet undefined"]]
           else do setParWuiStore newValidationTokenStore (sinfo,allUsers)
                    ("",ctime,head allUsers)
                   return [formElem newValidationTokenForm])

--- A WUI form to create a new ValidationToken entity.
--- The default values for the fields are stored in 'newValidationTokenStore'.
newValidationTokenForm
  :: HtmlFormDef ((UserSessionInfo,[User]),WuiStore NewValidationToken)
newValidationTokenForm =
  pwui2FormDef "Controller.ValidationToken.newValidationTokenForm"
   newValidationTokenStore
   (\(_,possibleUsers) -> wValidationToken possibleUsers)
   (\_ entity ->
     checkAuthorization (validationTokenOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createValidationTokenT entity))
         (\newentity ->
           do setPageMessage "New ValidationToken created"
              nextInProcessOr (redirectController (showRoute newentity))
               Nothing)))
   (\(sinfo,_) ->
     let phantom = failed :: ValidationToken
     in renderWUI sinfo "Create new ValidationToken" "Create"
         (listRoute phantom)
         ())

--- The data stored for executing the "new entity" WUI form.
newValidationTokenStore
  :: SessionStore ((UserSessionInfo,[User]),WuiStore NewValidationToken)
newValidationTokenStore = sessionStore "newValidationTokenStore"

--- Transaction to persist a new ValidationToken entity to the database.
createValidationTokenT :: NewValidationToken -> DBAction ValidationToken
createValidationTokenT (token,validSince,user) =
  do newentity <- newValidationTokenWithUserValidatingKey token validSince
                   (userKey user)
     return newentity

--- Shows a form to edit the given ValidationToken entity.
editValidationTokenController :: ValidationToken -> Controller
editValidationTokenController validationTokenToEdit =
  checkAuthorization
   (validationTokenOperationAllowed (UpdateEntity validationTokenToEdit))
   $ (\sinfo ->
     do allUsers <- runQ queryAllUsers
        validatingUser <- runJustT (getValidatingUser validationTokenToEdit)
        setParWuiStore editValidationTokenStore
         (sinfo,validationTokenToEdit,validatingUser,allUsers)
         validationTokenToEdit
        return [formElem editValidationTokenForm])

--- A WUI form to edit a ValidationToken entity.
--- The default values for the fields are stored in 'editValidationTokenStore'.
editValidationTokenForm
  :: HtmlFormDef ((UserSessionInfo,ValidationToken,User,[User])
                 ,WuiStore ValidationToken)
editValidationTokenForm =
  pwui2FormDef "Controller.ValidationToken.editValidationTokenForm"
   editValidationTokenStore
   (\(_,validationToken,relatedUser,possibleUsers) ->
     wValidationTokenType validationToken relatedUser possibleUsers)
   (\_ entity@validationTokenToEdit ->
     checkAuthorization
      (validationTokenOperationAllowed (UpdateEntity validationTokenToEdit))
      (\_ ->
        transactionController (runT (updateValidationTokenT entity))
         (const
           (do setPageMessage "ValidationToken updated"
               nextInProcessOr
                (redirectController (showRoute validationTokenToEdit))
                Nothing))))
   (\(sinfo,entity,_,_) ->
     renderWUI sinfo "Edit ValidationToken" "Change" (listRoute entity) ())

--- The data stored for executing the edit WUI form.
editValidationTokenStore
  :: SessionStore ((UserSessionInfo,ValidationToken,User,[User])
                  ,WuiStore ValidationToken)
editValidationTokenStore = sessionStore "editValidationTokenStore"

--- Transaction to persist modifications of a given ValidationToken entity
--- to the database.
updateValidationTokenT :: ValidationToken -> DBAction ()
updateValidationTokenT validationToken = updateValidationToken validationToken

--- Deletes a given ValidationToken entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteValidationTokenController :: ValidationToken -> Controller
deleteValidationTokenController validationToken =
  checkAuthorization
   (validationTokenOperationAllowed (DeleteEntity validationToken))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat
        ["Really delete entity \""
        ,validationTokenToShortView validationToken
        ,"\"?"]))

--- Deletes a given ValidationToken entity
--- and proceeds with the list controller.
destroyValidationTokenController :: ValidationToken -> Controller
destroyValidationTokenController validationToken =
  checkAuthorization
   (validationTokenOperationAllowed (DeleteEntity validationToken))
   $ (\_ ->
     transactionController (runT (deleteValidationTokenT validationToken))
      (const
        (do setPageMessage "ValidationToken deleted"
            redirectController (listRoute validationToken))))

--- Transaction to delete a given ValidationToken entity.
deleteValidationTokenT :: ValidationToken -> DBAction ()
deleteValidationTokenT validationToken = deleteValidationToken validationToken

--- Lists all ValidationToken entities with buttons to show, delete,
--- or edit an entity.
listValidationTokenController :: Controller
listValidationTokenController =
  checkAuthorization (validationTokenOperationAllowed ListEntities)
   $ (\sinfo ->
     do validationTokens <- runQ queryAllValidationTokens
        return (listValidationTokenView sinfo validationTokens))

--- Shows a ValidationToken entity.
showValidationTokenController :: ValidationToken -> Controller
showValidationTokenController validationToken =
  checkAuthorization
   (validationTokenOperationAllowed (ShowEntity validationToken))
   $ (\sinfo ->
     do validatingUser <- runJustT (getValidatingUser validationToken)
        return (showValidationTokenView sinfo validationToken validatingUser))

--- Gets the associated User entity for a given ValidationToken entity.
getValidatingUser :: ValidationToken -> DBAction User
getValidatingUser vUser = getUser (validationTokenUserValidatingKey vUser)
