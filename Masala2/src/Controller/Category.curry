module Controller.Category
  ( mainCategoryController, newCategoryForm, editCategoryForm ) where

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
import View.EntitiesToHtml
import View.Category
import Database.CDBI.Connection

type NewCategory = (String,String,[Version])

--- Choose the controller for a Category entity according to the URL parameter.
mainCategoryController :: Controller
mainCategoryController =
  do args <- getControllerParams
     case args of
       [] -> listCategoryController
       ["list"] -> listCategoryController
       ["new"] -> newCategoryController
       ["show",s] -> controllerOnKey s showCategoryController
       ["edit",s] -> controllerOnKey s editCategoryController
       ["delete",s] -> controllerOnKey s deleteCategoryController
       ["destroy",s] -> controllerOnKey s destroyCategoryController
       _ -> displayUrlError

--- Shows a form to create a new Category entity.
newCategoryController :: Controller
newCategoryController =
  checkAuthorization (categoryOperationAllowed NewEntity)
   $ (\sinfo ->
     do allVersions <- runQ queryAllVersions
        setParWuiStore newCategoryStore (sinfo,allVersions) ("","",[])
        return [formElem newCategoryForm])

--- A WUI form to create a new Category entity.
--- The default values for the fields are stored in 'newCategoryStore'.
newCategoryForm
  :: HtmlFormDef ((UserSessionInfo,[Version]),WuiStore NewCategory)
newCategoryForm =
  pwui2FormDef "Controller.Category.newCategoryForm" newCategoryStore
   (\(_,possibleVersions) -> wCategory possibleVersions)
   (\_ entity ->
     checkAuthorization (categoryOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createCategoryT entity))
         (nextInProcessOr (redirectController "?Category/list") Nothing)))
   (\(sinfo,_) ->
     renderWUI sinfo "Create new Category" "Create" "?Category/list" ())

--- The data stored for executing the "new entity" WUI form.
newCategoryStore
  :: SessionStore ((UserSessionInfo,[Version]),WuiStore NewCategory)
newCategoryStore = sessionStore "newCategoryStore"

--- Transaction to persist a new Category entity to the database.
createCategoryT :: NewCategory -> DBAction ()
createCategoryT (name,description,versions) =
  newCategory name description
   >>= (\newentity -> addCategorizes versions newentity >> return ())

--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $ (\sinfo ->
     do allVersions <- runQ queryAllVersions
        categorizesVersions <- runJustT (getCategoryVersions categoryToEdit)
        setParWuiStore editCategoryStore (sinfo,categoryToEdit,allVersions)
         (categoryToEdit,categorizesVersions)
        return [formElem editCategoryForm])

--- A WUI form to edit a Category entity.
--- The default values for the fields are stored in 'editCategoryStore'.
editCategoryForm
  :: HtmlFormDef ((UserSessionInfo,Category,[Version])
                 ,WuiStore (Category,[Version]))
editCategoryForm =
  pwui2FormDef "Controller.Category.editCategoryForm" editCategoryStore
   (\(_,category,possibleVersions) -> wCategoryType category possibleVersions)
   (\_ entity@(categoryToEdit,_) ->
     checkAuthorization
      (categoryOperationAllowed (UpdateEntity categoryToEdit))
      (\_ ->
        transactionController (runT (updateCategoryT entity))
         (nextInProcessOr (redirectController "?Category/list") Nothing)))
   (\(sinfo,_,_) ->
     renderWUI sinfo "Edit Category" "Change" "?Category/list" ())

--- The data stored for executing the edit WUI form.
editCategoryStore
  :: SessionStore ((UserSessionInfo,Category,[Version])
                  ,WuiStore (Category,[Version]))
editCategoryStore = sessionStore "editCategoryStore"

--- Transaction to persist modifications of a given Category entity
--- to the database.
updateCategoryT :: (Category,[Version]) -> DBAction ()
updateCategoryT (category,versionsCategorizes) =
  updateCategory category
   >> ((getCategoryVersions category
         >>= (\oldCategorizesVersions ->
           removeCategorizes oldCategorizesVersions category))
        >> addCategorizes versionsCategorizes category)

--- Deletes a given Category entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteCategoryController :: Category -> Controller
deleteCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat ["Really delete entity \"",categoryToShortView category,"\"?"]))

--- Deletes a given Category entity
--- and proceeds with the list controller.
destroyCategoryController :: Category -> Controller
destroyCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category))
   $ (\_ ->
     transactionController (runT (deleteCategoryT category))
      (redirectController "?Category/list"))

--- Transaction to delete a given Category entity.
deleteCategoryT :: Category -> DBAction ()
deleteCategoryT category =
  (getCategoryVersions category
    >>= (\oldCategorizesVersions ->
      removeCategorizes oldCategorizesVersions category))
   >> deleteCategory category

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listCategoryController :: Controller
listCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities)
   $ (\sinfo ->
     do categorys <- runQ queryAllCategorys
        return (listCategoryView sinfo categorys))

--- Shows a Category entity.
showCategoryController :: Category -> Controller
showCategoryController category =
  checkAuthorization (categoryOperationAllowed (ShowEntity category))
   $ (\sinfo ->
     do categorizesVersions <- runJustT (getCategoryVersions category)
        return (showCategoryView sinfo category categorizesVersions))

--- Associates given entities with the Category entity.
addCategorizes :: [Version] -> Category -> DBAction ()
addCategorizes versions category =
  mapM_ (\t -> newCategorizes (categoryKey category) (versionKey t)) versions

--- Removes association to the given entities with the Category entity.
removeCategorizes :: [Version] -> Category -> DBAction ()
removeCategorizes versions category =
  mapM_ (\t -> deleteCategorizes (categoryKey category) (versionKey t))
   versions