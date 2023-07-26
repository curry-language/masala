module Controller.Category
  ( mainCategoryController, newCategoryForm
  , editCategoryDescForm, addCategorizes, removeCategorizes )
 where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.UserProcesses
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import View.EntitiesToHtml
import View.Category
import Database.CDBI.Connection

type NewCategory = (String,String)

--- Choose the controller for a Category entity according to the URL parameter.
mainCategoryController :: Controller
mainCategoryController =
  do args <- getControllerParams
     case args of
       [] -> allCategoriesController
       ["list"] -> allCategoriesController
       ["listpkgs"] -> allCategoriesPackagesController
       ["new"] -> newCategoryController
       ["show",s] -> controllerOnKey s showCategoryController
       ["edit",s] -> controllerOnKey s editCategoryDescController
       --["editversion",s] -> controllerOnKey s editCategoryController
       ["delete",s] -> controllerOnKey s deleteCategoryController
       ["destroy",s] -> controllerOnKey s destroyCategoryController
       _ -> displayUrlError

--- Shows a form to create a new Category entity.
newCategoryController :: Controller
newCategoryController =
  checkAuthorization (categoryOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore newCategoryStore sinfo ("","")
        return [formElem newCategoryForm])

--- A WUI form to create a new Category entity.
--- The default values for the fields are stored in 'newCategoryStore'.
newCategoryForm :: HtmlFormDef (UserSessionInfo,WuiStore NewCategory)
newCategoryForm =
  pwui2FormDef "Controller.Category.newCategoryForm" newCategoryStore
   (\_ -> wCategory)
   (\_ entity ->
     checkAuthorization (categoryOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createCategoryT entity))
         (\newentity ->
           do setPageMessage "New Category created"
              nextInProcessOr (redirectController (showRoute newentity))
               Nothing)))
   (\sinfo ->
     let phantom = failed :: Category
     in renderWUI sinfo "Create new Category" "Create" (listRoute phantom) ())

--- The data stored for executing the "new entity" WUI form.
newCategoryStore :: SessionStore (UserSessionInfo,WuiStore NewCategory)
newCategoryStore = sessionStore "newCategoryStore"

--- Transaction to persist a new Category entity to the database.
createCategoryT :: NewCategory -> DBAction Category
createCategoryT (name,description) = newCategory name description

{-
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
         (const
           (do setPageMessage "Category updated"
               nextInProcessOr (redirectController (showRoute categoryToEdit))
                Nothing))))
   (\(sinfo,entity,_) ->
     renderWUI sinfo "Edit Category" "Change" (listRoute entity) ())

--- The data stored for executing the edit WUI form.
editCategoryStore
  :: SessionStore ((UserSessionInfo,Category,[Version])
                  ,WuiStore (Category,[Version]))
editCategoryStore = sessionStore "editCategoryStore"

--- Transaction to persist modifications of a given Category entity
--- to the database.
updateCategoryT :: (Category,[Version]) -> DBAction ()
updateCategoryT (category,versionsCategorizes) =
  do updateCategory category
     oldCategorizesVersions <- getCategoryVersions category
     removeCategorizes oldCategorizesVersions category
     addCategorizes versionsCategorizes category
-}
------------------------------------------------------------------------------
--- Shows a form to edit the description of the given Category entity.
editCategoryDescController :: Category -> Controller
editCategoryDescController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $ (\sinfo ->
     do setParWuiStore editCategoryDescStore (sinfo,categoryToEdit)
                       categoryToEdit
        return [formElem editCategoryDescForm])

--- A WUI form to edit a Category entity.
--- The default values for the fields are stored in 'editCategoryDescStore'.
editCategoryDescForm
  :: HtmlFormDef ((UserSessionInfo,Category), WuiStore Category)
editCategoryDescForm =
  pwui2FormDef "Controller.Category.editCategoryDescForm" editCategoryDescStore
   (\(_,category) -> wCategoryDescType category)
   (\_ newcat ->
     checkAuthorization
      (categoryOperationAllowed (UpdateEntity newcat))
      (\_ ->
        transactionController (runT (updateCategoryDescT newcat))
         (const
           (do setPageMessage "Category updated"
               nextInProcessOr (redirectController (showRoute newcat))
                Nothing))))
   (\(sinfo,cat) ->
     renderWUI sinfo "Edit Category" "Change" (showRoute cat) ())

--- The data stored for executing the edit WUI form.
editCategoryDescStore
  :: SessionStore ((UserSessionInfo,Category), WuiStore Category)
editCategoryDescStore = sessionStore "editCategoryDescStore"

--- Transaction to persist modifications of a given Category entity
--- to the database.
updateCategoryDescT :: Category -> DBAction ()
updateCategoryDescT category = updateCategory category

------------------------------------------------------------------------------
--- Deletes a given Category entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteCategoryController :: Category -> Controller
deleteCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat ["Really delete category \"",categoryToShortView category,"\"?"]))

--- Deletes a given Category entity
--- and proceeds with the list controller.
destroyCategoryController :: Category -> Controller
destroyCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category))
   $ (\_ ->
     transactionController (runT (deleteCategoryT category))
      (const
        (do setPageMessage "Category deleted"
            redirectController (listRoute category))))

--- Transaction to delete a given Category entity.
deleteCategoryT :: Category -> DBAction ()
deleteCategoryT category =
  do oldCategorizesVersions <- getCategorizesCategoryVersions category
     removeCategorizes oldCategorizesVersions category
     deleteCategory category

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listCategoryController :: Controller
listCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities)
   $ (\sinfo ->
     do categorys <- runQ queryAllCategorys
        return (listCategoryView sinfo categorys))

--- Lists all Category entities.
allCategoriesController :: Controller
allCategoriesController =
  checkAuthorization (categoryOperationAllowed ListEntities)
   $ (\sinfo ->
     do categorys <- runQ queryAllCategorys
        return (allCategoriesView sinfo categorys))

--- Lists all Category entities together with their packages.
allCategoriesPackagesController :: Controller
allCategoriesPackagesController =
  checkAuthorization (categoryOperationAllowed ListEntities)
   $ (\sinfo ->
     do categories <- runQ queryAllCategorys
        catpkgs <- mapM (\c -> getPackagesOfCategory c >>= \ps -> return (c,ps)) categories
        return (allCategoriesPackagesView sinfo catpkgs))

--- Shows a Category entity.
showCategoryController :: Category -> Controller
showCategoryController category =
  checkAuthorization (categoryOperationAllowed (ShowEntity category)) $
   \sinfo -> do
     --categorizesVersions <- runJustT (getCategorizesCategoryVersions category)
     packages <- getPackagesOfCategory category
     return (showCategoryView sinfo category packages)

--- Associates given entities with the Category entity
--- with respect to the `Categorizes` relation.
addCategorizes :: [Version] -> Category -> DBAction ()
addCategorizes versions category =
  mapM_ (\t -> newCategorizes (categoryKey category) (versionKey t)) versions

--- Removes association to the given entities with the Category entity
--- with respect to the `Categorizes` relation.
removeCategorizes :: [Version] -> Category -> DBAction ()
removeCategorizes versions category =
  mapM_ (\t -> deleteCategorizes (categoryKey category) (versionKey t))
   versions
