module View.Category
  ( wCategory, tuple2Category, category2Tuple, wCategoryType, showCategoryView
  , listCategoryView ) where

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Masala2
import Config.EntityRoutes
import System.SessionInfo
import System.Spicey
import View.EntitiesToHtml

--- The WUI specification for the entity type Category.
--- It also includes fields for associated entities.
wCategory :: [Version] -> WuiSpec (String,String,[Version])
wCategory versionList =
  withRendering
   (wTriple wRequiredString wRequiredString
     (wMultiCheckSelect (\version -> [htxt (versionToShortView version)])
       versionList))
   (renderLabels categoryLabelList)

--- Transformation from data of a WUI form to entity type Category.
tuple2Category
  :: Category -> (String,String,[Version]) -> (Category,[Version])
tuple2Category categoryToUpdate (name,description,versions) =
  (setCategoryName (setCategoryDescription categoryToUpdate description) name
  ,versions)

--- Transformation from entity type Category to a tuple
--- which can be used in WUI specifications.
category2Tuple :: (Category,[Version]) -> (String,String,[Version])
category2Tuple (category,versions) =
  (categoryName category,categoryDescription category,versions)

--- WUI Type for editing or creating Category entities.
--- Includes fields for associated entities.
wCategoryType :: Category -> [Version] -> WuiSpec (Category,[Version])
wCategoryType category versionList =
  transformWSpec (tuple2Category category,category2Tuple)
   (wCategory versionList)

--- Supplies a view to show the details of a Category.
showCategoryView :: UserSessionInfo -> Category -> [Version] -> [BaseHtml]
showCategoryView _ category versions =
  categoryToDetailsView category versions
   ++ [hrefPrimSmButton "?Category/list" [htxt "back to Category list"]]

--- Compares two Category entities. This order is used in the list view.
leqCategory :: Category -> Category -> Bool
leqCategory x1 x2 =
  (categoryName x1,categoryDescription x1)
   <= (categoryName x2,categoryDescription x2)

--- Supplies a list view for a given list of Category entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of Category entities.
listCategoryView :: UserSessionInfo -> [Category] -> [BaseHtml]
listCategoryView sinfo categorys =
  [h1 [htxt "Category list"]
  ,spTable
    ([take 2 categoryLabelList]
      ++ map listCategory (sortBy leqCategory categorys))]
  where
    listCategory category =
      categoryToListView category
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute category) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute category) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute category) [htxt "Delete"]]])