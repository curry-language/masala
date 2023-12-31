module View.Category
  ( wCategory, tuple2Category, category2Tuple, wCategoryType, wCategoryDescType
  , showCategoryView, listCategoryView, allCategoriesView
  , allCategoriesPackagesView ) where

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Model.Masala2
import Config.EntityRoutes
import System.SessionInfo
import System.Spicey
import View.EntitiesToHtml
import View.Package

------------------------------------------------------------------------------
--- The WUI specification for the entity type Category.
--- It also includes fields for associated entities.
wCategory :: WuiSpec (String,String)
wCategory =
  withRendering
   (wPair (wRequiredStringSize 60) (wTextArea (5,60)))
   (renderLabels (take 2 categoryLabelList))

--- Transformation from data of a WUI form to entity type Category.
tuple2Category :: Category -> (String,String) -> Category
tuple2Category categoryToUpdate (name,description) =
  setCategoryName (setCategoryDescription categoryToUpdate description) name

--- Transformation from entity type Category to a tuple
--- which can be used in WUI specifications.
category2Tuple :: Category -> (String,String)
category2Tuple category = (categoryName category, categoryDescription category)

--- WUI Type for editing or creating Category entities.
--- Includes fields for associated entities.
wCategoryType :: Category -> WuiSpec Category
wCategoryType category =
  transformWSpec (tuple2Category category,category2Tuple)
    wCategory

------------------------------------------------------------------------------
--- The WUI specification for the entity type Category.
--- It also includes fields for associated entities.
wCategoryDesc :: WuiSpec (String,String)
wCategoryDesc =
  withRendering
    (wPair (wConstant htxt) (wTextArea (5,60)))
    (renderLabels (take 2 categoryLabelList))

--- Transformation from data of a WUI form to entity type Category.
tuple2CategoryDesc :: Category -> (String,String) -> Category
tuple2CategoryDesc categoryToUpdate (name,description) =
  setCategoryName (setCategoryDescription categoryToUpdate description) name

--- Transformation from entity type Category to a tuple
--- which can be used in WUI specifications.
categoryDesc2Tuple :: Category -> (String,String)
categoryDesc2Tuple category =
  (categoryName category, categoryDescription category)

--- WUI Type for editing or creating Category entities
--- without asscoiated entities
wCategoryDescType :: Category -> WuiSpec Category
wCategoryDescType category =
  transformWSpec (tuple2CategoryDesc category,categoryDesc2Tuple) wCategoryDesc

------------------------------------------------------------------------------
--- Supplies a view to show the details of a Category.
showCategoryView :: UserSessionInfo -> Category -> [Package] -> [BaseHtml]
showCategoryView sinfo category packages =
  [ h1 $ [htxt $ categoryName category, nbsp] ++ editButton ++ delButton,
          hrule ] ++
  (if null catdesc then [] else [par [italic [htxt catdesc, hrule]]]) ++ 
  [ h5 [htxt "Packages in this category:"]
  , par (intersperse nbsp (map listPackage (sortBy leqPackage packages)))
  , hrule ]
 where
  editButton = if isAdminSession sinfo
                 then [htmlStruct "small" []
                         [hrefWarnBadge (editRoute category)
                                        [htxt "Edit description"], nbsp]]
                 else []
  delButton = if isAdminSession sinfo && null packages
                 then [htmlStruct "small" []
                         [hrefWarnBadge (deleteRoute category)
                                        [htxt "Delete category"]]]
                 else []
  catdesc = categoryDescription category
  listPackage p = hrefPrimBadge (showRoute p) [htxt (packageName p)]

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

--- A view for a given list of Category entities.
allCategoriesView :: UserSessionInfo -> [Category] -> [BaseHtml]
allCategoriesView sinfo categorys =
  [ h1 [htxt "All categories"]
  , spTable $ [[[htxt "Category name"], [htxt "Description"]]] ++
              map listCategory (sortBy leqCategory categorys)
  ] ++
  if isAdminSession sinfo
    then [ breakline
         , hrefWarnSmButton "?Category/new" [htxt "Add a new category"]]
    else []
  where
   listCategory category =
     [[hrefPrimSmBlock (showRoute category) [htxt (categoryName category)]]
     , [htxt (categoryDescription category)]]

--- A view for a given list of Category entities.
allCategoriesPackagesView :: UserSessionInfo -> [(Category,[Package])]
                          -> [BaseHtml]
allCategoriesPackagesView sinfo catpkgs =
  [ h1 [htxt "All categories and their packages"] ] ++
  concatMap showCatPkgs
            (sortBy (\ (c1,_) (c2,_) -> leqCategory c1 c2) catpkgs) ++
  if isAdminSession sinfo
    then [ breakline
         , hrefWarnSmButton "?Category/new" [htxt "Add a new category"]]
    else []
  where
    showCatPkgs (category,pkgs) =
      [h3 [hrefInfoBadge (showRoute category)
                         [htxt (categoryName category)]]] ++
      (if null catdesc then [] else [par [italic [htxt catdesc]]]) ++ 
  
      [ par (intersperse nbsp
               (map listPackage (sortBy leqPackage pkgs)))
      , hrule
      ]
     where
      catdesc = categoryDescription category
      listPackage p = hrefPrimBadge (showRoute p) [htxt (packageName p)]
