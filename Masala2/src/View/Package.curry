module View.Package
  ( wPackage, tuple2Package, package2Tuple, wPackageType, showPackageView
  , leqPackage, listPackageView, allPackagesView ) where

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

--- The WUI specification for the entity type Package.
wPackage :: WuiSpec (String,Bool)
wPackage =
  withRendering (wPair wRequiredString wBoolean)
   (renderLabels packageLabelList)

--- Transformation from data of a WUI form to entity type Package.
tuple2Package :: Package -> (String,Bool) -> Package
tuple2Package packageToUpdate (name,abandoned) =
  setPackageName (setPackageAbandoned packageToUpdate abandoned) name

--- Transformation from entity type Package to a tuple
--- which can be used in WUI specifications.
package2Tuple :: Package -> (String,Bool)
package2Tuple package = (packageName package,packageAbandoned package)

--- WUI Type for editing or creating Package entities.
--- Includes fields for associated entities.
wPackageType :: Package -> WuiSpec Package
wPackageType package =
  transformWSpec (tuple2Package package,package2Tuple) wPackage

--- Supplies a view to show the details of a Package.
showPackageView :: UserSessionInfo -> [Version] -> Package -> [BaseHtml]
showPackageView _ versions package =
  [h2 [htxt $ "Package: " ++ packageName package]
  ,h5 [htxt $ "Abandoned: " ++ show (packageAbandoned package)]
  ,h5 [htxt $ "Versions:"]
  ,par (intersperse nbsp
          (map (\v -> hrefPrimBadge (showRoute v) [htxt (versionVersion v)])
            versions))
  ,hrefPrimSmButton "?Package/list" [htxt "back to Package list"]]

--- Compares two Package entities. This order is used in the list view.
leqPackage :: Package -> Package -> Bool
leqPackage x1 x2 =
  (packageName x1,packageAbandoned x1) <= (packageName x2,packageAbandoned x2)

--- Supplies a list view for a given list of Package entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of Package entities.
listPackageView :: UserSessionInfo -> [Package] -> [BaseHtml]
listPackageView sinfo packages =
  [h1 [htxt "Package list"]
  ,spTable
    ([take 2 packageLabelList]
      ++ map listPackage (sortBy leqPackage packages))]
  where
    listPackage package =
      [[hrefPrimBadge (showRoute package) [htxt (packageName package)]]
      ,[boolToHtml (packageAbandoned package)]] ++
      (if userLoginOfSession sinfo == Nothing
          then []
          else [[hrefPrimBadge (showRoute package) [htxt "Show"]]
               ,[hrefPrimBadge (editRoute package) [htxt "Edit"]]
               ,[hrefPrimBadge (deleteRoute package) [htxt "Delete"]]])

--- A view for a given list of Package entities.
allPackagesView :: UserSessionInfo -> String -> [Package] -> [BaseHtml]
allPackagesView _ title packages =
  [ h1 [htxt title]
  , par (intersperse nbsp (map listPackage (sortBy leqPackage packages)))]
  where
    listPackage package =
      (if packageAbandoned package then hrefScndBadge else hrefPrimBadge)
        (showRoute package)
        [htxt (packageName package)]
