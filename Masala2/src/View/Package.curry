module View.Package
  ( wPackage, tuple2Package, package2Tuple, wPackageType
  , showNoVersionPackageView, showPackageView
  , wAddMaintainer, wAddMaintainerType
  , wAddMaintainerAdmin, wAddMaintainerTypeAdmin
  , leqPackage, listPackageView, allPackagesView
  , removeMaintainerView ) where

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

--- Supplies a view to show the a package having no versions.
--- For the admin, a delete button is displayed.
showNoVersionPackageView :: UserSessionInfo -> Package -> [BaseHtml]
showNoVersionPackageView sinfo package =
  [ h1 $ [smallMutedText "Curry Package ", htxt $ packageName package] ++
         deleteButton
  , h5 [htxt $ "This package has no versions."]]
 where
  deleteButton =
    if isAdminSession sinfo
      then [nbsp,
            hStruct "small" [hrefWarnBadge (deleteRoute package)
                               [htxt $ "Delete package"]]]
      else []

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

removeMaintainerView :: Maybe (Package, String, UserID) -> [HtmlExp]
removeMaintainerView Nothing = [ h3 [htxt "Something went wrong!"]]
removeMaintainerView (Just (package, userName, user)) =
    [ h3 [htxt "Removing maintainer:"]
    , htxt $ "Are you sure you want to remove '"
      ++ userName ++ "' as a maintainer from package '"
      ++ packageName package ++ "'?"
    , nbsp
    , primSmButton "Remove" removeHandler, nbsp
    , primSmButton "Cancel" cancelHandler]
  where 
    removeHandler :: (HtmlRef -> String) -> IO HtmlPage
    removeHandler _ = do 
      result <- runT $ deleteMaintainer user (packageKey package)
      case result of 
        Left _ -> do 
          setPageMessage "Something went wrong, please try again"
        Right _ -> do 
          setPageMessage "Maintainer has been removed"
      redirectController (showRoute package) >>= getPage
    
    cancelHandler :: (HtmlRef -> String) -> IO HtmlPage
    cancelHandler _ = do
      redirectController (showRoute package) >>= getPage

--- The WUI specification for adding a User as a Maintainer.
--- Normal users must give the name of the new Maintainer.
wAddMaintainer :: WuiSpec String
wAddMaintainer =
  withRendering
    wRequiredString
    (renderLabels addMaintainerLabels)

--- The WUI specification for adding a User as a Maintainer.
--- An Admin gets a list of all possible users.
wAddMaintainerAdmin :: [String] -> WuiSpec [String]
wAddMaintainerAdmin users =
  wMultiCheckSelect (\user -> [htxt user]) users

--- WUI Type for adding a maintainer.
wAddMaintainerType :: Package -> WuiSpec (Package, String)
wAddMaintainerType package =
  transformWSpec (\name -> (package, name), \(_, name) -> name) wAddMaintainer

--- WUI Type for adding a maintainer as an Admin.
wAddMaintainerTypeAdmin :: Package -> [String] -> WuiSpec (Package, [String])
wAddMaintainerTypeAdmin package userNames =
  transformWSpec (\names -> (package, names), snd) (wAddMaintainerAdmin userNames)