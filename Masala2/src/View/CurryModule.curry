module View.CurryModule
  ( wCurryModule, tuple2CurryModule, curryModule2Tuple, wCurryModuleType
  , showCurryModuleView, listCurryModuleView ) where

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

--- The WUI specification for the entity type CurryModule.
wCurryModule :: WuiSpec String
wCurryModule =
  withRendering wRequiredString (renderLabels curryModuleLabelList)

--- Transformation from data of a WUI form to entity type CurryModule.
tuple2CurryModule :: CurryModule -> String -> CurryModule
tuple2CurryModule curryModuleToUpdate name =
  setCurryModuleName curryModuleToUpdate name

--- Transformation from entity type CurryModule to a tuple
--- which can be used in WUI specifications.
curryModule2Tuple :: CurryModule -> String
curryModule2Tuple curryModule = curryModuleName curryModule

--- WUI Type for editing or creating CurryModule entities.
--- Includes fields for associated entities.
wCurryModuleType :: CurryModule -> WuiSpec CurryModule
wCurryModuleType curryModule =
  transformWSpec (tuple2CurryModule curryModule,curryModule2Tuple)
   wCurryModule

--- Supplies a view to show the details of a CurryModule.
showCurryModuleView :: UserSessionInfo -> CurryModule -> [BaseHtml]
showCurryModuleView _ curryModule =
  curryModuleToDetailsView curryModule
   ++ [hrefPrimSmButton "?CurryModule/list" [htxt "back to CurryModule list"]]

--- Compares two CurryModule entities. This order is used in the list view.
leqCurryModule :: CurryModule -> CurryModule -> Bool
leqCurryModule x1 x2 = curryModuleName x1 <= curryModuleName x2

--- Supplies a list view for a given list of CurryModule entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of CurryModule entities.
listCurryModuleView :: UserSessionInfo -> [CurryModule] -> [BaseHtml]
listCurryModuleView sinfo curryModules =
  [h1 [htxt "CurryModule list"]
  ,spTable
    ([take 1 curryModuleLabelList]
      ++ map listCurryModule (sortBy leqCurryModule curryModules))]
  where
    listCurryModule curryModule =
      curryModuleToListView curryModule
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute curryModule) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute curryModule) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute curryModule)
                      [htxt "Delete"]]])