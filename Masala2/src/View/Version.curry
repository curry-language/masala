module View.Version
  ( wVersion, tuple2Version, version2Tuple, wVersionType
  , showStandardVersionView, showVersionView, allVersionsView
  , listVersionView, leqVersion ) where

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Model.Masala2
import Config.EntityRoutes
import Config.Masala
import Config.Roles
import System.SessionInfo
import System.Spicey
import View.EntitiesToHtml
import View.Package

--- The WUI specification for the entity type Version.
--- It also includes fields for associated entities.
wVersion
  :: [User]
  -> [Package]
  -> [Package]
  -> [CurryModule]
  -> WuiSpec (String
             ,Bool
             ,Bool
             ,String
             ,String
             ,Int
             ,ClockTime
             ,Bool
             ,User
             ,Package
             ,[Package]
             ,[CurryModule])
wVersion userList versioningPackageList dependingPackageList curryModuleList =
  withRendering
   (w12Tuple wRequiredString wBoolean wBoolean wRequiredString wString wInt
     wDateType
     wBoolean
     (wSelect userToShortView userList)
     (wSelect packageToShortView versioningPackageList)
     (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
       dependingPackageList)
     (wMultiCheckSelect
       (\curryModule -> [htxt (curryModuleToShortView curryModule)])
       curryModuleList))
   (renderLabels versionLabelList)

--- Transformation from data of a WUI form to entity type Version.
tuple2Version
  :: Version
  -> (String
     ,Bool
     ,Bool
     ,String
     ,String
     ,Int
     ,ClockTime
     ,Bool
     ,User
     ,Package
     ,[Package]
     ,[CurryModule])
  -> (Version,[Package],[CurryModule])
tuple2Version
    versionToUpdate
    (version,published,tested,description,jobStatus,downloads,uploadDate
    ,deprecated,user,package,dependencypackages,curryModules) =
  (setVersionVersion
    (setVersionPublished
      (setVersionTested
        (setVersionDescription
          (setVersionJobStatus
            (setVersionDownloads
              (setVersionUploadDate
                (setVersionDeprecated
                  (setVersionPackageVersioningKey
                    (setVersionUserUploadKey versionToUpdate (userKey user))
                    (packageKey package))
                  deprecated)
                uploadDate)
              downloads)
            jobStatus)
          description)
        tested)
      published)
    version
  ,dependencypackages
  ,curryModules)

--- Transformation from entity type Version to a tuple
--- which can be used in WUI specifications.
version2Tuple
  :: User
  -> Package
  -> (Version,[Package],[CurryModule])
  -> (String
     ,Bool
     ,Bool
     ,String
     ,String
     ,Int
     ,ClockTime
     ,Bool
     ,User
     ,Package
     ,[Package]
     ,[CurryModule])
version2Tuple user package (version,dependencypackages,curryModules) =
  (versionVersion version
  ,versionPublished version
  ,versionTested version
  ,versionDescription version
  ,versionJobStatus version
  ,versionDownloads version
  ,versionUploadDate version
  ,versionDeprecated version
  ,user
  ,package
  ,dependencypackages
  ,curryModules)

--- WUI Type for editing or creating Version entities.
--- Includes fields for associated entities.
wVersionType
  :: Version
  -> User
  -> Package
  -> [User]
  -> [Package]
  -> [Package] -> [CurryModule] -> WuiSpec (Version,[Package],[CurryModule])
wVersionType
    version user package userList versioningPackageList dependingPackageList
    curryModuleList =
  transformWSpec (tuple2Version version,version2Tuple user package)
   (wVersion userList versioningPackageList dependingPackageList
             curryModuleList)

--- Supplies a view to show the details of a Version.
showStandardVersionView
  :: UserSessionInfo
  -> Version -> User -> Package -> [Package] -> [CurryModule] -> [BaseHtml]
showStandardVersionView _ version relatedUser relatedPackage packages
                        curryModules =
  versionToDetailsView version relatedUser relatedPackage packages
   curryModules
   ++ [hrefPrimSmButton "?Version/list" [htxt "back to Version list"]]

--- Supplies a view to show the details of a Version.
showVersionView
  :: UserSessionInfo
  -> Version -> Package -> User -> [User] -> [Category] -> [Version]
  -> [Package] -> [CurryModule] -> Bool -> Maybe User -> [BaseHtml]
showVersionView sinfo version package uploader maintainers cats allversions
                deppackages exportedmods watchesPackage currentUser =
  [blockstyle "container-fluid"
     ([blockstyle "row"
        ([blockstyle (bsCols 4)
                   [blockstyle "card" sidemenu],
          blockstyle (bsCols 8)
                   (headerRow ++ contents)])])]
 where
  bsCols n = "col-sm-" ++ show n ++ " " ++ "col-md-" ++ show n
  
  -- header row:
  headerRow = [htmlStruct "header" [("class","jumbotron")]
                 [h1 $ [smallMutedText "Curry Package ",
                        htxt $ packageName package] ++
                        (if isAdminSession sinfo
                           then [nbsp,
                                 hStruct "small"
                                   [hrefWarnBadge (deleteRoute version)
                                      [htxt $ "Delete version " ++
                                              versionVersion version]]]
                           else []) ]]

  sidemenu =
    [ulistWithClass "list-group" "list-group-item"
       (map (\ (t,c) -> (h5 [htxt t] : c))
            (versionInfoAsHTML sinfo package version deppackages cats allversions
                               uploader maintainers exportedmods))] ++
    [if versionTested version
       then blockstyle "badge badge-success" [htxt "Successfully tested"]
       else blockstyle "badge badge-secondary" [htxt "not tested"]]

  pkgAbandoned = packageAbandoned package

  contents =
    [ par [htxt $ versionDescription version]
    , par [ehrefInfoBadge (packageURLinCPM (packageName package))
             [htxt "Detailed package information"],
           htxt " (available when the package has been published)"]
    ] ++
    ( case currentUser of 
        Nothing -> []
        Just currentUser' ->
          [blockstyle "badge badge-secondary"
             [ bold [htxt $ if watchesPackage then "I am watching"
                                              else "I do not watch",
                     htxt " this package "]
             , nbsp
             , hrefWarnBadge
                 (entityRoute "togglewatching" currentUser' ++ "/" ++
                  showPackageKey package)
                 [htxt $ if watchesPackage then "Stop watching"
                                           else "Start watching"]]]
    ) ++
    (if isAdminSession sinfo
       then [blockstyle "badge badge-secondary"
              [ bold [htxt $ "Abandoned: "], htxt $ show pkgAbandoned
              , nbsp
              , hrefWarnBadge (entityRoute "toggleabandoned" package)
                           [htxt $ "Set to " ++ show (not pkgAbandoned)]]]
       else if pkgAbandoned
              then [blockstyle "badge badge-secondary"
                     [ bold [htxt $ "This package is abandoned"]]]
              else []) ++
    [ hrule ]

--- Renders information about a package as HTML description list.
versionInfoAsHTML :: UserSessionInfo -> Package -> Version -> [Package]
                  -> [Category] -> [Version] -> User -> [User] -> [CurryModule]
                  -> [(String,[BaseHtml])]
versionInfoAsHTML sinfo package version deppackages cats allversions uploader
                  maintainers exportedmods =
  [("Categor" ++ if length cats == 1 then "y" else "ies",
    hitems $ map (\c -> hrefPrimBadge (showRoute c) [htxt $ categoryName c])
                 cats)] ++
  [ ("Versions", hitems $ map (showPkgVersion version)
                              (reverse (sortBy leqVersion allversions)))
  , ("Dependencies", hitems $ map dep2html $ deppackages) ] ++
  expmods ++
  [ ("Uploaded by",
     [userToRef uploader, breakline,
      htxt $ "at " ++
             calendarTimeToString (toUTCTime (versionUploadDate version)) ++
             " (UTC)"])
  , ("Maintainer" ++ if length maintainers > 1 then "s" else "",
     concatMap ((++ [breakline]) .
                userToEntry (canEditMaintainers && length maintainers > 1))
               maintainers ++ maintainerControl)
  , ("Visibility",
     [htxt (publicText (versionPublished version)), nbsp] ++ togglePublicButton)
  , ("Deprecated",
     [htxt (if versionDepr then "yes" else "no")] ++ toggleDeprButton)
  , ("JobStatus", [htxt $ versionJobStatus version])
  , ("Downloads", [htxt $ show $ versionDownloads version])
  ]
 where
  canEditMaintainers = case userLoginOfSession sinfo of
    Nothing             -> False
    Just (loginname, _) -> isAdminSession sinfo ||
                           loginname `elem` map userLoginName maintainers

  maintainerControl =
    if canEditMaintainers
      then [hrefWarnBadge (entityRoute "addmaintainer" package)
                          [htxt "Add new maintainer"]]
      else []

  userToEntry True  u = [userToRef u, nbsp, userToRemoveButton u]
  userToEntry False u = [userToRef u]

  userToRemoveButton u =
    hrefWarnBadge
      (entityRoute "removemaintainer" package ++ "/" ++ showUserKey u)
      [htxt "Remove"]
      
  publicText b = if b then "Public" else "Private"
  versionDepr = versionDeprecated version

  userToRef u = hrefScndBadge (showRoute u) [htxt (userToShortView u)]

  dep2html deppkg =
    hrefPrimBadge (showRoute deppkg) [htxt (packageName deppkg)]

  expmods =
    if null exportedmods
      then []
      else [("Exported modules",
             hitems $ map (\m -> code [textstyle "badge badge-dark"
                                                 (curryModuleName m)])
                          exportedmods)]

  togglePublicButton = case userLoginOfSession sinfo of
    Nothing             -> []
    Just (loginname, role)
      | versionPublished version && isAdminSession sinfo
        -> [hrefWarnBadge (entityRoute "togglepublic" version)
              [htxt "Publish this package version again", uploadSpinner]
              `addAttr` onClickAttr]
      | versionPublished version -> []
      | isAdminSession sinfo ||
        (loginname `elem` map userLoginName maintainers && role == roleTrusted)
        -> [hrefWarnBadge (entityRoute "togglepublic" version)
              [htxt "Publish this package version", uploadSpinner]
              `addAttr` onClickAttr]
      | loginname `elem` map userLoginName maintainers
        -> [hrefWarnBadge (entityRoute "requestpublish" version)
              [htxt "Request publishing this version"]]
      | otherwise -> []
   where
    onClickAttr = ("onClick","setDisplayInlineBlock('uploadSpinner')")
    uploadSpinner = textstyle "spinner-border spinner-border-sm" "Publishing..."
      `addAttrs` [("id","uploadSpinner"), ("role","status"),
                  ("style","display:none")]

  toggleDeprButton =
    if isAdminSession sinfo
      then [nbsp,
            hrefWarnBadge (entityRoute "toggledepr" version)
               [htxt $ "Set to " ++ if versionDepr then "no" else "yes"]]
      else []

showPkgVersion :: Version -> Version -> BaseHtml
showPkgVersion currversion v =
  (if versionVersion currversion == vers then hrefPrimBadge else hrefScndBadge)
    (showRoute v) [htxt vers]
 where
  vers = versionVersion v

--- Horizontal placement of HTML expressions separated by blanks.
hitems :: [BaseHtml] -> [BaseHtml]
hitems = intersperse (htxt " ")

--- Vertical placement of HTML expressions.
vitems :: [BaseHtml] -> [BaseHtml]
vitems = intersperse breakline

--- Compares two Version entities. This order is used in the list view.
leqVersion :: Version -> Version -> Bool
leqVersion v1 v2 = readVersionString v1 <= readVersionString v2
 where
  readVersionString v = case split (=='.') (versionVersion v) of
    [majs,mins,revpres] -> let (revs,pres) = break (=='-') revpres
                           in (readN majs, readN mins, read revs, pres)
    _                   -> (0,0,0,"")

  readN s | all isDigit s = read s
          | otherwise     = 0

--- A view for a given list of Package entities.
allVersionsView :: UserSessionInfo -> String -> [(Package,Version)] -> [BaseHtml]
allVersionsView _ title pkgversions =
  [ h1 [htxt title]
  , par (intersperse nbsp (map listVersion (sortBy leqPkgVersion pkgversions)))]
 where
  listVersion (pkg,version) =
      (if packageAbandoned pkg then hrefScndBadge else hrefPrimBadge)
        (showRoute version)
        [htxt (packageName pkg ++ "-" ++ versionVersion version)]

  leqPkgVersion (p1,v1) (p2,v2) =
    leqPackage p1 p2 || (packageName p1 == packageName p2 && leqVersion v1 v2)

--- Supplies a list view for a given list of Version entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of Version entities.
listVersionView :: UserSessionInfo -> [(Package,Version)] -> [BaseHtml]
listVersionView sinfo pkgversions =
  [h1 [htxt "Package version list"]
  ,spTable
    ([[textstyle "spicey_label" "Package"] : take 8 versionLabelList]
      ++ map listVersion (sortBy leqPkgVersion pkgversions))]
 where
  leqPkgVersion (p1,v1) (p2,v2) =
    leqPackage p1 p2 || (packageName p1 == packageName p2 && leqVersion v1 v2)

  listVersion (pkg,version) =
      packageVersionToListView (pkg,version)
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute version) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute version) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute version) [htxt "Delete"]]])
