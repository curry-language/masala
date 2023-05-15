module View.Version
  ( wVersion, tuple2Version, version2Tuple, wVersionType, showVersionView
  , listVersionView ) where

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
   (w12Tuple wRequiredString wBoolean wBoolean wRequiredString wRequiredString
     wInt
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
showVersionView
  :: UserSessionInfo
  -> Version -> User -> Package -> [Package] -> [CurryModule] -> [BaseHtml]
showVersionView _ version relatedUser relatedPackage packages curryModules =
  versionToDetailsView version relatedUser relatedPackage packages
   curryModules
   ++ [hrefPrimSmButton "?Version/list" [htxt "back to Version list"]]

--- Compares two Version entities. This order is used in the list view.
leqVersion :: Version -> Version -> Bool
leqVersion x1 x2 =
  (versionVersion x1
  ,versionPublished x1
  ,versionTested x1
  ,versionDescription x1
  ,versionJobStatus x1)
   <= (versionVersion x2
      ,versionPublished x2
      ,versionTested x2
      ,versionDescription x2
      ,versionJobStatus x2)

--- Supplies a list view for a given list of Version entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of Version entities.
listVersionView :: UserSessionInfo -> [Version] -> [BaseHtml]
listVersionView sinfo versions =
  [h1 [htxt "Version list"]
  ,spTable
    ([take 8 versionLabelList]
      ++ map listVersion (sortBy leqVersion versions))]
  where
    listVersion version =
      versionToListView version
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute version) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute version) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute version) [htxt "Delete"]]])