module View.EntitiesToHtml where

import Data.Time
import HTML.Base
import HTML.WUI
import System.Spicey
import Masala2

--- The list view of a User entity in HTML format.
--- This view is used in a row of a table of all entities.
userToListView :: HTML h => User -> [[h]]
userToListView user =
  [[stringToHtml (userName user)]
  ,[stringToHtml (userEmail user)]
  ,[stringToHtml (userPublicEmail user)]
  ,[stringToHtml (userRole user)]
  ,[stringToHtml (userPassword user)]
  ,[stringToHtml (userToken user)]
  ,[maybeDateToHtml (userLastLogin user)]]

--- The short view of a User entity as a string.
--- This view is used in menus and comments to refer to a User entity.
userToShortView :: User -> String
userToShortView user = userName user

--- The detailed view of a User entity in HTML format.
--- It also takes associated entities for every associated entity type.
userToDetailsView :: HTML h => User -> [Package] -> [Package] -> [h]
userToDetailsView user maintainerpackages watchingpackages =
  [spTable
    (map (\(label,value) -> [label,value]) (zip userLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (userName user)]
      ,[stringToHtml (userEmail user)]
      ,[stringToHtml (userPublicEmail user)]
      ,[stringToHtml (userRole user)]
      ,[stringToHtml (userPassword user)]
      ,[stringToHtml (userToken user)]
      ,[maybeDateToHtml (userLastLogin user)]
      ,[htxt (unwords (map packageToShortView maintainerpackages))]
      ,[htxt (unwords (map packageToShortView watchingpackages))]]

--- The labels of a User entity, as used in HTML tables.
userLabelList :: HTML h => [[h]]
userLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Email"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "PublicEmail"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Role"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Password"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Token"]
  ,[textstyle "spicey_label spicey_label_for_type_date" "LastLogin"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Maintains Packages"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Watching Packages"]]

--- The list view of a Package entity in HTML format.
--- This view is used in a row of a table of all entities.
packageToListView :: HTML h => Package -> [[h]]
packageToListView package =
  [[stringToHtml (packageName package)]
  ,[boolToHtml (packageAbandoned package)]]

--- The short view of a Package entity as a string.
--- This view is used in menus and comments to refer to a Package entity.
packageToShortView :: Package -> String
packageToShortView package = packageName package

--- The detailed view of a Package entity in HTML format.
packageToDetailsView :: HTML h => Package -> [h]
packageToDetailsView package =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip packageLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (packageName package)]
      ,[boolToHtml (packageAbandoned package)]]

--- The labels of a Package entity, as used in HTML tables.
packageLabelList :: HTML h => [[h]]
packageLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Abandoned"]]

--- The list view of a Version entity in HTML format.
--- This view is used in a row of a table of all entities.
versionToListView :: HTML h => Version -> [[h]]
versionToListView version =
  [[stringToHtml (versionVersion version)]
  ,[boolToHtml (versionPublished version)]
  ,[boolToHtml (versionTested version)]
  ,[stringToHtml (versionDescription version)]
  ,[stringToHtml (versionJobStatus version)]
  ,[intToHtml (versionDownloads version)]
  ,[dateToHtml (versionUploadDate version)]
  ,[boolToHtml (versionDeprecated version)]]

--- The short view of a Version entity as a string.
--- This view is used in menus and comments to refer to a Version entity.
versionToShortView :: Version -> String
versionToShortView version = versionVersion version

--- The detailed view of a Version entity in HTML format.
--- It also takes associated entities for every associated entity type.
versionToDetailsView
  :: HTML h => Version -> User -> Package -> [Package] -> [CurryModule] -> [h]
versionToDetailsView
    version relatedUser relatedPackage packages curryModules =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip versionLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (versionVersion version)]
      ,[boolToHtml (versionPublished version)]
      ,[boolToHtml (versionTested version)]
      ,[stringToHtml (versionDescription version)]
      ,[stringToHtml (versionJobStatus version)]
      ,[intToHtml (versionDownloads version)]
      ,[dateToHtml (versionUploadDate version)]
      ,[boolToHtml (versionDeprecated version)]
      ,[htxt (userToShortView relatedUser)]
      ,[htxt (packageToShortView relatedPackage)]
      ,[htxt (unwords (map packageToShortView packages))]
      ,[htxt (unwords (map curryModuleToShortView curryModules))]]

--- The labels of a Version entity, as used in HTML tables.
versionLabelList :: HTML h => [[h]]
versionLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Version"]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Published"]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Tested"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Description"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "JobStatus"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Downloads"]
  ,[textstyle "spicey_label spicey_label_for_type_date" "UploadDate"]
  ,[textstyle "spicey_label spicey_label_for_type_bool" "Deprecated"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Uploaded by"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Version of Package"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Depends on Packages"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "CurryModule"]]

--- The list view of a Category entity in HTML format.
--- This view is used in a row of a table of all entities.
categoryToListView :: HTML h => Category -> [[h]]
categoryToListView category =
  [[stringToHtml (categoryName category)]
  ,[stringToHtml (categoryDescription category)]]

--- The short view of a Category entity as a string.
--- This view is used in menus and comments to refer to a Category entity.
categoryToShortView :: Category -> String
categoryToShortView category = categoryName category

--- The detailed view of a Category entity in HTML format.
--- It also takes associated entities for every associated entity type.
categoryToDetailsView :: HTML h => Category -> [Version] -> [h]
categoryToDetailsView category versions =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip categoryLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (categoryName category)]
      ,[stringToHtml (categoryDescription category)]
      ,[htxt (unwords (map versionToShortView versions))]]

--- The labels of a Category entity, as used in HTML tables.
categoryLabelList :: HTML h => [[h]]
categoryLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Description"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Version"]]

--- The list view of a CurryModule entity in HTML format.
--- This view is used in a row of a table of all entities.
curryModuleToListView :: HTML h => CurryModule -> [[h]]
curryModuleToListView curryModule =
  [[stringToHtml (curryModuleName curryModule)]]

--- The short view of a CurryModule entity as a string.
--- This view is used in menus and comments to refer to a CurryModule entity.
curryModuleToShortView :: CurryModule -> String
curryModuleToShortView curryModule = curryModuleName curryModule

--- The detailed view of a CurryModule entity in HTML format.
curryModuleToDetailsView :: HTML h => CurryModule -> [h]
curryModuleToDetailsView curryModule =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip curryModuleLabelList detailedView))]
  where
    detailedView = [[stringToHtml (curryModuleName curryModule)]]

--- The labels of a CurryModule entity, as used in HTML tables.
curryModuleLabelList :: HTML h => [[h]]
curryModuleLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]]

--- The list view of a ValidationToken entity in HTML format.
--- This view is used in a row of a table of all entities.
validationTokenToListView :: HTML h => ValidationToken -> [[h]]
validationTokenToListView validationToken =
  [[stringToHtml (validationTokenToken validationToken)]
  ,[dateToHtml (validationTokenValidSince validationToken)]]

--- The short view of a ValidationToken entity as a string.
--- This view is used in menus and comments to refer to a ValidationToken entity.
validationTokenToShortView :: ValidationToken -> String
validationTokenToShortView validationToken =
  validationTokenToken validationToken

--- The detailed view of a ValidationToken entity in HTML format.
--- It also takes associated entities for every associated entity type.
validationTokenToDetailsView :: HTML h => ValidationToken -> User -> [h]
validationTokenToDetailsView validationToken relatedUser =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip validationTokenLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (validationTokenToken validationToken)]
      ,[dateToHtml (validationTokenValidSince validationToken)]
      ,[htxt (userToShortView relatedUser)]]

--- The labels of a ValidationToken entity, as used in HTML tables.
validationTokenLabelList :: HTML h => [[h]]
validationTokenLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Token"]
  ,[textstyle "spicey_label spicey_label_for_type_date" "ValidSince"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "User"]]
