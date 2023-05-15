--- This file has been generated from
--- 
---     Masala2ERD.curry
--- 
--- and contains definitions for all entities and relations
--- specified in this model.

module Masala2 where

import qualified Data.Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

data Categorizes = Categorizes CategoryID VersionID
 deriving (Eq,Show,Read)

data CategorizesID = CategorizesID Int
 deriving (Eq,Show,Read)

data Maintainer = Maintainer UserID PackageID
 deriving (Eq,Show,Read)

data MaintainerID = MaintainerID Int
 deriving (Eq,Show,Read)

data Depending = Depending VersionID PackageID
 deriving (Eq,Show,Read)

data DependingID = DependingID Int
 deriving (Eq,Show,Read)

data Exporting = Exporting VersionID CurryModuleID
 deriving (Eq,Show,Read)

data ExportingID = ExportingID Int
 deriving (Eq,Show,Read)

data Watching = Watching UserID PackageID
 deriving (Eq,Show,Read)

data WatchingID = WatchingID Int
 deriving (Eq,Show,Read)

data User = User UserID String String String String String String (Maybe Data.Time.ClockTime)
 deriving (Eq,Show,Read)

data UserID = UserID Int
 deriving (Eq,Show,Read)

data Package = Package PackageID String Bool
 deriving (Eq,Show,Read)

data PackageID = PackageID Int
 deriving (Eq,Show,Read)

data Version = Version VersionID String Bool Bool String String Int Data.Time.ClockTime Bool PackageID UserID
 deriving (Eq,Show,Read)

data VersionID = VersionID Int
 deriving (Eq,Show,Read)

data Category = Category CategoryID String String
 deriving (Eq,Show,Read)

data CategoryID = CategoryID Int
 deriving (Eq,Show,Read)

data CurryModule = CurryModule CurryModuleID String
 deriving (Eq,Show,Read)

data CurryModuleID = CurryModuleID Int
 deriving (Eq,Show,Read)

data ValidationToken = ValidationToken ValidationTokenID String Data.Time.ClockTime UserID
 deriving (Eq,Show,Read)

data ValidationTokenID = ValidationTokenID Int
 deriving (Eq,Show,Read)

--- The name of the SQLite database file.
sqliteDBFile :: String
sqliteDBFile = "/home/mh/home/curry/masala2/repository/Masala2/Masala2.db"

--- The ER description of the `Categorizes` entity.
categorizes_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Categorizes
categorizes_CDBI_Description =
  Database.CDBI.Description.ED "Categorizes"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Categorizes
       (CategoryID categoryCategorizesKey) (VersionID versionCategorizesKey)) ->
     [Database.CDBI.Connection.SQLInt categoryCategorizesKey
     ,Database.CDBI.Connection.SQLInt versionCategorizesKey])
   (\(Categorizes
       (CategoryID categoryCategorizesKey) (VersionID versionCategorizesKey)) ->
     [Database.CDBI.Connection.SQLInt categoryCategorizesKey
     ,Database.CDBI.Connection.SQLInt versionCategorizesKey])
   (\[Database.CDBI.Connection.SQLInt categoryCategorizesKey
     ,Database.CDBI.Connection.SQLInt versionCategorizesKey] ->
     Categorizes (CategoryID categoryCategorizesKey)
      (VersionID versionCategorizesKey))

--- The database table of the `Categorizes` entity.
categorizesTable :: Database.CDBI.Description.Table
categorizesTable = "Categorizes"

--- The database column `CategoryCategorizesKey` of the `Categorizes` entity.
categorizesColumnCategoryCategorizesKey
  :: Database.CDBI.Description.Column CategoryID
categorizesColumnCategoryCategorizesKey =
  Database.CDBI.Description.Column "\"CategoryCategorizesKey\""
   "\"Categorizes\".\"CategoryCategorizesKey\""

--- The database column `VersionCategorizesKey` of the `Categorizes` entity.
categorizesColumnVersionCategorizesKey
  :: Database.CDBI.Description.Column VersionID
categorizesColumnVersionCategorizesKey =
  Database.CDBI.Description.Column "\"VersionCategorizesKey\""
   "\"Categorizes\".\"VersionCategorizesKey\""

--- The description of the database column `CategoryCategorizesKey` of the `Categorizes` entity.
categorizesCategoryCategorizesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CategoryID
categorizesCategoryCategorizesKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Categorizes\".\"CategoryCategorizesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID categoryCategorizesKey) ->
     Database.CDBI.Connection.SQLInt categoryCategorizesKey)
   (\(Database.CDBI.Connection.SQLInt categoryCategorizesKey) ->
     CategoryID categoryCategorizesKey)

--- The description of the database column `VersionCategorizesKey` of the `Categorizes` entity.
categorizesVersionCategorizesKeyColDesc
  :: Database.CDBI.Description.ColumnDescription VersionID
categorizesVersionCategorizesKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Categorizes\".\"VersionCategorizesKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(VersionID versionCategorizesKey) ->
     Database.CDBI.Connection.SQLInt versionCategorizesKey)
   (\(Database.CDBI.Connection.SQLInt versionCategorizesKey) ->
     VersionID versionCategorizesKey)

--- Gets the attribute `CategoryCategorizesKey` of the `Categorizes` entity.
categorizesCategoryCategorizesKey :: Categorizes -> CategoryID
categorizesCategoryCategorizesKey (Categorizes a _) = a

--- Gets the attribute `VersionCategorizesKey` of the `Categorizes` entity.
categorizesVersionCategorizesKey :: Categorizes -> VersionID
categorizesVersionCategorizesKey (Categorizes _ a) = a

--- Sets the attribute `CategoryCategorizesKey` of the `Categorizes` entity.
setCategorizesCategoryCategorizesKey :: Categorizes -> CategoryID -> Categorizes
setCategorizesCategoryCategorizesKey (Categorizes _ b1) a = Categorizes a b1

--- Sets the attribute `VersionCategorizesKey` of the `Categorizes` entity.
setCategorizesVersionCategorizesKey :: Categorizes -> VersionID -> Categorizes
setCategorizesVersionCategorizesKey (Categorizes a2 _) a = Categorizes a2 a

--- Inserts a new `Categorizes` relation.
newCategorizes
  :: CategoryID -> VersionID -> Database.CDBI.Connection.DBAction ()
newCategorizes k1 k2 =
  Database.CDBI.ER.insertEntry categorizes_CDBI_Description (Categorizes k1 k2)

--- Deletes an existing `Categorizes` relation.
deleteCategorizes
  :: CategoryID -> VersionID -> Database.CDBI.Connection.DBAction ()
deleteCategorizes k1 k2 =
  Database.CDBI.ER.deleteEntryR categorizes_CDBI_Description
   categorizesColumnCategoryCategorizesKey
   (categoryID k1)
   categorizesColumnVersionCategorizesKey
   (versionID k2)

--- Gets the associated `Category` entities for a given `Version` entity
--- w.r.t. the `Categorizes` relation.
getCategoryVersions :: Category -> Database.CDBI.Connection.DBAction [Version]
getCategoryVersions en =
  Database.CDBI.ER.getEntriesWithColVal categorizes_CDBI_Description
   categorizesColumnCategoryCategorizesKey
   (categoryID (categoryKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getVersion (map categorizesVersionCategorizesKey vals))

--- The ER description of the `Maintainer` entity.
maintainer_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Maintainer
maintainer_CDBI_Description =
  Database.CDBI.Description.ED "Maintainer"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Maintainer (UserID userMaintainerKey) (PackageID packageMaintainerKey)) ->
     [Database.CDBI.Connection.SQLInt userMaintainerKey
     ,Database.CDBI.Connection.SQLInt packageMaintainerKey])
   (\(Maintainer (UserID userMaintainerKey) (PackageID packageMaintainerKey)) ->
     [Database.CDBI.Connection.SQLInt userMaintainerKey
     ,Database.CDBI.Connection.SQLInt packageMaintainerKey])
   (\[Database.CDBI.Connection.SQLInt userMaintainerKey
     ,Database.CDBI.Connection.SQLInt packageMaintainerKey] ->
     Maintainer (UserID userMaintainerKey) (PackageID packageMaintainerKey))

--- The database table of the `Maintainer` entity.
maintainerTable :: Database.CDBI.Description.Table
maintainerTable = "Maintainer"

--- The database column `UserMaintainerKey` of the `Maintainer` entity.
maintainerColumnUserMaintainerKey :: Database.CDBI.Description.Column UserID
maintainerColumnUserMaintainerKey =
  Database.CDBI.Description.Column "\"UserMaintainerKey\""
   "\"Maintainer\".\"UserMaintainerKey\""

--- The database column `PackageMaintainerKey` of the `Maintainer` entity.
maintainerColumnPackageMaintainerKey
  :: Database.CDBI.Description.Column PackageID
maintainerColumnPackageMaintainerKey =
  Database.CDBI.Description.Column "\"PackageMaintainerKey\""
   "\"Maintainer\".\"PackageMaintainerKey\""

--- The description of the database column `UserMaintainerKey` of the `Maintainer` entity.
maintainerUserMaintainerKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
maintainerUserMaintainerKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Maintainer\".\"UserMaintainerKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userMaintainerKey) ->
     Database.CDBI.Connection.SQLInt userMaintainerKey)
   (\(Database.CDBI.Connection.SQLInt userMaintainerKey) ->
     UserID userMaintainerKey)

--- The description of the database column `PackageMaintainerKey` of the `Maintainer` entity.
maintainerPackageMaintainerKeyColDesc
  :: Database.CDBI.Description.ColumnDescription PackageID
maintainerPackageMaintainerKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Maintainer\".\"PackageMaintainerKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(PackageID packageMaintainerKey) ->
     Database.CDBI.Connection.SQLInt packageMaintainerKey)
   (\(Database.CDBI.Connection.SQLInt packageMaintainerKey) ->
     PackageID packageMaintainerKey)

--- Gets the attribute `UserMaintainerKey` of the `Maintainer` entity.
maintainerUserMaintainerKey :: Maintainer -> UserID
maintainerUserMaintainerKey (Maintainer a _) = a

--- Gets the attribute `PackageMaintainerKey` of the `Maintainer` entity.
maintainerPackageMaintainerKey :: Maintainer -> PackageID
maintainerPackageMaintainerKey (Maintainer _ a) = a

--- Sets the attribute `UserMaintainerKey` of the `Maintainer` entity.
setMaintainerUserMaintainerKey :: Maintainer -> UserID -> Maintainer
setMaintainerUserMaintainerKey (Maintainer _ b1) a = Maintainer a b1

--- Sets the attribute `PackageMaintainerKey` of the `Maintainer` entity.
setMaintainerPackageMaintainerKey :: Maintainer -> PackageID -> Maintainer
setMaintainerPackageMaintainerKey (Maintainer a2 _) a = Maintainer a2 a

--- Inserts a new `Maintainer` relation.
newMaintainer :: UserID -> PackageID -> Database.CDBI.Connection.DBAction ()
newMaintainer k1 k2 =
  Database.CDBI.ER.insertEntry maintainer_CDBI_Description (Maintainer k1 k2)

--- Deletes an existing `Maintainer` relation.
deleteMaintainer :: UserID -> PackageID -> Database.CDBI.Connection.DBAction ()
deleteMaintainer k1 k2 =
  Database.CDBI.ER.deleteEntryR maintainer_CDBI_Description
   maintainerColumnUserMaintainerKey
   (userID k1)
   maintainerColumnPackageMaintainerKey
   (packageID k2)

--- Gets the associated `User` entities for a given `Package` entity
--- w.r.t. the `Maintainer` relation.
getMaintainerUserPackages :: User -> Database.CDBI.Connection.DBAction [Package]
getMaintainerUserPackages en =
  Database.CDBI.ER.getEntriesWithColVal maintainer_CDBI_Description
   maintainerColumnUserMaintainerKey
   (userID (userKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getPackage (map maintainerPackageMaintainerKey vals))

--- The ER description of the `Depending` entity.
depending_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Depending
depending_CDBI_Description =
  Database.CDBI.Description.ED "Depending"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Depending
       (VersionID versionDependingKey) (PackageID packageDependingKey)) ->
     [Database.CDBI.Connection.SQLInt versionDependingKey
     ,Database.CDBI.Connection.SQLInt packageDependingKey])
   (\(Depending
       (VersionID versionDependingKey) (PackageID packageDependingKey)) ->
     [Database.CDBI.Connection.SQLInt versionDependingKey
     ,Database.CDBI.Connection.SQLInt packageDependingKey])
   (\[Database.CDBI.Connection.SQLInt versionDependingKey
     ,Database.CDBI.Connection.SQLInt packageDependingKey] ->
     Depending (VersionID versionDependingKey) (PackageID packageDependingKey))

--- The database table of the `Depending` entity.
dependingTable :: Database.CDBI.Description.Table
dependingTable = "Depending"

--- The database column `VersionDependingKey` of the `Depending` entity.
dependingColumnVersionDependingKey :: Database.CDBI.Description.Column VersionID
dependingColumnVersionDependingKey =
  Database.CDBI.Description.Column "\"VersionDependingKey\""
   "\"Depending\".\"VersionDependingKey\""

--- The database column `PackageDependingKey` of the `Depending` entity.
dependingColumnPackageDependingKey :: Database.CDBI.Description.Column PackageID
dependingColumnPackageDependingKey =
  Database.CDBI.Description.Column "\"PackageDependingKey\""
   "\"Depending\".\"PackageDependingKey\""

--- The description of the database column `VersionDependingKey` of the `Depending` entity.
dependingVersionDependingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription VersionID
dependingVersionDependingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Depending\".\"VersionDependingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(VersionID versionDependingKey) ->
     Database.CDBI.Connection.SQLInt versionDependingKey)
   (\(Database.CDBI.Connection.SQLInt versionDependingKey) ->
     VersionID versionDependingKey)

--- The description of the database column `PackageDependingKey` of the `Depending` entity.
dependingPackageDependingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription PackageID
dependingPackageDependingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Depending\".\"PackageDependingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(PackageID packageDependingKey) ->
     Database.CDBI.Connection.SQLInt packageDependingKey)
   (\(Database.CDBI.Connection.SQLInt packageDependingKey) ->
     PackageID packageDependingKey)

--- Gets the attribute `VersionDependingKey` of the `Depending` entity.
dependingVersionDependingKey :: Depending -> VersionID
dependingVersionDependingKey (Depending a _) = a

--- Gets the attribute `PackageDependingKey` of the `Depending` entity.
dependingPackageDependingKey :: Depending -> PackageID
dependingPackageDependingKey (Depending _ a) = a

--- Sets the attribute `VersionDependingKey` of the `Depending` entity.
setDependingVersionDependingKey :: Depending -> VersionID -> Depending
setDependingVersionDependingKey (Depending _ b1) a = Depending a b1

--- Sets the attribute `PackageDependingKey` of the `Depending` entity.
setDependingPackageDependingKey :: Depending -> PackageID -> Depending
setDependingPackageDependingKey (Depending a2 _) a = Depending a2 a

--- Inserts a new `Depending` relation.
newDepending :: VersionID -> PackageID -> Database.CDBI.Connection.DBAction ()
newDepending k1 k2 =
  Database.CDBI.ER.insertEntry depending_CDBI_Description (Depending k1 k2)

--- Deletes an existing `Depending` relation.
deleteDepending
  :: VersionID -> PackageID -> Database.CDBI.Connection.DBAction ()
deleteDepending k1 k2 =
  Database.CDBI.ER.deleteEntryR depending_CDBI_Description
   dependingColumnVersionDependingKey
   (versionID k1)
   dependingColumnPackageDependingKey
   (packageID k2)

--- Gets the associated `Version` entities for a given `Package` entity
--- w.r.t. the `Depending` relation.
getVersionPackages :: Version -> Database.CDBI.Connection.DBAction [Package]
getVersionPackages en =
  Database.CDBI.ER.getEntriesWithColVal depending_CDBI_Description
   dependingColumnVersionDependingKey
   (versionID (versionKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getPackage (map dependingPackageDependingKey vals))

--- The ER description of the `Exporting` entity.
exporting_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Exporting
exporting_CDBI_Description =
  Database.CDBI.Description.ED "Exporting"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Exporting
       (VersionID versionExportingKey)
       (CurryModuleID curryModuleExportingKey)) ->
     [Database.CDBI.Connection.SQLInt versionExportingKey
     ,Database.CDBI.Connection.SQLInt curryModuleExportingKey])
   (\(Exporting
       (VersionID versionExportingKey)
       (CurryModuleID curryModuleExportingKey)) ->
     [Database.CDBI.Connection.SQLInt versionExportingKey
     ,Database.CDBI.Connection.SQLInt curryModuleExportingKey])
   (\[Database.CDBI.Connection.SQLInt versionExportingKey
     ,Database.CDBI.Connection.SQLInt curryModuleExportingKey] ->
     Exporting (VersionID versionExportingKey)
      (CurryModuleID curryModuleExportingKey))

--- The database table of the `Exporting` entity.
exportingTable :: Database.CDBI.Description.Table
exportingTable = "Exporting"

--- The database column `VersionExportingKey` of the `Exporting` entity.
exportingColumnVersionExportingKey :: Database.CDBI.Description.Column VersionID
exportingColumnVersionExportingKey =
  Database.CDBI.Description.Column "\"VersionExportingKey\""
   "\"Exporting\".\"VersionExportingKey\""

--- The database column `CurryModuleExportingKey` of the `Exporting` entity.
exportingColumnCurryModuleExportingKey
  :: Database.CDBI.Description.Column CurryModuleID
exportingColumnCurryModuleExportingKey =
  Database.CDBI.Description.Column "\"CurryModuleExportingKey\""
   "\"Exporting\".\"CurryModuleExportingKey\""

--- The description of the database column `VersionExportingKey` of the `Exporting` entity.
exportingVersionExportingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription VersionID
exportingVersionExportingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Exporting\".\"VersionExportingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(VersionID versionExportingKey) ->
     Database.CDBI.Connection.SQLInt versionExportingKey)
   (\(Database.CDBI.Connection.SQLInt versionExportingKey) ->
     VersionID versionExportingKey)

--- The description of the database column `CurryModuleExportingKey` of the `Exporting` entity.
exportingCurryModuleExportingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CurryModuleID
exportingCurryModuleExportingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Exporting\".\"CurryModuleExportingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CurryModuleID curryModuleExportingKey) ->
     Database.CDBI.Connection.SQLInt curryModuleExportingKey)
   (\(Database.CDBI.Connection.SQLInt curryModuleExportingKey) ->
     CurryModuleID curryModuleExportingKey)

--- Gets the attribute `VersionExportingKey` of the `Exporting` entity.
exportingVersionExportingKey :: Exporting -> VersionID
exportingVersionExportingKey (Exporting a _) = a

--- Gets the attribute `CurryModuleExportingKey` of the `Exporting` entity.
exportingCurryModuleExportingKey :: Exporting -> CurryModuleID
exportingCurryModuleExportingKey (Exporting _ a) = a

--- Sets the attribute `VersionExportingKey` of the `Exporting` entity.
setExportingVersionExportingKey :: Exporting -> VersionID -> Exporting
setExportingVersionExportingKey (Exporting _ b1) a = Exporting a b1

--- Sets the attribute `CurryModuleExportingKey` of the `Exporting` entity.
setExportingCurryModuleExportingKey :: Exporting -> CurryModuleID -> Exporting
setExportingCurryModuleExportingKey (Exporting a2 _) a = Exporting a2 a

--- Inserts a new `Exporting` relation.
newExporting
  :: VersionID -> CurryModuleID -> Database.CDBI.Connection.DBAction ()
newExporting k1 k2 =
  Database.CDBI.ER.insertEntry exporting_CDBI_Description (Exporting k1 k2)

--- Deletes an existing `Exporting` relation.
deleteExporting
  :: VersionID -> CurryModuleID -> Database.CDBI.Connection.DBAction ()
deleteExporting k1 k2 =
  Database.CDBI.ER.deleteEntryR exporting_CDBI_Description
   exportingColumnVersionExportingKey
   (versionID k1)
   exportingColumnCurryModuleExportingKey
   (curryModuleID k2)

--- Gets the associated `Version` entities for a given `CurryModule` entity
--- w.r.t. the `Exporting` relation.
getVersionCurryModules
  :: Version -> Database.CDBI.Connection.DBAction [CurryModule]
getVersionCurryModules en =
  Database.CDBI.ER.getEntriesWithColVal exporting_CDBI_Description
   exportingColumnVersionExportingKey
   (versionID (versionKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getCurryModule (map exportingCurryModuleExportingKey vals))

--- The ER description of the `Watching` entity.
watching_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Watching
watching_CDBI_Description =
  Database.CDBI.Description.ED "Watching"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Watching (UserID userWatchingKey) (PackageID packageWatchingKey)) ->
     [Database.CDBI.Connection.SQLInt userWatchingKey
     ,Database.CDBI.Connection.SQLInt packageWatchingKey])
   (\(Watching (UserID userWatchingKey) (PackageID packageWatchingKey)) ->
     [Database.CDBI.Connection.SQLInt userWatchingKey
     ,Database.CDBI.Connection.SQLInt packageWatchingKey])
   (\[Database.CDBI.Connection.SQLInt userWatchingKey
     ,Database.CDBI.Connection.SQLInt packageWatchingKey] ->
     Watching (UserID userWatchingKey) (PackageID packageWatchingKey))

--- The database table of the `Watching` entity.
watchingTable :: Database.CDBI.Description.Table
watchingTable = "Watching"

--- The database column `UserWatchingKey` of the `Watching` entity.
watchingColumnUserWatchingKey :: Database.CDBI.Description.Column UserID
watchingColumnUserWatchingKey =
  Database.CDBI.Description.Column "\"UserWatchingKey\""
   "\"Watching\".\"UserWatchingKey\""

--- The database column `PackageWatchingKey` of the `Watching` entity.
watchingColumnPackageWatchingKey :: Database.CDBI.Description.Column PackageID
watchingColumnPackageWatchingKey =
  Database.CDBI.Description.Column "\"PackageWatchingKey\""
   "\"Watching\".\"PackageWatchingKey\""

--- The description of the database column `UserWatchingKey` of the `Watching` entity.
watchingUserWatchingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
watchingUserWatchingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Watching\".\"UserWatchingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userWatchingKey) ->
     Database.CDBI.Connection.SQLInt userWatchingKey)
   (\(Database.CDBI.Connection.SQLInt userWatchingKey) ->
     UserID userWatchingKey)

--- The description of the database column `PackageWatchingKey` of the `Watching` entity.
watchingPackageWatchingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription PackageID
watchingPackageWatchingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Watching\".\"PackageWatchingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(PackageID packageWatchingKey) ->
     Database.CDBI.Connection.SQLInt packageWatchingKey)
   (\(Database.CDBI.Connection.SQLInt packageWatchingKey) ->
     PackageID packageWatchingKey)

--- Gets the attribute `UserWatchingKey` of the `Watching` entity.
watchingUserWatchingKey :: Watching -> UserID
watchingUserWatchingKey (Watching a _) = a

--- Gets the attribute `PackageWatchingKey` of the `Watching` entity.
watchingPackageWatchingKey :: Watching -> PackageID
watchingPackageWatchingKey (Watching _ a) = a

--- Sets the attribute `UserWatchingKey` of the `Watching` entity.
setWatchingUserWatchingKey :: Watching -> UserID -> Watching
setWatchingUserWatchingKey (Watching _ b1) a = Watching a b1

--- Sets the attribute `PackageWatchingKey` of the `Watching` entity.
setWatchingPackageWatchingKey :: Watching -> PackageID -> Watching
setWatchingPackageWatchingKey (Watching a2 _) a = Watching a2 a

--- Inserts a new `Watching` relation.
newWatching :: UserID -> PackageID -> Database.CDBI.Connection.DBAction ()
newWatching k1 k2 =
  Database.CDBI.ER.insertEntry watching_CDBI_Description (Watching k1 k2)

--- Deletes an existing `Watching` relation.
deleteWatching :: UserID -> PackageID -> Database.CDBI.Connection.DBAction ()
deleteWatching k1 k2 =
  Database.CDBI.ER.deleteEntryR watching_CDBI_Description
   watchingColumnUserWatchingKey
   (userID k1)
   watchingColumnPackageWatchingKey
   (packageID k2)

--- Gets the associated `User` entities for a given `Package` entity
--- w.r.t. the `Watching` relation.
getWatchingUserPackages :: User -> Database.CDBI.Connection.DBAction [Package]
getWatchingUserPackages en =
  Database.CDBI.ER.getEntriesWithColVal watching_CDBI_Description
   watchingColumnUserWatchingKey
   (userID (userKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getPackage (map watchingPackageWatchingKey vals))

--- The ER description of the `User` entity.
user_CDBI_Description :: Database.CDBI.Description.EntityDescription User
user_CDBI_Description =
  Database.CDBI.Description.ED "User"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeDate]
   (\(User (UserID key) name email publicEmail role password token lastLogin) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString email
     ,Database.CDBI.Connection.SQLString publicEmail
     ,Database.CDBI.Connection.SQLString role
     ,Database.CDBI.Connection.SQLString password
     ,Database.CDBI.Description.sqlString token
     ,Database.CDBI.Description.sqlDateOrNull lastLogin])
   (\(User _ name email publicEmail role password token lastLogin) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString email
     ,Database.CDBI.Connection.SQLString publicEmail
     ,Database.CDBI.Connection.SQLString role
     ,Database.CDBI.Connection.SQLString password
     ,Database.CDBI.Description.sqlString token
     ,Database.CDBI.Description.sqlDateOrNull lastLogin])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString email
     ,Database.CDBI.Connection.SQLString publicEmail
     ,Database.CDBI.Connection.SQLString role
     ,Database.CDBI.Connection.SQLString password
     ,token
     ,lastLogin] ->
     User (UserID key) name email publicEmail role password
      (Database.CDBI.Description.fromStringOrNull token)
      (Database.CDBI.Description.dateOrNothing lastLogin))

--- The database table of the `User` entity.
userTable :: Database.CDBI.Description.Table
userTable = "User"

--- The database column `Key` of the `User` entity.
userColumnKey :: Database.CDBI.Description.Column UserID
userColumnKey = Database.CDBI.Description.Column "\"Key\"" "\"User\".\"Key\""

--- The database column `Name` of the `User` entity.
userColumnName :: Database.CDBI.Description.Column String
userColumnName = Database.CDBI.Description.Column "\"Name\"" "\"User\".\"Name\""

--- The database column `Email` of the `User` entity.
userColumnEmail :: Database.CDBI.Description.Column String
userColumnEmail =
  Database.CDBI.Description.Column "\"Email\"" "\"User\".\"Email\""

--- The database column `PublicEmail` of the `User` entity.
userColumnPublicEmail :: Database.CDBI.Description.Column String
userColumnPublicEmail =
  Database.CDBI.Description.Column "\"PublicEmail\"" "\"User\".\"PublicEmail\""

--- The database column `Role` of the `User` entity.
userColumnRole :: Database.CDBI.Description.Column String
userColumnRole = Database.CDBI.Description.Column "\"Role\"" "\"User\".\"Role\""

--- The database column `Password` of the `User` entity.
userColumnPassword :: Database.CDBI.Description.Column String
userColumnPassword =
  Database.CDBI.Description.Column "\"Password\"" "\"User\".\"Password\""

--- The database column `Token` of the `User` entity.
userColumnToken :: Database.CDBI.Description.Column String
userColumnToken =
  Database.CDBI.Description.Column "\"Token\"" "\"User\".\"Token\""

--- The database column `LastLogin` of the `User` entity.
userColumnLastLogin :: Database.CDBI.Description.Column Data.Time.ClockTime
userColumnLastLogin =
  Database.CDBI.Description.Column "\"LastLogin\"" "\"User\".\"LastLogin\""

--- The description of the database column `Key` of the `User` entity.
userKeyColDesc :: Database.CDBI.Description.ColumnDescription UserID
userKeyColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> UserID key)

--- The description of the database column `Name` of the `User` entity.
userNameColDesc :: Database.CDBI.Description.ColumnDescription String
userNameColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Email` of the `User` entity.
userEmailColDesc :: Database.CDBI.Description.ColumnDescription String
userEmailColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Email\""
   Database.CDBI.Connection.SQLTypeString
   (\email -> Database.CDBI.Connection.SQLString email)
   (\(Database.CDBI.Connection.SQLString email) -> email)

--- The description of the database column `PublicEmail` of the `User` entity.
userPublicEmailColDesc :: Database.CDBI.Description.ColumnDescription String
userPublicEmailColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"PublicEmail\""
   Database.CDBI.Connection.SQLTypeString
   (\publicEmail -> Database.CDBI.Connection.SQLString publicEmail)
   (\(Database.CDBI.Connection.SQLString publicEmail) -> publicEmail)

--- The description of the database column `Role` of the `User` entity.
userRoleColDesc :: Database.CDBI.Description.ColumnDescription String
userRoleColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Role\""
   Database.CDBI.Connection.SQLTypeString
   (\role -> Database.CDBI.Connection.SQLString role)
   (\(Database.CDBI.Connection.SQLString role) -> role)

--- The description of the database column `Password` of the `User` entity.
userPasswordColDesc :: Database.CDBI.Description.ColumnDescription String
userPasswordColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Password\""
   Database.CDBI.Connection.SQLTypeString
   (\password -> Database.CDBI.Connection.SQLString password)
   (\(Database.CDBI.Connection.SQLString password) -> password)

--- The description of the database column `Token` of the `User` entity.
userTokenColDesc :: Database.CDBI.Description.ColumnDescription String
userTokenColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"Token\""
   Database.CDBI.Connection.SQLTypeString
   (\token -> Database.CDBI.Description.sqlString token)
   (\token -> Database.CDBI.Description.fromStringOrNull token)

--- The description of the database column `LastLogin` of the `User` entity.
userLastLoginColDesc
  :: Database.CDBI.Description.ColumnDescription (Maybe Data.Time.ClockTime)
userLastLoginColDesc =
  Database.CDBI.Description.ColDesc "\"User\".\"LastLogin\""
   Database.CDBI.Connection.SQLTypeDate
   (\lastLogin -> Database.CDBI.Description.sqlDateOrNull lastLogin)
   (\lastLogin -> Database.CDBI.Description.dateOrNothing lastLogin)

--- Gets the attribute `Key` of the `User` entity.
userKey :: User -> UserID
userKey (User a _ _ _ _ _ _ _) = a

--- Gets the attribute `Name` of the `User` entity.
userName :: User -> String
userName (User _ a _ _ _ _ _ _) = a

--- Gets the attribute `Email` of the `User` entity.
userEmail :: User -> String
userEmail (User _ _ a _ _ _ _ _) = a

--- Gets the attribute `PublicEmail` of the `User` entity.
userPublicEmail :: User -> String
userPublicEmail (User _ _ _ a _ _ _ _) = a

--- Gets the attribute `Role` of the `User` entity.
userRole :: User -> String
userRole (User _ _ _ _ a _ _ _) = a

--- Gets the attribute `Password` of the `User` entity.
userPassword :: User -> String
userPassword (User _ _ _ _ _ a _ _) = a

--- Gets the attribute `Token` of the `User` entity.
userToken :: User -> String
userToken (User _ _ _ _ _ _ a _) = a

--- Gets the attribute `LastLogin` of the `User` entity.
userLastLogin :: User -> Maybe Data.Time.ClockTime
userLastLogin (User _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `User` entity.
setUserKey :: User -> UserID -> User
setUserKey (User _ b7 b6 b5 b4 b3 b2 b1) a = User a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `User` entity.
setUserName :: User -> String -> User
setUserName (User a2 _ b6 b5 b4 b3 b2 b1) a = User a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `Email` of the `User` entity.
setUserEmail :: User -> String -> User
setUserEmail (User a3 a2 _ b5 b4 b3 b2 b1) a = User a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `PublicEmail` of the `User` entity.
setUserPublicEmail :: User -> String -> User
setUserPublicEmail (User a4 a3 a2 _ b4 b3 b2 b1) a = User a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Role` of the `User` entity.
setUserRole :: User -> String -> User
setUserRole (User a5 a4 a3 a2 _ b3 b2 b1) a = User a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `Password` of the `User` entity.
setUserPassword :: User -> String -> User
setUserPassword (User a6 a5 a4 a3 a2 _ b2 b1) a = User a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `Token` of the `User` entity.
setUserToken :: User -> String -> User
setUserToken (User a7 a6 a5 a4 a3 a2 _ b1) a = User a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `LastLogin` of the `User` entity.
setUserLastLogin :: User -> Maybe Data.Time.ClockTime -> User
setUserLastLogin (User a8 a7 a6 a5 a4 a3 a2 _) a = User a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `User`.
userID :: UserID -> Database.CDBI.Criteria.Value UserID
userID (UserID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `User`.
userKeyToInt :: UserID -> Int
userKeyToInt (UserID key) = key

--- Shows the key of a `User` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showUserKey :: User -> String
showUserKey entry =
  Database.CDBI.ER.showDatabaseKey "User" userKeyToInt (userKey entry)

--- Transforms a string into a key of a `User` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readUserKey :: String -> Maybe UserID
readUserKey = Database.CDBI.ER.readDatabaseKey "User" UserID

--- Gets all `User` entities.
queryAllUsers :: Database.CDBI.Connection.DBAction [User]
queryAllUsers = Database.CDBI.ER.getAllEntries user_CDBI_Description

--- Gets all `User` entities satisfying a given predicate.
queryCondUser :: (User -> Bool) -> Database.CDBI.Connection.DBAction [User]
queryCondUser = Database.CDBI.ER.getCondEntries user_CDBI_Description

--- Gets a `User` entry by a given key.
getUser :: UserID -> Database.CDBI.Connection.DBAction User
getUser =
  Database.CDBI.ER.getEntryWithKey user_CDBI_Description userColumnKey userID

--- Inserts a new `User` entity.
newUser
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> Maybe Data.Time.ClockTime -> Database.CDBI.Connection.DBAction User
newUser name_p email_p publicEmail_p role_p password_p token_p lastLogin_p =
  Database.CDBI.ER.insertNewEntry user_CDBI_Description setUserKey UserID
   (User (UserID 0) name_p email_p publicEmail_p role_p password_p token_p
     lastLogin_p)

--- Deletes an existing `User` entry by its key.
deleteUser :: User -> Database.CDBI.Connection.DBAction ()
deleteUser =
  Database.CDBI.ER.deleteEntry user_CDBI_Description userColumnKey
   (userID . userKey)

--- Updates an existing `User` entry by its key.
updateUser :: User -> Database.CDBI.Connection.DBAction ()
updateUser = Database.CDBI.ER.updateEntry user_CDBI_Description

--- The ER description of the `Package` entity.
package_CDBI_Description :: Database.CDBI.Description.EntityDescription Package
package_CDBI_Description =
  Database.CDBI.Description.ED "Package"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeBool]
   (\(Package (PackageID key) name abandoned) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLBool abandoned])
   (\(Package _ name abandoned) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLBool abandoned])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLBool abandoned] ->
     Package (PackageID key) name abandoned)

--- The database table of the `Package` entity.
packageTable :: Database.CDBI.Description.Table
packageTable = "Package"

--- The database column `Key` of the `Package` entity.
packageColumnKey :: Database.CDBI.Description.Column PackageID
packageColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Package\".\"Key\""

--- The database column `Name` of the `Package` entity.
packageColumnName :: Database.CDBI.Description.Column String
packageColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Package\".\"Name\""

--- The database column `Abandoned` of the `Package` entity.
packageColumnAbandoned :: Database.CDBI.Description.Column Bool
packageColumnAbandoned =
  Database.CDBI.Description.Column "\"Abandoned\"" "\"Package\".\"Abandoned\""

--- The description of the database column `Key` of the `Package` entity.
packageKeyColDesc :: Database.CDBI.Description.ColumnDescription PackageID
packageKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Package\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(PackageID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> PackageID key)

--- The description of the database column `Name` of the `Package` entity.
packageNameColDesc :: Database.CDBI.Description.ColumnDescription String
packageNameColDesc =
  Database.CDBI.Description.ColDesc "\"Package\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Abandoned` of the `Package` entity.
packageAbandonedColDesc :: Database.CDBI.Description.ColumnDescription Bool
packageAbandonedColDesc =
  Database.CDBI.Description.ColDesc "\"Package\".\"Abandoned\""
   Database.CDBI.Connection.SQLTypeBool
   (\abandoned -> Database.CDBI.Connection.SQLBool abandoned)
   (\(Database.CDBI.Connection.SQLBool abandoned) -> abandoned)

--- Gets the attribute `Key` of the `Package` entity.
packageKey :: Package -> PackageID
packageKey (Package a _ _) = a

--- Gets the attribute `Name` of the `Package` entity.
packageName :: Package -> String
packageName (Package _ a _) = a

--- Gets the attribute `Abandoned` of the `Package` entity.
packageAbandoned :: Package -> Bool
packageAbandoned (Package _ _ a) = a

--- Sets the attribute `Key` of the `Package` entity.
setPackageKey :: Package -> PackageID -> Package
setPackageKey (Package _ b2 b1) a = Package a b2 b1

--- Sets the attribute `Name` of the `Package` entity.
setPackageName :: Package -> String -> Package
setPackageName (Package a2 _ b1) a = Package a2 a b1

--- Sets the attribute `Abandoned` of the `Package` entity.
setPackageAbandoned :: Package -> Bool -> Package
setPackageAbandoned (Package a3 a2 _) a = Package a3 a2 a

--- id-to-value function for entity `Package`.
packageID :: PackageID -> Database.CDBI.Criteria.Value PackageID
packageID (PackageID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Package`.
packageKeyToInt :: PackageID -> Int
packageKeyToInt (PackageID key) = key

--- Shows the key of a `Package` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showPackageKey :: Package -> String
showPackageKey entry =
  Database.CDBI.ER.showDatabaseKey "Package" packageKeyToInt (packageKey entry)

--- Transforms a string into a key of a `Package` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readPackageKey :: String -> Maybe PackageID
readPackageKey = Database.CDBI.ER.readDatabaseKey "Package" PackageID

--- Gets all `Package` entities.
queryAllPackages :: Database.CDBI.Connection.DBAction [Package]
queryAllPackages = Database.CDBI.ER.getAllEntries package_CDBI_Description

--- Gets all `Package` entities satisfying a given predicate.
queryCondPackage
  :: (Package -> Bool) -> Database.CDBI.Connection.DBAction [Package]
queryCondPackage = Database.CDBI.ER.getCondEntries package_CDBI_Description

--- Gets a `Package` entry by a given key.
getPackage :: PackageID -> Database.CDBI.Connection.DBAction Package
getPackage =
  Database.CDBI.ER.getEntryWithKey package_CDBI_Description packageColumnKey
   packageID

--- Inserts a new `Package` entity.
newPackage :: String -> Bool -> Database.CDBI.Connection.DBAction Package
newPackage name_p abandoned_p =
  Database.CDBI.ER.insertNewEntry package_CDBI_Description setPackageKey
   PackageID
   (Package (PackageID 0) name_p abandoned_p)

--- Deletes an existing `Package` entry by its key.
deletePackage :: Package -> Database.CDBI.Connection.DBAction ()
deletePackage =
  Database.CDBI.ER.deleteEntry package_CDBI_Description packageColumnKey
   (packageID . packageKey)

--- Updates an existing `Package` entry by its key.
updatePackage :: Package -> Database.CDBI.Connection.DBAction ()
updatePackage = Database.CDBI.ER.updateEntry package_CDBI_Description

--- The ER description of the `Version` entity.
version_CDBI_Description :: Database.CDBI.Description.EntityDescription Version
version_CDBI_Description =
  Database.CDBI.Description.ED "Version"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeDate
   ,Database.CDBI.Connection.SQLTypeBool
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Version
       (VersionID key)
       version
       published
       tested
       description
       jobStatus
       downloads
       uploadDate
       deprecated
       (PackageID packageVersioningKey)
       (UserID userUploadKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString version
     ,Database.CDBI.Connection.SQLBool published
     ,Database.CDBI.Connection.SQLBool tested
     ,Database.CDBI.Connection.SQLString description
     ,Database.CDBI.Connection.SQLString jobStatus
     ,Database.CDBI.Connection.SQLInt downloads
     ,Database.CDBI.Connection.SQLDate uploadDate
     ,Database.CDBI.Connection.SQLBool deprecated
     ,Database.CDBI.Connection.SQLInt packageVersioningKey
     ,Database.CDBI.Connection.SQLInt userUploadKey])
   (\(Version
       _
       version
       published
       tested
       description
       jobStatus
       downloads
       uploadDate
       deprecated
       (PackageID packageVersioningKey)
       (UserID userUploadKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString version
     ,Database.CDBI.Connection.SQLBool published
     ,Database.CDBI.Connection.SQLBool tested
     ,Database.CDBI.Connection.SQLString description
     ,Database.CDBI.Connection.SQLString jobStatus
     ,Database.CDBI.Connection.SQLInt downloads
     ,Database.CDBI.Connection.SQLDate uploadDate
     ,Database.CDBI.Connection.SQLBool deprecated
     ,Database.CDBI.Connection.SQLInt packageVersioningKey
     ,Database.CDBI.Connection.SQLInt userUploadKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString version
     ,Database.CDBI.Connection.SQLBool published
     ,Database.CDBI.Connection.SQLBool tested
     ,Database.CDBI.Connection.SQLString description
     ,Database.CDBI.Connection.SQLString jobStatus
     ,Database.CDBI.Connection.SQLInt downloads
     ,Database.CDBI.Connection.SQLDate uploadDate
     ,Database.CDBI.Connection.SQLBool deprecated
     ,Database.CDBI.Connection.SQLInt packageVersioningKey
     ,Database.CDBI.Connection.SQLInt userUploadKey] ->
     Version (VersionID key) version published tested description jobStatus
      downloads
      uploadDate
      deprecated
      (PackageID packageVersioningKey)
      (UserID userUploadKey))

--- The database table of the `Version` entity.
versionTable :: Database.CDBI.Description.Table
versionTable = "Version"

--- The database column `Key` of the `Version` entity.
versionColumnKey :: Database.CDBI.Description.Column VersionID
versionColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Version\".\"Key\""

--- The database column `Version` of the `Version` entity.
versionColumnVersion :: Database.CDBI.Description.Column String
versionColumnVersion =
  Database.CDBI.Description.Column "\"Version\"" "\"Version\".\"Version\""

--- The database column `Published` of the `Version` entity.
versionColumnPublished :: Database.CDBI.Description.Column Bool
versionColumnPublished =
  Database.CDBI.Description.Column "\"Published\"" "\"Version\".\"Published\""

--- The database column `Tested` of the `Version` entity.
versionColumnTested :: Database.CDBI.Description.Column Bool
versionColumnTested =
  Database.CDBI.Description.Column "\"Tested\"" "\"Version\".\"Tested\""

--- The database column `Description` of the `Version` entity.
versionColumnDescription :: Database.CDBI.Description.Column String
versionColumnDescription =
  Database.CDBI.Description.Column "\"Description\""
   "\"Version\".\"Description\""

--- The database column `JobStatus` of the `Version` entity.
versionColumnJobStatus :: Database.CDBI.Description.Column String
versionColumnJobStatus =
  Database.CDBI.Description.Column "\"JobStatus\"" "\"Version\".\"JobStatus\""

--- The database column `Downloads` of the `Version` entity.
versionColumnDownloads :: Database.CDBI.Description.Column Int
versionColumnDownloads =
  Database.CDBI.Description.Column "\"Downloads\"" "\"Version\".\"Downloads\""

--- The database column `UploadDate` of the `Version` entity.
versionColumnUploadDate :: Database.CDBI.Description.Column Data.Time.ClockTime
versionColumnUploadDate =
  Database.CDBI.Description.Column "\"UploadDate\"" "\"Version\".\"UploadDate\""

--- The database column `Deprecated` of the `Version` entity.
versionColumnDeprecated :: Database.CDBI.Description.Column Bool
versionColumnDeprecated =
  Database.CDBI.Description.Column "\"Deprecated\"" "\"Version\".\"Deprecated\""

--- The database column `PackageVersioningKey` of the `Version` entity.
versionColumnPackageVersioningKey :: Database.CDBI.Description.Column PackageID
versionColumnPackageVersioningKey =
  Database.CDBI.Description.Column "\"PackageVersioningKey\""
   "\"Version\".\"PackageVersioningKey\""

--- The database column `UserUploadKey` of the `Version` entity.
versionColumnUserUploadKey :: Database.CDBI.Description.Column UserID
versionColumnUserUploadKey =
  Database.CDBI.Description.Column "\"UserUploadKey\""
   "\"Version\".\"UserUploadKey\""

--- The description of the database column `Key` of the `Version` entity.
versionKeyColDesc :: Database.CDBI.Description.ColumnDescription VersionID
versionKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(VersionID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> VersionID key)

--- The description of the database column `Version` of the `Version` entity.
versionVersionColDesc :: Database.CDBI.Description.ColumnDescription String
versionVersionColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Version\""
   Database.CDBI.Connection.SQLTypeString
   (\version -> Database.CDBI.Connection.SQLString version)
   (\(Database.CDBI.Connection.SQLString version) -> version)

--- The description of the database column `Published` of the `Version` entity.
versionPublishedColDesc :: Database.CDBI.Description.ColumnDescription Bool
versionPublishedColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Published\""
   Database.CDBI.Connection.SQLTypeBool
   (\published -> Database.CDBI.Connection.SQLBool published)
   (\(Database.CDBI.Connection.SQLBool published) -> published)

--- The description of the database column `Tested` of the `Version` entity.
versionTestedColDesc :: Database.CDBI.Description.ColumnDescription Bool
versionTestedColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Tested\""
   Database.CDBI.Connection.SQLTypeBool
   (\tested -> Database.CDBI.Connection.SQLBool tested)
   (\(Database.CDBI.Connection.SQLBool tested) -> tested)

--- The description of the database column `Description` of the `Version` entity.
versionDescriptionColDesc :: Database.CDBI.Description.ColumnDescription String
versionDescriptionColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Description\""
   Database.CDBI.Connection.SQLTypeString
   (\description -> Database.CDBI.Connection.SQLString description)
   (\(Database.CDBI.Connection.SQLString description) -> description)

--- The description of the database column `JobStatus` of the `Version` entity.
versionJobStatusColDesc :: Database.CDBI.Description.ColumnDescription String
versionJobStatusColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"JobStatus\""
   Database.CDBI.Connection.SQLTypeString
   (\jobStatus -> Database.CDBI.Connection.SQLString jobStatus)
   (\(Database.CDBI.Connection.SQLString jobStatus) -> jobStatus)

--- The description of the database column `Downloads` of the `Version` entity.
versionDownloadsColDesc :: Database.CDBI.Description.ColumnDescription Int
versionDownloadsColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Downloads\""
   Database.CDBI.Connection.SQLTypeInt
   (\downloads -> Database.CDBI.Connection.SQLInt downloads)
   (\(Database.CDBI.Connection.SQLInt downloads) -> downloads)

--- The description of the database column `UploadDate` of the `Version` entity.
versionUploadDateColDesc
  :: Database.CDBI.Description.ColumnDescription Data.Time.ClockTime
versionUploadDateColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"UploadDate\""
   Database.CDBI.Connection.SQLTypeDate
   (\uploadDate -> Database.CDBI.Connection.SQLDate uploadDate)
   (\(Database.CDBI.Connection.SQLDate uploadDate) -> uploadDate)

--- The description of the database column `Deprecated` of the `Version` entity.
versionDeprecatedColDesc :: Database.CDBI.Description.ColumnDescription Bool
versionDeprecatedColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"Deprecated\""
   Database.CDBI.Connection.SQLTypeBool
   (\deprecated -> Database.CDBI.Connection.SQLBool deprecated)
   (\(Database.CDBI.Connection.SQLBool deprecated) -> deprecated)

--- The description of the database column `PackageVersioningKey` of the `Version` entity.
versionPackageVersioningKeyColDesc
  :: Database.CDBI.Description.ColumnDescription PackageID
versionPackageVersioningKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"PackageVersioningKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(PackageID packageVersioningKey) ->
     Database.CDBI.Connection.SQLInt packageVersioningKey)
   (\(Database.CDBI.Connection.SQLInt packageVersioningKey) ->
     PackageID packageVersioningKey)

--- The description of the database column `UserUploadKey` of the `Version` entity.
versionUserUploadKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
versionUserUploadKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Version\".\"UserUploadKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userUploadKey) -> Database.CDBI.Connection.SQLInt userUploadKey)
   (\(Database.CDBI.Connection.SQLInt userUploadKey) -> UserID userUploadKey)

--- Gets the attribute `Key` of the `Version` entity.
versionKey :: Version -> VersionID
versionKey (Version a _ _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Version` of the `Version` entity.
versionVersion :: Version -> String
versionVersion (Version _ a _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Published` of the `Version` entity.
versionPublished :: Version -> Bool
versionPublished (Version _ _ a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Tested` of the `Version` entity.
versionTested :: Version -> Bool
versionTested (Version _ _ _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Description` of the `Version` entity.
versionDescription :: Version -> String
versionDescription (Version _ _ _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `JobStatus` of the `Version` entity.
versionJobStatus :: Version -> String
versionJobStatus (Version _ _ _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Downloads` of the `Version` entity.
versionDownloads :: Version -> Int
versionDownloads (Version _ _ _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `UploadDate` of the `Version` entity.
versionUploadDate :: Version -> Data.Time.ClockTime
versionUploadDate (Version _ _ _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `Deprecated` of the `Version` entity.
versionDeprecated :: Version -> Bool
versionDeprecated (Version _ _ _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `PackageVersioningKey` of the `Version` entity.
versionPackageVersioningKey :: Version -> PackageID
versionPackageVersioningKey (Version _ _ _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `UserUploadKey` of the `Version` entity.
versionUserUploadKey :: Version -> UserID
versionUserUploadKey (Version _ _ _ _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `Version` entity.
setVersionKey :: Version -> VersionID -> Version
setVersionKey (Version _ b10 b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  Version a b10 b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Version` of the `Version` entity.
setVersionVersion :: Version -> String -> Version
setVersionVersion (Version a2 _ b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  Version a2 a b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Published` of the `Version` entity.
setVersionPublished :: Version -> Bool -> Version
setVersionPublished (Version a3 a2 _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  Version a3 a2 a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Tested` of the `Version` entity.
setVersionTested :: Version -> Bool -> Version
setVersionTested (Version a4 a3 a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  Version a4 a3 a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Description` of the `Version` entity.
setVersionDescription :: Version -> String -> Version
setVersionDescription (Version a5 a4 a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  Version a5 a4 a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `JobStatus` of the `Version` entity.
setVersionJobStatus :: Version -> String -> Version
setVersionJobStatus (Version a6 a5 a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  Version a6 a5 a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Downloads` of the `Version` entity.
setVersionDownloads :: Version -> Int -> Version
setVersionDownloads (Version a7 a6 a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  Version a7 a6 a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `UploadDate` of the `Version` entity.
setVersionUploadDate :: Version -> Data.Time.ClockTime -> Version
setVersionUploadDate (Version a8 a7 a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  Version a8 a7 a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `Deprecated` of the `Version` entity.
setVersionDeprecated :: Version -> Bool -> Version
setVersionDeprecated (Version a9 a8 a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  Version a9 a8 a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `PackageVersioningKey` of the `Version` entity.
setVersionPackageVersioningKey :: Version -> PackageID -> Version
setVersionPackageVersioningKey (Version a10 a9 a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  Version a10 a9 a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `UserUploadKey` of the `Version` entity.
setVersionUserUploadKey :: Version -> UserID -> Version
setVersionUserUploadKey (Version a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  Version a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `Version`.
versionID :: VersionID -> Database.CDBI.Criteria.Value VersionID
versionID (VersionID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Version`.
versionKeyToInt :: VersionID -> Int
versionKeyToInt (VersionID key) = key

--- Shows the key of a `Version` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showVersionKey :: Version -> String
showVersionKey entry =
  Database.CDBI.ER.showDatabaseKey "Version" versionKeyToInt (versionKey entry)

--- Transforms a string into a key of a `Version` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readVersionKey :: String -> Maybe VersionID
readVersionKey = Database.CDBI.ER.readDatabaseKey "Version" VersionID

--- Gets all `Version` entities.
queryAllVersions :: Database.CDBI.Connection.DBAction [Version]
queryAllVersions = Database.CDBI.ER.getAllEntries version_CDBI_Description

--- Gets all `Version` entities satisfying a given predicate.
queryCondVersion
  :: (Version -> Bool) -> Database.CDBI.Connection.DBAction [Version]
queryCondVersion = Database.CDBI.ER.getCondEntries version_CDBI_Description

--- Gets a `Version` entry by a given key.
getVersion :: VersionID -> Database.CDBI.Connection.DBAction Version
getVersion =
  Database.CDBI.ER.getEntryWithKey version_CDBI_Description versionColumnKey
   versionID

--- Inserts a new `Version` entity.
newVersionWithPackageVersioningKeyWithUserUploadKey
  :: String
  -> Bool
  -> Bool
  -> String
  -> String
  -> Int
  -> Data.Time.ClockTime
  -> Bool -> PackageID -> UserID -> Database.CDBI.Connection.DBAction Version
newVersionWithPackageVersioningKeyWithUserUploadKey
    version_p
    published_p
    tested_p
    description_p
    jobStatus_p
    downloads_p
    uploadDate_p
    deprecated_p
    packageVersioningKey_p
    userUploadKey_p =
  Database.CDBI.ER.insertNewEntry version_CDBI_Description setVersionKey
   VersionID
   (Version (VersionID 0) version_p published_p tested_p description_p
     jobStatus_p
     downloads_p
     uploadDate_p
     deprecated_p
     packageVersioningKey_p
     userUploadKey_p)

--- Deletes an existing `Version` entry by its key.
deleteVersion :: Version -> Database.CDBI.Connection.DBAction ()
deleteVersion =
  Database.CDBI.ER.deleteEntry version_CDBI_Description versionColumnKey
   (versionID . versionKey)

--- Updates an existing `Version` entry by its key.
updateVersion :: Version -> Database.CDBI.Connection.DBAction ()
updateVersion = Database.CDBI.ER.updateEntry version_CDBI_Description

--- The ER description of the `Category` entity.
category_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Category
category_CDBI_Description =
  Database.CDBI.Description.ED "Category"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString]
   (\(Category (CategoryID key) name description) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString description])
   (\(Category _ name description) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString description])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString description] ->
     Category (CategoryID key) name description)

--- The database table of the `Category` entity.
categoryTable :: Database.CDBI.Description.Table
categoryTable = "Category"

--- The database column `Key` of the `Category` entity.
categoryColumnKey :: Database.CDBI.Description.Column CategoryID
categoryColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Category\".\"Key\""

--- The database column `Name` of the `Category` entity.
categoryColumnName :: Database.CDBI.Description.Column String
categoryColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Category\".\"Name\""

--- The database column `Description` of the `Category` entity.
categoryColumnDescription :: Database.CDBI.Description.Column String
categoryColumnDescription =
  Database.CDBI.Description.Column "\"Description\""
   "\"Category\".\"Description\""

--- The description of the database column `Key` of the `Category` entity.
categoryKeyColDesc :: Database.CDBI.Description.ColumnDescription CategoryID
categoryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> CategoryID key)

--- The description of the database column `Name` of the `Category` entity.
categoryNameColDesc :: Database.CDBI.Description.ColumnDescription String
categoryNameColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Description` of the `Category` entity.
categoryDescriptionColDesc :: Database.CDBI.Description.ColumnDescription String
categoryDescriptionColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Description\""
   Database.CDBI.Connection.SQLTypeString
   (\description -> Database.CDBI.Connection.SQLString description)
   (\(Database.CDBI.Connection.SQLString description) -> description)

--- Gets the attribute `Key` of the `Category` entity.
categoryKey :: Category -> CategoryID
categoryKey (Category a _ _) = a

--- Gets the attribute `Name` of the `Category` entity.
categoryName :: Category -> String
categoryName (Category _ a _) = a

--- Gets the attribute `Description` of the `Category` entity.
categoryDescription :: Category -> String
categoryDescription (Category _ _ a) = a

--- Sets the attribute `Key` of the `Category` entity.
setCategoryKey :: Category -> CategoryID -> Category
setCategoryKey (Category _ b2 b1) a = Category a b2 b1

--- Sets the attribute `Name` of the `Category` entity.
setCategoryName :: Category -> String -> Category
setCategoryName (Category a2 _ b1) a = Category a2 a b1

--- Sets the attribute `Description` of the `Category` entity.
setCategoryDescription :: Category -> String -> Category
setCategoryDescription (Category a3 a2 _) a = Category a3 a2 a

--- id-to-value function for entity `Category`.
categoryID :: CategoryID -> Database.CDBI.Criteria.Value CategoryID
categoryID (CategoryID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Category`.
categoryKeyToInt :: CategoryID -> Int
categoryKeyToInt (CategoryID key) = key

--- Shows the key of a `Category` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCategoryKey :: Category -> String
showCategoryKey entry =
  Database.CDBI.ER.showDatabaseKey "Category" categoryKeyToInt
   (categoryKey entry)

--- Transforms a string into a key of a `Category` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readCategoryKey :: String -> Maybe CategoryID
readCategoryKey = Database.CDBI.ER.readDatabaseKey "Category" CategoryID

--- Gets all `Category` entities.
queryAllCategorys :: Database.CDBI.Connection.DBAction [Category]
queryAllCategorys = Database.CDBI.ER.getAllEntries category_CDBI_Description

--- Gets all `Category` entities satisfying a given predicate.
queryCondCategory
  :: (Category -> Bool) -> Database.CDBI.Connection.DBAction [Category]
queryCondCategory = Database.CDBI.ER.getCondEntries category_CDBI_Description

--- Gets a `Category` entry by a given key.
getCategory :: CategoryID -> Database.CDBI.Connection.DBAction Category
getCategory =
  Database.CDBI.ER.getEntryWithKey category_CDBI_Description categoryColumnKey
   categoryID

--- Inserts a new `Category` entity.
newCategory :: String -> String -> Database.CDBI.Connection.DBAction Category
newCategory name_p description_p =
  Database.CDBI.ER.insertNewEntry category_CDBI_Description setCategoryKey
   CategoryID
   (Category (CategoryID 0) name_p description_p)

--- Deletes an existing `Category` entry by its key.
deleteCategory :: Category -> Database.CDBI.Connection.DBAction ()
deleteCategory =
  Database.CDBI.ER.deleteEntry category_CDBI_Description categoryColumnKey
   (categoryID . categoryKey)

--- Updates an existing `Category` entry by its key.
updateCategory :: Category -> Database.CDBI.Connection.DBAction ()
updateCategory = Database.CDBI.ER.updateEntry category_CDBI_Description

--- The ER description of the `CurryModule` entity.
curryModule_CDBI_Description
  :: Database.CDBI.Description.EntityDescription CurryModule
curryModule_CDBI_Description =
  Database.CDBI.Description.ED "CurryModule"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeString]
   (\(CurryModule (CurryModuleID key) name) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name])
   (\(CurryModule _ name) ->
     [Database.CDBI.Connection.SQLNull,Database.CDBI.Connection.SQLString name])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name] ->
     CurryModule (CurryModuleID key) name)

--- The database table of the `CurryModule` entity.
curryModuleTable :: Database.CDBI.Description.Table
curryModuleTable = "CurryModule"

--- The database column `Key` of the `CurryModule` entity.
curryModuleColumnKey :: Database.CDBI.Description.Column CurryModuleID
curryModuleColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"CurryModule\".\"Key\""

--- The database column `Name` of the `CurryModule` entity.
curryModuleColumnName :: Database.CDBI.Description.Column String
curryModuleColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"CurryModule\".\"Name\""

--- The description of the database column `Key` of the `CurryModule` entity.
curryModuleKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CurryModuleID
curryModuleKeyColDesc =
  Database.CDBI.Description.ColDesc "\"CurryModule\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CurryModuleID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> CurryModuleID key)

--- The description of the database column `Name` of the `CurryModule` entity.
curryModuleNameColDesc :: Database.CDBI.Description.ColumnDescription String
curryModuleNameColDesc =
  Database.CDBI.Description.ColDesc "\"CurryModule\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- Gets the attribute `Key` of the `CurryModule` entity.
curryModuleKey :: CurryModule -> CurryModuleID
curryModuleKey (CurryModule a _) = a

--- Gets the attribute `Name` of the `CurryModule` entity.
curryModuleName :: CurryModule -> String
curryModuleName (CurryModule _ a) = a

--- Sets the attribute `Key` of the `CurryModule` entity.
setCurryModuleKey :: CurryModule -> CurryModuleID -> CurryModule
setCurryModuleKey (CurryModule _ b1) a = CurryModule a b1

--- Sets the attribute `Name` of the `CurryModule` entity.
setCurryModuleName :: CurryModule -> String -> CurryModule
setCurryModuleName (CurryModule a2 _) a = CurryModule a2 a

--- id-to-value function for entity `CurryModule`.
curryModuleID :: CurryModuleID -> Database.CDBI.Criteria.Value CurryModuleID
curryModuleID (CurryModuleID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `CurryModule`.
curryModuleKeyToInt :: CurryModuleID -> Int
curryModuleKeyToInt (CurryModuleID key) = key

--- Shows the key of a `CurryModule` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCurryModuleKey :: CurryModule -> String
showCurryModuleKey entry =
  Database.CDBI.ER.showDatabaseKey "CurryModule" curryModuleKeyToInt
   (curryModuleKey entry)

--- Transforms a string into a key of a `CurryModule` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readCurryModuleKey :: String -> Maybe CurryModuleID
readCurryModuleKey =
  Database.CDBI.ER.readDatabaseKey "CurryModule" CurryModuleID

--- Gets all `CurryModule` entities.
queryAllCurryModules :: Database.CDBI.Connection.DBAction [CurryModule]
queryAllCurryModules =
  Database.CDBI.ER.getAllEntries curryModule_CDBI_Description

--- Gets all `CurryModule` entities satisfying a given predicate.
queryCondCurryModule
  :: (CurryModule -> Bool) -> Database.CDBI.Connection.DBAction [CurryModule]
queryCondCurryModule =
  Database.CDBI.ER.getCondEntries curryModule_CDBI_Description

--- Gets a `CurryModule` entry by a given key.
getCurryModule :: CurryModuleID -> Database.CDBI.Connection.DBAction CurryModule
getCurryModule =
  Database.CDBI.ER.getEntryWithKey curryModule_CDBI_Description
   curryModuleColumnKey
   curryModuleID

--- Inserts a new `CurryModule` entity.
newCurryModule :: String -> Database.CDBI.Connection.DBAction CurryModule
newCurryModule name_p =
  Database.CDBI.ER.insertNewEntry curryModule_CDBI_Description setCurryModuleKey
   CurryModuleID
   (CurryModule (CurryModuleID 0) name_p)

--- Deletes an existing `CurryModule` entry by its key.
deleteCurryModule :: CurryModule -> Database.CDBI.Connection.DBAction ()
deleteCurryModule =
  Database.CDBI.ER.deleteEntry curryModule_CDBI_Description curryModuleColumnKey
   (curryModuleID . curryModuleKey)

--- Updates an existing `CurryModule` entry by its key.
updateCurryModule :: CurryModule -> Database.CDBI.Connection.DBAction ()
updateCurryModule = Database.CDBI.ER.updateEntry curryModule_CDBI_Description

--- The ER description of the `ValidationToken` entity.
validationToken_CDBI_Description
  :: Database.CDBI.Description.EntityDescription ValidationToken
validationToken_CDBI_Description =
  Database.CDBI.Description.ED "ValidationToken"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeDate
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(ValidationToken
       (ValidationTokenID key) token validSince (UserID userValidatingKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString token
     ,Database.CDBI.Connection.SQLDate validSince
     ,Database.CDBI.Connection.SQLInt userValidatingKey])
   (\(ValidationToken _ token validSince (UserID userValidatingKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString token
     ,Database.CDBI.Connection.SQLDate validSince
     ,Database.CDBI.Connection.SQLInt userValidatingKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString token
     ,Database.CDBI.Connection.SQLDate validSince
     ,Database.CDBI.Connection.SQLInt userValidatingKey] ->
     ValidationToken (ValidationTokenID key) token validSince
      (UserID userValidatingKey))

--- The database table of the `ValidationToken` entity.
validationTokenTable :: Database.CDBI.Description.Table
validationTokenTable = "ValidationToken"

--- The database column `Key` of the `ValidationToken` entity.
validationTokenColumnKey :: Database.CDBI.Description.Column ValidationTokenID
validationTokenColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"ValidationToken\".\"Key\""

--- The database column `Token` of the `ValidationToken` entity.
validationTokenColumnToken :: Database.CDBI.Description.Column String
validationTokenColumnToken =
  Database.CDBI.Description.Column "\"Token\"" "\"ValidationToken\".\"Token\""

--- The database column `ValidSince` of the `ValidationToken` entity.
validationTokenColumnValidSince
  :: Database.CDBI.Description.Column Data.Time.ClockTime
validationTokenColumnValidSince =
  Database.CDBI.Description.Column "\"ValidSince\""
   "\"ValidationToken\".\"ValidSince\""

--- The database column `UserValidatingKey` of the `ValidationToken` entity.
validationTokenColumnUserValidatingKey
  :: Database.CDBI.Description.Column UserID
validationTokenColumnUserValidatingKey =
  Database.CDBI.Description.Column "\"UserValidatingKey\""
   "\"ValidationToken\".\"UserValidatingKey\""

--- The description of the database column `Key` of the `ValidationToken` entity.
validationTokenKeyColDesc
  :: Database.CDBI.Description.ColumnDescription ValidationTokenID
validationTokenKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ValidationToken\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(ValidationTokenID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> ValidationTokenID key)

--- The description of the database column `Token` of the `ValidationToken` entity.
validationTokenTokenColDesc
  :: Database.CDBI.Description.ColumnDescription String
validationTokenTokenColDesc =
  Database.CDBI.Description.ColDesc "\"ValidationToken\".\"Token\""
   Database.CDBI.Connection.SQLTypeString
   (\token -> Database.CDBI.Connection.SQLString token)
   (\(Database.CDBI.Connection.SQLString token) -> token)

--- The description of the database column `ValidSince` of the `ValidationToken` entity.
validationTokenValidSinceColDesc
  :: Database.CDBI.Description.ColumnDescription Data.Time.ClockTime
validationTokenValidSinceColDesc =
  Database.CDBI.Description.ColDesc "\"ValidationToken\".\"ValidSince\""
   Database.CDBI.Connection.SQLTypeDate
   (\validSince -> Database.CDBI.Connection.SQLDate validSince)
   (\(Database.CDBI.Connection.SQLDate validSince) -> validSince)

--- The description of the database column `UserValidatingKey` of the `ValidationToken` entity.
validationTokenUserValidatingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription UserID
validationTokenUserValidatingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"ValidationToken\".\"UserValidatingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(UserID userValidatingKey) ->
     Database.CDBI.Connection.SQLInt userValidatingKey)
   (\(Database.CDBI.Connection.SQLInt userValidatingKey) ->
     UserID userValidatingKey)

--- Gets the attribute `Key` of the `ValidationToken` entity.
validationTokenKey :: ValidationToken -> ValidationTokenID
validationTokenKey (ValidationToken a _ _ _) = a

--- Gets the attribute `Token` of the `ValidationToken` entity.
validationTokenToken :: ValidationToken -> String
validationTokenToken (ValidationToken _ a _ _) = a

--- Gets the attribute `ValidSince` of the `ValidationToken` entity.
validationTokenValidSince :: ValidationToken -> Data.Time.ClockTime
validationTokenValidSince (ValidationToken _ _ a _) = a

--- Gets the attribute `UserValidatingKey` of the `ValidationToken` entity.
validationTokenUserValidatingKey :: ValidationToken -> UserID
validationTokenUserValidatingKey (ValidationToken _ _ _ a) = a

--- Sets the attribute `Key` of the `ValidationToken` entity.
setValidationTokenKey :: ValidationToken -> ValidationTokenID -> ValidationToken
setValidationTokenKey (ValidationToken _ b3 b2 b1) a =
  ValidationToken a b3 b2 b1

--- Sets the attribute `Token` of the `ValidationToken` entity.
setValidationTokenToken :: ValidationToken -> String -> ValidationToken
setValidationTokenToken (ValidationToken a2 _ b2 b1) a =
  ValidationToken a2 a b2 b1

--- Sets the attribute `ValidSince` of the `ValidationToken` entity.
setValidationTokenValidSince
  :: ValidationToken -> Data.Time.ClockTime -> ValidationToken
setValidationTokenValidSince (ValidationToken a3 a2 _ b1) a =
  ValidationToken a3 a2 a b1

--- Sets the attribute `UserValidatingKey` of the `ValidationToken` entity.
setValidationTokenUserValidatingKey
  :: ValidationToken -> UserID -> ValidationToken
setValidationTokenUserValidatingKey (ValidationToken a4 a3 a2 _) a =
  ValidationToken a4 a3 a2 a

--- id-to-value function for entity `ValidationToken`.
validationTokenID
  :: ValidationTokenID -> Database.CDBI.Criteria.Value ValidationTokenID
validationTokenID (ValidationTokenID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `ValidationToken`.
validationTokenKeyToInt :: ValidationTokenID -> Int
validationTokenKeyToInt (ValidationTokenID key) = key

--- Shows the key of a `ValidationToken` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showValidationTokenKey :: ValidationToken -> String
showValidationTokenKey entry =
  Database.CDBI.ER.showDatabaseKey "ValidationToken" validationTokenKeyToInt
   (validationTokenKey entry)

--- Transforms a string into a key of a `ValidationToken` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readValidationTokenKey :: String -> Maybe ValidationTokenID
readValidationTokenKey =
  Database.CDBI.ER.readDatabaseKey "ValidationToken" ValidationTokenID

--- Gets all `ValidationToken` entities.
queryAllValidationTokens :: Database.CDBI.Connection.DBAction [ValidationToken]
queryAllValidationTokens =
  Database.CDBI.ER.getAllEntries validationToken_CDBI_Description

--- Gets all `ValidationToken` entities satisfying a given predicate.
queryCondValidationToken
  :: (ValidationToken -> Bool)
  -> Database.CDBI.Connection.DBAction [ValidationToken]
queryCondValidationToken =
  Database.CDBI.ER.getCondEntries validationToken_CDBI_Description

--- Gets a `ValidationToken` entry by a given key.
getValidationToken
  :: ValidationTokenID -> Database.CDBI.Connection.DBAction ValidationToken
getValidationToken =
  Database.CDBI.ER.getEntryWithKey validationToken_CDBI_Description
   validationTokenColumnKey
   validationTokenID

--- Inserts a new `ValidationToken` entity.
newValidationTokenWithUserValidatingKey
  :: String
  -> Data.Time.ClockTime
  -> UserID -> Database.CDBI.Connection.DBAction ValidationToken
newValidationTokenWithUserValidatingKey
    token_p validSince_p userValidatingKey_p =
  Database.CDBI.ER.insertNewEntry validationToken_CDBI_Description
   setValidationTokenKey
   ValidationTokenID
   (ValidationToken (ValidationTokenID 0) token_p validSince_p
     userValidatingKey_p)

--- Deletes an existing `ValidationToken` entry by its key.
deleteValidationToken :: ValidationToken -> Database.CDBI.Connection.DBAction ()
deleteValidationToken =
  Database.CDBI.ER.deleteEntry validationToken_CDBI_Description
   validationTokenColumnKey
   (validationTokenID . validationTokenKey)

--- Updates an existing `ValidationToken` entry by its key.
updateValidationToken :: ValidationToken -> Database.CDBI.Connection.DBAction ()
updateValidationToken =
  Database.CDBI.ER.updateEntry validationToken_CDBI_Description

--- Generates a new database (name provided as the parameter) and
--- creates its schema.
createNewDB :: String -> IO ()
createNewDB dbfile =
  do conn <- Database.CDBI.Connection.connectSQLite dbfile
     Database.CDBI.Connection.writeConnection cstr conn
     Database.CDBI.Connection.disconnect conn
  where
    cstr =
      unlines
       ["create table 'Categorizes'('CategoryCategorizesKey' int REFERENCES 'Category'(Key) not null ,'VersionCategorizesKey' int REFERENCES 'Version'(Key) not null, primary key ('CategoryCategorizesKey', 'VersionCategorizesKey'));"
       ,"create table 'Maintainer'('UserMaintainerKey' int REFERENCES 'User'(Key) not null ,'PackageMaintainerKey' int REFERENCES 'Package'(Key) not null, primary key ('UserMaintainerKey', 'PackageMaintainerKey'));"
       ,"create table 'Depending'('VersionDependingKey' int REFERENCES 'Version'(Key) not null ,'PackageDependingKey' int REFERENCES 'Package'(Key) not null, primary key ('VersionDependingKey', 'PackageDependingKey'));"
       ,"create table 'Exporting'('VersionExportingKey' int REFERENCES 'Version'(Key) not null ,'CurryModuleExportingKey' int REFERENCES 'CurryModule'(Key) not null, primary key ('VersionExportingKey', 'CurryModuleExportingKey'));"
       ,"create table 'Watching'('UserWatchingKey' int REFERENCES 'User'(Key) not null ,'PackageWatchingKey' int REFERENCES 'Package'(Key) not null, primary key ('UserWatchingKey', 'PackageWatchingKey'));"
       ,"create table 'User'('Key' integer primary key ,'Name' string unique not null ,'Email' string unique not null ,'PublicEmail' string not null ,'Role' string not null ,'Password' string not null ,'Token' string ,'LastLogin' string);"
       ,"create table 'Package'('Key' integer primary key ,'Name' string unique not null ,'Abandoned' string not null);"
       ,"create table 'Version'('Key' integer primary key ,'Version' string not null ,'Published' string not null ,'Tested' string not null ,'Description' string not null ,'JobStatus' string not null ,'Downloads' not null ,'UploadDate' string not null ,'Deprecated' string not null ,'PackageVersioningKey' int REFERENCES 'Package'(Key) not null ,'UserUploadKey' int REFERENCES 'User'(Key) not null);"
       ,"create table 'Category'('Key' integer primary key ,'Name' string unique not null ,'Description' string not null);"
       ,"create table 'CurryModule'('Key' integer primary key ,'Name' string unique not null);"
       ,"create table 'ValidationToken'('Key' integer primary key ,'Token' string unique not null ,'ValidSince' string not null ,'UserValidatingKey' int REFERENCES 'User'(Key) unique not null);"]

--- Saves complete database as term files into an existing directory
--- provided as a parameter.
saveDBTo :: String -> IO ()
saveDBTo dir =
  do Database.CDBI.ER.saveDBTerms categorizes_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms maintainer_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms depending_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms exporting_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms watching_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms user_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms package_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms version_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms category_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms curryModule_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms validationToken_CDBI_Description sqliteDBFile
      dir

--- Restores complete database from term files which are stored
--- in a directory provided as a parameter.
restoreDBFrom :: String -> IO ()
restoreDBFrom dir =
  do Database.CDBI.ER.restoreDBTerms categorizes_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms maintainer_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms depending_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms exporting_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms watching_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms user_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms package_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms version_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms category_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms curryModule_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms validationToken_CDBI_Description
      sqliteDBFile
      dir

--- Runs a DB action (typically a query).
runQ :: Database.CDBI.Connection.DBAction a -> IO a
runQ = Database.CDBI.ER.runQueryOnDB sqliteDBFile

--- Runs a DB action as a transaction.
runT
  :: Database.CDBI.Connection.DBAction a
  -> IO (Database.CDBI.Connection.SQLResult a)
runT = Database.CDBI.ER.runTransactionOnDB sqliteDBFile

--- Runs a DB action as a transaction. Emits an error in case of failure.
runJustT :: Database.CDBI.Connection.DBAction a -> IO a
runJustT = Database.CDBI.ER.runJustTransactionOnDB sqliteDBFile
