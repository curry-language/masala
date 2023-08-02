{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode --optF=-o #-}

--- Some SQL queries useful for the implementation of Masala.

module Model.Queries where

import Data.List  ( nub )
import Data.Maybe ( listToMaybe, isNothing )

import Database.CDBI.ER

import Model.Masala2

import Data.Time

import Config.Roles

import System.PreludeHelpers

-----------------------------------------------------------------------
-- Checks whether the given ValidationToken is available, i.e.,
-- not already used.
checkValidationTokenAvailable :: String -> IO Bool
checkValidationTokenAvailable token = fmap null $ runQ
  ``sql* Select *
         From ValidationToken as vt
         Where vt.Token = { token };''

-- Gets the corresponding ValidationToken for the given String.
getValidationTokenWithToken :: String -> IO (Maybe ValidationToken)
getValidationTokenWithToken token = fmap listToMaybe $ runQ
  ``sql* Select *
         From ValidationToken as vt
         Where vt.Token = { token };''

-- Deletes the corresponding ValidationToken for the given String.
deleteValidationTokenWithToken :: String -> IO ()
deleteValidationTokenWithToken token = runQ
  ``sql* Delete From ValidationToken Where Token = { token };''

-----------------------------------------------------------------------
-- Checks whether the given user login name is available, i.e.,
-- not already used.
checkUserNameAvailable :: String -> IO Bool
checkUserNameAvailable = (fmap isNothing) . getUserByName

-- Checks whether the given public user name is available, i.e.,
-- not already used.
checkPublicNameAvailable :: String -> IO Bool
checkPublicNameAvailable publicname = fmap null $ runQ
  ``sql* Select *
         From User as u
         Where u.PublicName = { publicname };''

-- Checks whether the given email is available, i.e., not already used.
checkEmailAvailable :: String -> IO Bool
checkEmailAvailable email = fmap null $ runQ
  ``sql* Select *
         From User as u
         Where u.Email = { email };''

-- Gets the User with the given LoginName and Password, i.e.,
-- only returns a User if the LoginName and Password match with
-- the ones in the database.
getUserByLoginData :: String -> String -> IO (Maybe User)
getUserByLoginData loginName password = fmap listToMaybe $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { loginName } And u.Password = { password }
               And u.Role != { roleInvalid };''

-- Gets a User with the given LoginName.
getUserByName :: String -> IO (Maybe User)
getUserByName loginName = fmap listToMaybe $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { loginName };''

-- Gets a User by the given String matching either the
-- LoginName or Email.
getUserByNameOrEmail :: String -> IO (Maybe User)
getUserByNameOrEmail login = fmap listToMaybe $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { login } Or u.Email = { login };''

-- Checks whether the given User is watching the given Package.
checkUserWatches :: User -> Package -> IO Bool
checkUserWatches user pkg = fmap (not . null) $ runQ
  ``sql* Select *
         From User as u, Package as p
         Where u.Key = { userKey user } And p.Key = { packageKey pkg } And Satisfies u watches p;''

-- Checks whether the given User is maintaining the given Package.
checkUserMaintains :: User -> Package -> IO Bool
checkUserMaintains user pkg = fmap (not . null) $ runQ
  ``sql* Select *
         From User as u, Package as p
         Where u.Key = { userKey user } And p.Key = { packageKey pkg } And Satisfies u maintains p;''

-- Gets the ValidationToken connected to the given User.
getUserValidationToken :: User -> IO (Maybe ValidationToken)
getUserValidationToken user = fmap (fmap fst) $ fmap listToMaybe $ runQ
  ``sql* Select *
         From ValidationToken as vt, User as u
         Where u.Key = { userKey user } And Satisfies vt validates u;''

-- Gets all Users that are watching the given Package.
getWatchingUsers :: Package -> IO [User]
getWatchingUsers pkg = fmap (fmap fst) $ runQ
  ``sql* Select *
         From User as u, Package as p
         Where p.Key = { packageKey pkg } And Satisfies u watches p;''

-----------------------------------------------------------------------
-- Gets the version of a package with a given name and version string.
getPackageWithNameAction :: String -> DBAction (Maybe Package)
getPackageWithNameAction pname = fmap listToMaybe $ 
  ``sql* Select *
         From Package as p
         Where p.Name = { pname };''

-- Gets the version of a package with a given name and version string.
getPackageWithName :: String -> IO (Maybe Package)
getPackageWithName = runQ . getPackageWithNameAction

-- Gets all Packages and missing PackageNames from the given list.
getDependenciesWithNameAction :: [String] -> DBAction [Either String Package]
getDependenciesWithNameAction = mapM (taggedDBAction getPackageWithNameAction)

-- Gets all Packages and missing PackageNames from the given list.
getDependenciesWithName :: [String] -> IO [Either String Package]
getDependenciesWithName = runQ . getDependenciesWithNameAction

-- Gets the version of a package with a given name and version string.
getPackageVersionByNameAction :: String -> String -> DBAction (Maybe Version)
getPackageVersionByNameAction pname vers = fmap (listToMaybe . map snd) $ 
  ``sql* Select *
         From Package as p, Version as v
         Where p.Name = { pname } And
               v.Version = { vers } And
               Satisfies v versionOf p;''

-- Gets the version of a package with a given name and version string.
getPackageVersionByName :: String -> String -> IO (Maybe Version)
getPackageVersionByName pname vers =
  runQ $ getPackageVersionByNameAction pname vers

-- Gets the number of Maintainers of the given Package.
getNumberOfMaintainers :: Package -> IO Int
getNumberOfMaintainers = fmap length . runQ . queryMaintainersOfPackage

-- Gets the Maintainers of the given Package.
getMaintainersOfPackage :: Package -> IO [User]
getMaintainersOfPackage = runQ . queryMaintainersOfPackage

-- Gets the Maintainers of the given Package.
queryMaintainersOfPackage :: Package -> DBAction [User]
queryMaintainersOfPackage package = fmap (map fst) $
  ``sql* Select *
         From User as u, Package as p
         Where p.Key = { packageKey package } And
               Satisfies u maintains p;''

--- Queries the users watching a package.
queryWatchersOfPackage :: Package -> DBAction [User]
queryWatchersOfPackage package = fmap (map fst) $
  ``sql* Select *
         From User as u, Package as p
         Where p.Key = { packageKey package } And
               Satisfies u watches p;''

-- Gets the Users that are not maintaining the given Package.
getNonMaintainersOfPackage :: Package -> IO [User]
getNonMaintainersOfPackage package = fmap (map fst) $ runQ
  ``sql* Select *
         From User as u, Package as p
         Where p.Key = { packageKey package } And
               Not Satisfies u maintains p;''

-- Checks whether the given User is a Maintainer of the given Package.
checkIfMaintainerAction :: Package -> User -> DBAction Bool
checkIfMaintainerAction package user = fmap (not . null) $
  ``sql* Select *
         From Package as p, User as u
         Where p.Key = { packageKey package } And
               u.Key = { userKey user } And
               Satisfies u maintains p;''

-- Checks whether the given User is a Maintainer of the given Package.
checkIfMaintainer :: Package -> User -> IO Bool
checkIfMaintainer package user = runQ $ checkIfMaintainerAction package user

-----------------------------------------------------------------------
-- Gets the versions of a given package.
getPackageVersions :: Package -> IO [Version]
getPackageVersions pkg = fmap (map snd) $ runQ
  ``sql* Select *
         From Package as p, Version as v
         Where p.Name = { packageName pkg } And
               Satisfies v versionOf p;''

-----------------------------------------------------------------------
-- Gets all private versions of all packages.
getUnpublishedVersions :: IO [Version]
getUnpublishedVersions = runQ
  ``sql* Select *
         From Version as v
         Where v.Published = FALSE;''

-----------------------------------------------------------------------
-- Gets the categories of a given package version.
getPackageVersionCategories :: Package -> Version -> DBAction [Category]
getPackageVersionCategories pkg vers = fmap (map (\(_,_,c) -> c)) $
  ``sql* Select *
         From Package as p, Version as v, Category as c
         Where p.Name = { packageName pkg } And
               Satisfies v versionOf p And
               v.Version = { versionVersion vers } And
               Satisfies v withCategory c;''

-----------------------------------------------------------------------
-- Gets the maintainers of a given package.
getPackageMaintainers :: Package -> IO [User]
getPackageMaintainers pkg = fmap (map snd) $ runQ
  ``sql* Select *
         From Package as p, User as u
         Where p.Name = { packageName pkg } And
               Satisfies p maintainedBy u;''

-----------------------------------------------------------------------
-- Gets a category with a given name.
getCategoryWithNameAction :: String -> DBAction (Maybe Category)
getCategoryWithNameAction cname = fmap listToMaybe $
  ``sql* Select * From Category as c
         Where c.Name = { cname };''

-- Gets a category with a given name.
getCategoryWithName :: String -> IO (Maybe Category)
getCategoryWithName = runQ . getCategoryWithNameAction

-- Gets all Categories and missing CategoryNames from the given list.
getCategoriesWithNameAction :: [String] -> DBAction [Either String Category]
getCategoriesWithNameAction = mapM (taggedDBAction getCategoryWithNameAction)

-- Gets all Categories and missing CategoryNames from the given list.
getCategoriesWithName :: [String] -> IO [Either String Category]
getCategoriesWithName = runQ . getCategoriesWithNameAction

-- Gets all packages in a category with a given name.
getPackagesOfCategory :: Category -> IO [Package]
getPackagesOfCategory cat = fmap (nub . map (\(_,_,c) -> c)) $ runQ
  ``sql* Select *
         From Category as c, Version as v, Package as p
         Where c.Name = { categoryName cat } And
               Satisfies v withCategory c And
               Satisfies v versionOf p;''

-----------------------------------------------------------------------
-- Gets a Curry module with a given name.
getCurryModuleWithNameAction :: String -> DBAction (Maybe CurryModule)
getCurryModuleWithNameAction mname = fmap listToMaybe $
  ``sql* Select * From CurryModule as m
         Where m.Name = { mname };''

-- Gets a Curry module with a given name.
getCurryModuleWithName :: String -> IO (Maybe CurryModule)
getCurryModuleWithName = runQ . getCurryModuleWithNameAction

-- Gets the package versions exporting a Curry module with a given name.
getPackageVersionsWithCurryModuleName :: String -> IO [(String,String)]
getPackageVersionsWithCurryModuleName mname = runQ
  ``sql* Select p.Name, v.Version
         From CurryModule as m, Version as v, Package as p
         Where m.Name = { mname } And
               Satisfies v exports m And
               Satisfies v versionOf p;''

-- Gets the Curry module together with their exported package versions
-- whose name contains the given string.
getCurryModulePattern :: String -> IO [(String,String,String)]
getCurryModulePattern mname = runQ
  ``sql* Select m.Name, p.Name, v.Version
         From CurryModule as m, Version as v, Package as p
         Where m.Name Like { "%" ++ mname ++ "%" } And
               Satisfies v exports m And
               Satisfies v versionOf p;''

-- Gets package versions containing a given string in the name or description.
getPackageVersionsByPattern :: String -> IO [(String,String)]
getPackageVersionsByPattern s = runQ
  ``sql* Select p.Name, v.Version
         From Version as v, Package as p
         Where (v.Description Like { "%" ++ s ++ "%" } Or
                p.Name Like { "%" ++ s ++ "%" } )
               And Satisfies v versionOf p;''

-----------------------------------------------------------------------
--- Gets the associated User entity for a given Version entity.
getUploadUser :: Version -> DBAction User
getUploadUser vUser = getUser (versionUserUploadKey vUser)

--- Gets the associated Package entity for a given Version entity.
getVersioningPackage :: Version -> DBAction Package
getVersioningPackage vPackage =
  getPackage (versionPackageVersioningKey vPackage)

-----------------------------------------------------------------------
