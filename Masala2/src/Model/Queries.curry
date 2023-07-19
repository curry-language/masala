{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

--- Some SQL queries useful for the implementation of Masala.

module Model.Queries where

import Data.List  ( nub )
import Data.Maybe ( listToMaybe )

import Database.CDBI.ER

import Model.Masala2

import Data.Time

import Config.Roles

import System.PreludeHelpers

-----------------------------------------------------------------------
-- Checks whether the given user login name is available, i.e.,
-- not already used.
checkValidationTokenAvailable :: String -> IO Bool
checkValidationTokenAvailable token = fmap null $ runQ
  ``sql* Select *
         From ValidationToken as vt
         Where vt.Token = { token };''

addValidationToken :: String -> ClockTime -> UserID -> IO ()
addValidationToken token time user = runQ
  ``sql* Insert into ValidationToken (Token, ValidSince, UserValidatingKey)
         Values ({ token }, { time }, { user });''

getValidationTokenWithToken :: String -> IO (Maybe ValidationToken)
getValidationTokenWithToken token = fmap listToMaybe $ runQ
  ``sql* Select *
         From ValidationToken as vt
         Where vt.Token = { token };''

deleteValidationTokenWithToken :: String -> IO ()
deleteValidationTokenWithToken token = runQ
  ``sql* Delete From ValidationToken Where Token = { token };''

-----------------------------------------------------------------------
-- Checks whether the given user login name is available, i.e.,
-- not already used.
checkUserNameAvailable :: String -> IO Bool
checkUserNameAvailable name = fmap null $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { name };''

-- Checks whether the given email is available, i.e., not already used.
checkEmailAvailable :: String -> IO Bool
checkEmailAvailable email = fmap null $ runQ
  ``sql* Select *
         From User as u
         Where u.Email = { email };''

checkLoginData :: String -> String -> IO Bool
checkLoginData loginName password = fmap (not . null) $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { loginName } And u.Password = { password } And u.Role != { roleInvalid };''

-- Gets the role of a user with a given login name.
getRoleOfUser :: String -> IO String
getRoleOfUser loginName = fmap (\rs -> if null rs then "" else head rs) $ runQ
  ``sql* Select u.Role
         From User as u
         Where u.LoginName = { loginName };''

getUserByName :: String -> IO (Maybe User)
getUserByName loginName = fmap listToMaybe $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { loginName };''

{-
getUsersByName :: [String] -> IO [String]
getUsersByName names = runQ
  ``sql* Select *
         From User as u
         Where u.LoginName In { names };''
-}

getUserByNameOrEmail :: String -> IO (Maybe User)
getUserByNameOrEmail login = fmap listToMaybe $ runQ
  ``sql* Select *
         From User as u
         Where u.LoginName = { login } Or u.Email = { login };''

checkUserWatches :: User -> Package -> IO Bool
checkUserWatches user package = fmap (not . null) $ runQ
  ``sql* Select *
         From Watching as w
         Where w.UserWatchingKey = { userKey user } And w.PackageWatchingKey = { packageKey package };''

checkUserMaintains :: User -> Package -> IO Bool
checkUserMaintains user package = fmap (not . null) $ runQ
  ``sql* Select *
         From Maintainer as m
         Where m.UserMaintainerKey = { userKey user } And m.PackageMaintainerKey = { packageKey package };''

getUserValidationToken :: User -> IO (Maybe ValidationToken)
getUserValidationToken user = fmap listToMaybe $ runQ
  ``sql* Select *
         From ValidationToken as vt
         Where vt.UserValidatingKey = { userKey user };''

{-
-- Requires currypp of 07/06/2023 or newer:
addUser :: String -> String -> String -> String -> DBAction ()
addUser loginName publicName email password =
  ``sql* Insert into User (LoginName, PublicName, Email, PublicEmail, Role,
                           Password, Token)
         Values ({ loginName }, { publicName }, { email }, "", "Invalid",
                 { password },  "" );''
-}

{-
validateUser :: UserID -> IO ()
validateUser user = runQ
  ``sql* Update User
         Set Role = { "Not Trusted" }
         Where Key = { user };''
-}

-----------------------------------------------------------------------
-- Gets the version of a package with a given name and version string.
getPackageWithNameAction :: String -> DBAction (Maybe Package)
getPackageWithNameAction pname = fmap listToMaybe $ 
  ``sql* Select *
         From Package as p
         Where p.Name = { pname };''

getPackageWithName :: String -> IO (Maybe Package)
getPackageWithName = runQ . getPackageWithNameAction

getDependenciesWithNameAction :: [String] -> DBAction [Either String Package]
getDependenciesWithNameAction = mapM (taggedDBAction getPackageWithNameAction)

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

getPackageVersionByName :: String -> String -> IO (Maybe Version)
getPackageVersionByName pname vers = runQ $  getPackageVersionByNameAction pname vers

getNumberOfMaintainers :: Package -> IO Int
getNumberOfMaintainers = fmap length . getMaintainersOfPackage

getMaintainersOfPackage :: Package -> IO [User]
getMaintainersOfPackage package = fmap (map fst) $ runQ
  ``sql* Select *
         From User as u, Package as p
         Where p.Key = { packageKey package } And
               Satisfies u maintains p;''

getNonMaintainersOfPackage :: Package -> IO [User]
getNonMaintainersOfPackage package = fmap (map fst) $ runQ
  ``sql* Select *
         From User as u, Package as p
         Where p.Key = { packageKey package } And
               Not Satisfies u maintains p;''

checkIfMaintainerAction :: Package -> User -> DBAction Bool
checkIfMaintainerAction package user = fmap (not . null) $
  ``sql* Select *
         From Package as p, User as u
         Where p.Key = { packageKey package } And
               u.Key = { userKey user } And
               Satisfies u maintains p;''

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

getCategoryWithName :: String -> IO (Maybe Category)
getCategoryWithName = runQ . getCategoryWithNameAction

getCategoriesWithNameAction :: [String] -> DBAction [Either String Category]
getCategoriesWithNameAction = mapM (taggedDBAction getCategoryWithNameAction)

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

-- Gets package versions containing a given string in the description.
getPackageVersionsByPattern :: String -> IO [(String,String)]
getPackageVersionsByPattern s = runQ
  ``sql* Select p.Name, v.Version
         From Version as v, Package as p
         Where v.Description Like { "%" ++ s ++ "%" } And
               Satisfies v versionOf p;''

-----------------------------------------------------------------------
