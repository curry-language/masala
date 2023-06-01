{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

--- Some SQL queries useful for the implementation of Masala.

module MasalaQueries where

import Data.List  ( nub )
import Data.Maybe ( listToMaybe )

import Database.CDBI.ER

import Masala2

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

-----------------------------------------------------------------------
-- Gets the version of a package with a given name and version string.
getPackageWithName :: String -> IO (Maybe Package)
getPackageWithName pname = fmap listToMaybe $ runQ
  ``sql* Select *
         From Package as p
         Where p.Name = { pname };''

-- Gets the version of a package with a given name and version string.
getPackageVersionByName :: String -> String -> IO (Maybe Version)
getPackageVersionByName pname vers = fmap (listToMaybe . map snd) $ runQ
  ``sql* Select *
         From Package as p, Version as v
         Where p.Name = { pname } And
               v.Version = { vers } And
               Satisfies v versionOf p;''

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
getPackageVersionCategories :: Package -> Version -> IO [Category]
getPackageVersionCategories pkg vers = fmap (map (\(_,_,c) -> c)) $ runQ
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
getCategoryWithName :: String -> IO (Maybe Category)
getCategoryWithName cname = fmap listToMaybe $ runQ
  ``sql* Select * From Category as c
         Where c.Name = { cname };''

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
getCurryModuleWithName :: String -> IO (Maybe CurryModule)
getCurryModuleWithName mname = fmap listToMaybe $ runQ
  ``sql* Select * From CurryModule as m
         Where m.Name = { mname };''

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
