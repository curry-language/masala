{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Model.Initialization where

import Model.Masala2

import Text.CSV
import System.Environment
import Data.List
import Data.Either
import Data.Maybe
import System.IO.Unsafe

import Data.Time
import Database.CDBI.ER
import Database.CDBI.Criteria
import Database.CDBI.Connection
import Database.CDBI.Description
import System.Authentication ( getUserHash )
import System.Directory      ( removeFile )

-- DBAction to create an admin in a database.
createAdmin :: String -> String -> String -> String -> DBAction User
createAdmin loginname publicname email cryptpasswd =
  newUser loginname publicname email email "Admin" cryptpasswd "" Nothing

-- DBAction to create a category in a database.
createCategory :: String -> DBAction Category
createCategory category = newCategory category ""

-- This function inserts categories into a database
-- from a list of lines read from a csv file.
insertCategories :: Connection -> [[String]]
                 -> IO [Either DBError (String, CategoryID)]
insertCategories connection lines = do 
    let cats = nub $ concatMap read $ drop 1 $ concatMap (drop 6) lines
    results <- mapM (flip runDBAction connection . createCategory) cats
    return $ zipWith (\cat result -> fmap ((,) cat) result) cats (map (fmap categoryKey) results)

-- DBAction to create a module in a database.
createModule :: String -> DBAction CurryModule
createModule = newCurryModule

-- This function inserts modules into a database
-- from a list of lines read from a csv file.
insertModules :: Connection -> [[String]]
           -> IO [Either DBError (String, CurryModuleID)]
insertModules connection lines = do 
    let mods = nub $ concatMap read $ drop 1 $ concatMap (take 1 . drop 5) lines
    results <- mapM (flip runDBAction connection . createModule) mods
    return $ zipWith (\mod result -> fmap ((,) mod) result) mods (map (fmap curryModuleKey) results)

-- DBAction to create a package in a database.
createPackage :: String -> DBAction Package
createPackage package = newPackage package False

-- This function inserts packages into a database
-- from a list of lines read from a csv file.
insertPackages :: Connection -> [[String]] -> IO [Either DBError (String, PackageID)]
insertPackages connection lines = do 
    let pkgs = nub $ drop 1 $ concatMap (take 1) lines
    results <- mapM (flip runDBAction connection . createPackage) pkgs
    return $ zipWith (\pkg result -> fmap ((,) pkg) result) pkgs (map (fmap packageKey) results)

-- This function transform a list with 4 values into a tuple.
listTo4Tuple :: [a] -> (a, a, a, a)
listTo4Tuple [a, b, c, d] = (a, b, c, d)

-- uncurry for a tuple with length 4.
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- DBAction to create a version in a database.
createVersion :: [(String, PackageID)] -> UserID -> ClockTime -> String
              -> String -> String -> String -> DBAction Version
createVersion packages admin ctime package version description uploadtimeS =
  maybe
    (error "createVersion: no packageID found!")
    (\packageID ->
      let uptime = case reads uploadtimeS of
                     [(Just upldtime, "")] -> toClockTime upldtime
                     _                     -> ctime
      in newVersionWithPackageVersioningKeyWithUserUploadKey version True True
           description "" 0 uptime False packageID admin)
    (lookup package packages)

-- This function inserts versions into a database
-- from a list of lines read from a csv file.
insertVersion :: Connection -> [[String]] -> [(String, PackageID)] -> UserID
              -> ClockTime -> IO [Either DBError ((String, String), VersionID)]
insertVersion connection lines packages admin currtime = do
    let versions = map listTo4Tuple $ map (take 4) $ drop 1 lines
    results <- mapM (flip runDBAction connection .
                       (uncurry4 (createVersion packages admin currtime)))
                    versions
    return $ zipWith (\(pkg, vsn, _, _) result -> fmap ((,) (pkg, vsn)) result)
                     versions
                     (map (fmap versionKey) results)

-- DBAction to make a user a maintainer for a package.
createMaintainer :: UserID -> PackageID -> DBAction ()
createMaintainer = newMaintainer

-- This function inserts maintainers into a database.
insertMaintainer :: Connection -> [(String, PackageID)] -> UserID
                 -> IO [Either DBError ()]
insertMaintainer connection packageKeys admin = do
    let dbactions = map (createMaintainer admin) (map snd packageKeys) :: [DBAction ()]
    mapM (flip runDBAction connection) dbactions

-- DBAction to make a version depend on a package.
createDepending :: [(String, PackageID)] -> [((String, String), VersionID)] -> [String] -> DBAction ()
createDepending packageKeys versionKeys line =
  case lookup (line !! 0, line !! 1) versionKeys of 
    Just versionKey -> foldl (>+) (returnDB (Right ()))
        $ map (newDepending versionKey)
        $ map fromJust
        $ filter isJust
        $ map (flip lookup packageKeys) ((read::String -> [String]) $ line !! 4)
    Nothing -> error "createDepending: no version key found"

-- This function inserts dependency relationships into a database.
insertDependings :: Connection -> [(String, PackageID)]
                 -> [((String, String), VersionID)] -> [[String]]
                 -> IO [Either DBError ()]
insertDependings connection packageKeys versionKeys lines = do 
    let dbactions = map (createDepending packageKeys versionKeys) lines
    mapM (flip runDBAction connection) dbactions

-- DBAction to make a version export a module.
createExporting :: [((String, String), VersionID)] -> [(String, CurryModuleID)] -> [String] -> DBAction ()
createExporting versionKeys moduleKeys line = case lookup (line !! 0, line !! 1) versionKeys of 
        Just versionKey -> foldl (>+) (returnDB (Right ()))
            $ map (newExporting versionKey)
            $ map fromJust
            $ filter isJust
            $ map (flip lookup moduleKeys) (read $ line !! 5)
        Nothing -> error "createExporting: no version key found"

-- This function inserts exporting relationships into a database.
insertExportings :: Connection -> [((String, String), VersionID)] -> [(String, CurryModuleID)] -> [[String]]
                 -> IO [Either DBError ()]
insertExportings connection versionKeys moduleKeys lines = do
    let dbactions = map (createExporting versionKeys moduleKeys) lines
    mapM (flip runDBAction connection) dbactions

-- DBAction to make a version use a category.
createCategorizes :: [((String, String), VersionID)] -> [(String, CategoryID)] -> [String] -> DBAction ()
createCategorizes versionKeys categoryKeys line = case lookup (line !! 0, line !! 1) versionKeys of 
        Just versionKey -> foldl (>+) (returnDB (Right ()))
            $ map (flip newCategorizes versionKey)
            $ map fromJust
            $ filter isJust
            $ map (flip lookup categoryKeys) (read $ line !! 6)
        Nothing -> error "createExporting: no version key found"

-- This function inserts categorizes relationships into a database.
insertCategorizes :: Connection -> [((String, String), VersionID)] -> [(String, CategoryID)] -> [[String]]
                 -> IO [Either DBError ()]
insertCategorizes connection versionKeys moduleKeys lines = do
    let dbactions = map (createCategorizes versionKeys moduleKeys) lines
    mapM (flip runDBAction connection) dbactions

-- This function deletes a sql file and creates a new one.
resetDatabase :: IO ()
resetDatabase = do
    putStrLn $ "Reset database '" ++ sqliteDBFile ++ "'..."
    removeFile sqliteDBFile
    createNewDB sqliteDBFile

------------------------------------------------------------------------------

--- Create a new empty database and fills it with data from file `allpkgs.csv`
--- (which was generated by `cpm-manage writeall`) and an initial
--- admin user.
main :: IO ()
main =
  initializeDatabase "admin" "The Masala Administrator"
                     "masala@curry-lang.org" "123456"

--- Create a new empty database and fills it with data from file `allpkgs.csv`
--- (which was generated by `cpm-manage writeall`).
--- The arguments are the login name, public name, email,
--- and password of the admin user.
initializeDatabase :: String -> String -> String -> String -> IO ()
initializeDatabase lname pname email passwd = do
    resetDatabase
    connection <- connectSQLite sqliteDBFile
    putStrLn "Add admin"
    cryptpasswd <- getUserHash lname passwd
    result <- runDBAction (createAdmin lname pname email cryptpasswd) connection
    let admin = userKey $ fromRight result

    lines <- readCSVFile "allpkgs.csv"

    -- Add categories
    putStrLn "Add categories"
    categoryResults <- insertCategories connection lines
    let categoryKeys = rights categoryResults
    --putStrLn categoryKeys

    -- Add modules
    putStrLn "Add modules"
    moduleResults <- insertModules connection lines
    let moduleKeys = rights moduleResults
    --putStrLn moduleKeys
    
    -- Add packages
    putStrLn "Add packages"
    packageResults <- insertPackages connection lines
    let packageKeys = rights packageResults
    --putStrLn packageKeys

    -- Add versions
    putStrLn "Add versions"
    time <- Data.Time.getClockTime
    versionResults <- insertVersion connection lines packageKeys admin time
    let versionKeys = rights versionResults
    --putStrLn versionKeys

    -- Add admin as maintainer of all packages
    putStrLn "Add admin as maintainer"
    insertMaintainer connection packageKeys admin

    -- Add depending
    putStrLn "Add depending"
    insertDependings connection packageKeys versionKeys
                                        (drop 1 lines)

    -- Add exporting
    putStrLn "Add exporting"
    insertExportings connection versionKeys moduleKeys (drop 1 lines)

    -- Add categorizes
    putStrLn "Add categorizes"
    insertCategorizes connection versionKeys categoryKeys (drop 1 lines)

    disconnect connection

testInitialization :: IO ()
testInitialization = do
    resetDatabase
    initializeDatabase "Admin" "Admin" "admin@masala.org" "123456"
