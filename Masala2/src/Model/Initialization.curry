module Initialization where

import Masala2

import Text.CSV
import System.Environment
import Data.List
import Data.Either
import Data.Maybe
import System.IO.Unsafe

import qualified Data.Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

createAdmin :: Database.CDBI.Connection.DBAction User
createAdmin = newUser "Admin" "admin@masala2.org" "" "Admin" "password" "" Nothing

createCategory :: String -> Database.CDBI.Connection.DBAction Category
createCategory category = newCategory category ""

insertCategories :: Database.CDBI.Connection.Connection -> [[String]] -> IO [Either Database.CDBI.Connection.DBError (String, CategoryID)]
insertCategories connection lines = do 
    let cats = nub $ concatMap read $ drop 1 $ concatMap (drop 5) lines
    results <- mapM (flip Database.CDBI.Connection.runDBAction connection . createCategory) cats
    return $ zipWith (\cat result -> fmap ((,) cat) result) cats (map (fmap categoryKey) results)

createModule :: String -> Database.CDBI.Connection.DBAction CurryModule
createModule = newCurryModule

insertModules :: Database.CDBI.Connection.Connection -> [[String]] -> IO [Either Database.CDBI.Connection.DBError (String, CurryModuleID)]
insertModules connection lines = do 
    let mods = nub $ concatMap read $ drop 1 $ concatMap (take 1 . drop 4) lines
    results <- mapM (flip Database.CDBI.Connection.runDBAction connection . createModule) mods
    return $ zipWith (\mod result -> fmap ((,) mod) result) mods (map (fmap curryModuleKey) results)

createPackage :: String -> Database.CDBI.Connection.DBAction Package
createPackage package = newPackage package False

insertPackages :: Database.CDBI.Connection.Connection -> [[String]] -> IO [Either Database.CDBI.Connection.DBError (String, PackageID)]
insertPackages connection lines = do 
    let pkgs = nub $ drop 1 $ concatMap (take 1) lines
    results <- mapM (flip Database.CDBI.Connection.runDBAction connection . createPackage) pkgs
    return $ zipWith (\pkg result -> fmap ((,) pkg) result) pkgs (map (fmap packageKey) results)

listToTriple :: [a] -> (a, a, a)
listToTriple [x, y, z] = (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

createVersion :: [(String, PackageID)] -> UserID -> Data.Time.ClockTime -> String -> String -> String -> Database.CDBI.Connection.DBAction Version
createVersion packages admin time package version description 
    | lookup package packages == Just packageID = newVersionWithPackageVersioningKeyWithUserUploadKey version True True description "" 0 time False packageID admin where packageID free

insertVersion :: Database.CDBI.Connection.Connection -> [[String]] -> [(String, PackageID)] -> UserID -> Data.Time.ClockTime -> IO [Either Database.CDBI.Connection.DBError ((String, String), VersionID)]
insertVersion connection lines packages admin time = do
    let versions = map listToTriple $ map (take 3) $ drop 1 lines
    results <- mapM (flip Database.CDBI.Connection.runDBAction connection . (uncurry3 (createVersion packages admin time))) versions
    return $ zipWith (\(pkg, vsn, dsc) result -> fmap ((,) (pkg, vsn)) result) versions (map (fmap versionKey) results)

createMaintainer :: UserID -> PackageID -> Database.CDBI.Connection.DBAction ()
createMaintainer = newMaintainer

insertMaintainer :: Database.CDBI.Connection.Connection -> [(String, PackageID)] -> UserID -> IO [Either Database.CDBI.Connection.DBError ()]
insertMaintainer connection packageKeys admin = do
    let dbactions = map (createMaintainer admin) (map snd packageKeys) :: [Database.CDBI.Connection.DBAction ()]
    mapM (flip Database.CDBI.Connection.runDBAction connection) dbactions

{-
createDepending :: [(String, PackageID)] -> [((String, String), VersionID)] -> [String] -> Database.CDBI.Connection.DBAction ()
createDepending packageKeys versionKeys line
    | lookup (line !! 0, line !! 1) versionKeys == Just (versionKey::VersionID)
        = foldl1 (Database.CDBI.Connection.>+)
        $ map (newDepending versionKey)
        $ map fromJust
        $ filter isJust
        $ map (flip lookup packageKeys) ((read::String -> [String]) $ line !! 3)
    where versionKey free
-}
createDepending :: [(String, PackageID)] -> [((String, String), VersionID)] -> [String] -> Database.CDBI.Connection.DBAction ()
createDepending packageKeys versionKeys line = case lookup (line !! 0, line !! 1) versionKeys of 
    Just versionKey -> foldl1 (Database.CDBI.Connection.>+)
        $ map (newDepending versionKey)
        $ map fromJust
        $ filter isJust
        $ map (flip lookup packageKeys) ((read::String -> [String]) $ line !! 3)
    Nothing -> trace (show (line !! 0, line !! 1)) failed


insertDependings :: Database.CDBI.Connection.Connection -> [(String, PackageID)] -> [((String, String), VersionID)] -> [[String]] -> IO [Either Database.CDBI.Connection.DBError ()]
insertDependings connection packageKeys versionKeys lines = do 
    let dbactions = map (createDepending packageKeys versionKeys) lines
    mapM (flip Database.CDBI.Connection.runDBAction connection) dbactions


resetDatabase :: IO ()
resetDatabase = do
    createNewDB sqliteDBFile

initializeDatabase :: IO ()
initializeDatabase = do 
    print "Connect"
    connection <- Database.CDBI.Connection.connectSQLite sqliteDBFile
    print "Add admin"
    result <- Database.CDBI.Connection.runDBAction createAdmin connection
    let admin = userKey $ fromRight result

    lines <- readCSVFile "allpkgs.csv"

    -- Add categories
    print "Add categories"
    categoryResults <- insertCategories connection lines
    let categoryKeys = rights categoryResults
    --print categoryKeys

    -- Add modules
    print "Add modules"
    moduleResults <- insertModules connection lines
    let moduleKeys = rights moduleResults
    --print moduleKeys
    
    -- Add packages
    print "Add packages"
    packageResults <- insertPackages connection lines
    let packageKeys = rights packageResults
    --print packageKeys

    -- Add versions
    print "Add versions"
    time <- Data.Time.getClockTime
    versionResults <- insertVersion connection lines packageKeys admin time
    let versionKeys = rights versionResults
    --print versionKeys

    -- Add admin as maintainer of all packages
    print "Add admin as maintainer"
    maintainerResults <- insertMaintainer connection packageKeys admin

    -- Add depending
    print "Add depending"
    dependingResults <- insertDependings connection packageKeys versionKeys (drop 1 lines)


    Database.CDBI.Connection.disconnect connection