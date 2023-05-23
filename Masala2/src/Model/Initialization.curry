module Initialization where

import Masala2

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

createAdmin :: String -> String -> String -> String -> DBAction User
createAdmin loginname publicname email cryptpasswd =
  newUser loginname publicname email email "Admin" cryptpasswd "" Nothing

createCategory :: String -> DBAction Category
createCategory category = newCategory category ""

insertCategories :: Connection -> [[String]]
                 -> IO [Either DBError (String, CategoryID)]
insertCategories connection lines = do 
    let cats = nub $ concatMap read $ drop 1 $ concatMap (drop 6) lines
    results <- mapM (flip runDBAction connection . createCategory) cats
    return $ zipWith (\cat result -> fmap ((,) cat) result) cats (map (fmap categoryKey) results)

createModule :: String -> DBAction CurryModule
createModule = newCurryModule

insertModules :: Connection -> [[String]]
           -> IO [Either DBError (String, CurryModuleID)]
insertModules connection lines = do 
    let mods = nub $ concatMap read $ drop 1 $ concatMap (take 1 . drop 5) lines
    results <- mapM (flip runDBAction connection . createModule) mods
    return $ zipWith (\mod result -> fmap ((,) mod) result) mods (map (fmap curryModuleKey) results)

createPackage :: String -> DBAction Package
createPackage package = newPackage package False

insertPackages :: Connection -> [[String]] -> IO [Either DBError (String, PackageID)]
insertPackages connection lines = do 
    let pkgs = nub $ drop 1 $ concatMap (take 1) lines
    results <- mapM (flip runDBAction connection . createPackage) pkgs
    return $ zipWith (\pkg result -> fmap ((,) pkg) result) pkgs (map (fmap packageKey) results)

listTo4Tuple :: [a] -> (a, a, a, a)
listTo4Tuple [a, b, c, d] = (a, b, c, d)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

createVersion :: [(String, PackageID)] -> UserID -> ClockTime -> String
              -> String -> String -> String -> DBAction Version
createVersion packages admin ctime package version description uploadtimeS =
  maybe
    (error "createVersion: no packageID found!")
    (\packageID ->
      let uptime = case reads uploadtimeS of
                     [(Just uptime, "")] -> toClockTime uptime
                     _                   -> ctime
      in newVersionWithPackageVersioningKeyWithUserUploadKey version True True
           description "" 0 uptime False packageID admin)
    (lookup package packages)

insertVersion :: Connection -> [[String]] -> [(String, PackageID)] -> UserID
              -> ClockTime -> IO [Either DBError ((String, String), VersionID)]
insertVersion connection lines packages admin currtime = do
    let versions = map listTo4Tuple $ map (take 4) $ drop 1 lines
    results <- mapM (flip runDBAction connection .
                       (uncurry4 (createVersion packages admin currtime)))
                    versions
    return $ zipWith (\(pkg, vsn, dsc, _) result -> fmap ((,) (pkg, vsn)) result)
                     versions
                     (map (fmap versionKey) results)

createMaintainer :: UserID -> PackageID -> DBAction ()
createMaintainer = newMaintainer

insertMaintainer :: Connection -> [(String, PackageID)] -> UserID
                 -> IO [Either DBError ()]
insertMaintainer connection packageKeys admin = do
    let dbactions = map (createMaintainer admin) (map snd packageKeys) :: [DBAction ()]
    mapM (flip runDBAction connection) dbactions

{-
createDepending :: [(String, PackageID)] -> [((String, String), VersionID)] -> [String] -> DBAction ()
createDepending packageKeys versionKeys line
    | lookup (line !! 0, line !! 1) versionKeys == Just (versionKey::VersionID)
        = foldl1 (>+)
        $ map (newDepending versionKey)
        $ map fromJust
        $ filter isJust
        $ map (flip lookup packageKeys) ((read::String -> [String]) $ line !! 3)
    where versionKey free
-}
createDepending :: [(String, PackageID)] -> [((String, String), VersionID)] -> [String] -> DBAction ()
createDepending packageKeys versionKeys line =
  case lookup (line !! 0, line !! 1) versionKeys of 
    Just versionKey -> foldl (>+) (returnDB (Right ()))
        $ map (newDepending versionKey)
        $ map fromJust
        $ filter isJust
        $ map (flip lookup packageKeys) ((read::String -> [String]) $ line !! 4)
    Nothing -> error "createDepending: no version key found"


insertDependings :: Connection -> [(String, PackageID)]
                 -> [((String, String), VersionID)] -> [[String]]
                 -> IO [Either DBError ()]
insertDependings connection packageKeys versionKeys lines = do 
    let dbactions = map (createDepending packageKeys versionKeys) lines
    mapM (flip runDBAction connection) dbactions


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
  maintainerResults <- insertMaintainer connection packageKeys admin

  -- Add depending
  putStrLn "Add depending"
  dependingResults <- insertDependings connection packageKeys versionKeys
                                       (drop 1 lines)

  disconnect connection
