import Text.CSV
import System.Environment
import Data.List

-- Name
data Package = Package String
    deriving (Show, Read, Eq)

-- Version, Description, version_of, Dependencies, Modules, Category
data Version = Version String String Package [Package] [CurryModule] [Category]
    deriving (Show, Read, Eq)

-- Category
data Category = Category String
    deriving (Show, Read, Eq)

-- Module
data CurryModule = CurryModule String
    deriving (Show, Read, Eq)


readName :: String -> Package
readName = Package

readDependencies :: String -> [Package]
readDependencies = map Package . read

readCategories :: String -> [Category]
readCategories = map Category . read

readModules :: String -> [CurryModule]
readModules = map CurryModule . read

readLine :: [String] -> (Package, Version, [Category], [CurryModule])
readLine [name, version, description, dependencies, modules, categories] =
    ( Package name
    , Version version description (readName name) (readDependencies dependencies) (readModules modules)  (readCategories categories)
    , readCategories categories
    , readModules modules)

cleanup :: ([Package], [Version], [[Category]], [[CurryModule]]) -> ([Package], [Version], [Category], [CurryModule])
cleanup (packages, versions, categories, modules) = 
    ( nub packages
    , versions
    , (nub . concat) categories
    , (nub . concat) modules)

unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 = foldl (\(as, bs, cs, ds) (a, b, c, d) -> (a:as, b:bs, c:cs, d:ds)) ([], [], [], [])

readDatabaseEntries :: [[String]] -> ([Package], [Version], [Category], [CurryModule])
readDatabaseEntries = cleanup . unzip4 . map readLine

readEntries :: String -> IO ()
readEntries file = do
    lines <- readCSVFile file
    let (packages, versions, categories, modules) = readDatabaseEntries lines
    print categories
    return ()

main :: IO ()
main = do 
    arguments <- getArgs
    if null arguments
        then return ()
        else readEntries (arguments !! 0)
