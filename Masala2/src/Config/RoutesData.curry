module Config.RoutesData where

import Model.Queries
import System.Authentication

data ControllerReference = ProcessListController
                         | LoginController
                         | UserController
                         | PackageController
                         | VersionController
                         | CategoryController
                         | CurryModuleController
                         | ValidationTokenController
                         | RegistrationController
                         | ValidationController

data UrlMatch = Exact String
              | Prefix String String
              | Matcher (String -> Bool)
              | Always

type Route = (String,UrlMatch,ControllerReference)

--- This constant specifies the association of URLs to controllers.
--- Controllers are identified here by constants of type
--- ControllerReference. The actual mapping of these constants
--- into the controller operations is specified in the module
--- ControllerMapping.
getRoutes :: IO [Route]
getRoutes =
  do login <- getSessionLogin
     admin <- isAdmin
     return $
      --[("Processes",Exact "spiceyProcesses",ProcessListController)
      [("Packages",Prefix "Package" "list",PackageController)
      ,("New Package",Prefix "Package" "new",PackageController)
      ,("List Version",Prefix "Version" "list",VersionController)
      ,("New Version",Prefix "Version" "new",VersionController)
      ,("Categories",Prefix "Category" "list",CategoryController)
      ,("New Category",Prefix "Category" "new",CategoryController)
      ,("List CurryModule",Prefix "CurryModule" "list",CurryModuleController)
      ,("New CurryModule",Prefix "CurryModule" "new",CurryModuleController) ] ++
      (if admin
         then [("List User",Prefix "User" "list",UserController)
              ,("New User",Prefix "User" "new",UserController)
              ,("List ValidationToken"
               ,Prefix "ValidationToken" "list" ,ValidationTokenController)
              ,("New ValidationToken"
               ,Prefix "ValidationToken" "new"
               ,ValidationTokenController)]
         else []) ++
      [( "Registration"
       , Exact "Registration"
       , RegistrationController)
      ,( "Validation"
       , Exact "Validation"
       , ValidationController)
      ] ++
      case login of 
         Nothing -> []
         Just (loginName, role) -> 
                        [(loginName
                        ,Prefix "User" "profile"
                        ,UserController)]
      ++
      [(maybe "Login" (const "Logout") login,Exact "login",LoginController)
      ,("default",Always,PackageController)]
