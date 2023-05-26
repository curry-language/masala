import Database.ERD

masalaERD :: ERD
masalaERD =
    ERD "Masala2"
        [ Entity "User"
            [ -- Login name
              Attribute "LoginName"   (StringDom Nothing) Unique False
              -- Displayed name
            , Attribute "PublicName"  (StringDom Nothing) NoKey  False
            , Attribute "Email"       (StringDom Nothing) Unique False
              -- Displayed email
            , Attribute "PublicEmail" (StringDom Nothing) NoKey True
              -- Role of the user
              -- One of ADMIN,TRUSTED,NOT TRUSTED,INVALID
            , Attribute "Role"        (StringDom Nothing) NoKey False
            , Attribute "Password"    (StringDom Nothing) NoKey False 
            , Attribute "Token"       (StringDom Nothing) NoKey True
            , Attribute "LastLogin"   (DateDom Nothing) NoKey True
            ]
        , Entity "Package"
            [ Attribute "Name"        (StringDom Nothing) Unique False
            , Attribute "Abandoned"   (BoolDom Nothing) NoKey False]
        , Entity "Version"
            [ Attribute "Version"     (StringDom Nothing) NoKey False
              -- Whether the version has been published by an admin
              -- or automatically (for trusted users and admins)
            , Attribute "Published"   (BoolDom Nothing) NoKey False
            , Attribute "Tested"      (BoolDom Nothing) NoKey False
            , Attribute "Description" (StringDom Nothing) NoKey False
            , Attribute "JobStatus"   (StringDom Nothing) NoKey True
            , Attribute "Downloads"   (IntDom Nothing) NoKey False
            , Attribute "UploadDate"  (DateDom Nothing) NoKey False
            , Attribute "Deprecated"  (BoolDom Nothing) NoKey False]
        , Entity "Category" 
            [ Attribute "Name"        (StringDom Nothing) Unique False
            , Attribute "Description" (StringDom Nothing) NoKey True]
        , Entity "CurryModule" 
            [ Attribute "Name"        (StringDom Nothing) PKey False]
        , Entity "ValidationToken"
            [ Attribute "Token"       (StringDom Nothing) PKey False
            , Attribute "ValidSince"  (DateDom Nothing) NoKey False]
        ]
        [ -- A user can watch any number of packages
          Relationship "Watching"
            [ REnd "User" "watchedBy"       (Between 0 Infinite)
            , REnd "Package" "watches" (Between 0 Infinite)]
          -- A version exports any number of modules
        , Relationship "Exporting"
            [ REnd "Version" "exportedBy"  (Between 0 Infinite)
            , REnd "CurryModule" "exports" (Between 1 Infinite)]
          -- A package may have any number of versions
        , Relationship "Versioning"
            [ REnd "Package" "versionOf"   (Exactly 1)
            , REnd "Version" "withVersion" (Between 0 Infinite)]        
          -- A version may depend on any number of other packages
        , Relationship "Depending"
            [ REnd "Version" "dependencyOf" (Between 0 Infinite)
            , REnd "Package" "dependsOn"    (Between 0 Infinite)]
          -- A user can be the maintainer of any number of packages
          -- and a package can have any number of maintainers
        , Relationship "Maintainer"
            [ REnd "User" "maintainedBy"  (Between 0 Infinite)
            , REnd "Package"  "maintains" (Between 0 Infinite)]
          -- A user can upload any number of package versions
        , Relationship "Upload"
            [ REnd "User" "uploadedBy"     (Exactly 1)
            , REnd "Version" "hasUploaded" (Between 0 Infinite)]
          -- A category categorizes any number of versions
          -- Versions can have multiple categories
        , Relationship "Categorizes"
            [ REnd "Category" "withCategory" (Between 0 Infinite)
            , REnd "Version" "categorizes"   (Between 0 Infinite)]
        , Relationship "Validating"
            [ REnd "User" "validates"              (Exactly 1)
            , REnd "ValidationToken" "validatedBy" (Between 0 (Max 1))]]
