**User Roles**
1. Trusted:
    User is validated and trusted by Admin. Is allowed to upload packages, which will be publicized by Admin.
2. Not Trusted:
    User is validated but not trusted by Admin.
3. Invalid:
    User has been registered but not yet validated. Has no rights for now including login.

**Admin functionality regarding users**
1. Admin can change roles of users from "Not Trusted" to "Trusted" and vice versa
2. Admin can delete users

**Admin functionality regarding packages: checking, publishing**
1. Admin has a list of all uploaded but not yet published packages
2. Admin can run test on unpublished packages
3. Admin can look into unpublished packages files
4. Admin can publish unpublished packages, if tests run successfully

**Validation Email**
Is send to a new user on successful sign up. Contains a validation link to validate user. Validation link contains a Validationtoken linked to the invalid user. Validation link is itself valid for 10 minutes.

**Changing User from "Invalid" to "Not Trusted"**
*Precondition: Sing up was successful and validation email was send*
1. User opens their emails
2. User looks for the validation email
3. User clicks on validation link
4. If link still valid:
    1. App checks that User is "Invalid"
    2. If User is not "Invalid"
        1. App informs User, that they are already validated
    3. If User is "Invalid":
        1. App changes User from "Invalid" to "Not Trusted"
        2. App moves User to main page
        3. User is still logged out
        4. App deletes Validationtoken from database
5. If link not valid anymore:
    1. App moves User to website with a button, which resends a validation email

**Changing User from "Not Trusted" to "Trusted"**
*Precondition: Logged in*
*Who: Admin*
1. Admin clicks userlist
2. App shows list of all users
3. Admin can use a filter to only see users with "Not Trusted" role
4. Admin selects one or more users with "Not Trusted" role by clicking on checkboxes next to them
5. Admin clicks on button "Change to Trusted"
6. App changes Role of all selected users to "Trusted"

**Sign Up**
*Precondition: User not logged in*
*Where: Arbitrary*
1. User clicks button "Sign Up"
2. App moves User to Registrationside
3. User inputs e-mail address, username und 2 times the same password
4. User clicks button "Sign Up"
5. App checks, that both passwords are the same
6. App checks, that username and e-mail are available
7. If available:
    1. App creates new User Entry in database with this data
    2. App configures User as "Invalid"
    3. App sends a validation email to User with a validation link
    4. App informs User about successful sign up and the validation email
    5. App provides a button for User to resend validation email
8. If not available:
    1. App informs User whether e-mail address or username is not available
    2. User remains on Registrationside
    3. Input data remains except password
    4. App marks faulty data

**Login**
*Precondition: User is logged out*
*Where: Arbitrary*
1. User clicks button "Login"
2. App moves User to Loginpage
3. User inputs either username or email and password
4. App checks, that data is correct
5. If user with username or email does not exist:
    1. App informs User, that such a user does not exist
    2. User remains on Loginpage
    3. App deletes input data
6. If user with username or email does exist:
    1. App checks that password is correct
    2. If password is correct:
        1. App generates new Logintoken
        2. App sends confirmation and token to User
        3. App moves User to main page using token
    3. If password is not correct:
        1. App informs User, that password is not correct
        2. User remains on Loginpage
        3. App deletes password but username or email remains

**Logout**
**Precondition: User is logged in*
*Where: Arbitrary*
1. User clicks button "Logout"
2. App deletes current Logintoken
3. App moves User to mainpage without token

**Upload Package**
*Precondition: User is logged in*
*Where: mainpage*
1. User clicks button "Upload Package"
2. App moves User to upload page
3. User inputs JSON-file and tar-file, that form the package
4. User clicks button "Upload"
5. App informs User that upload is permanent and asks for confirmation
6. If User clicks button "Cancel":
    1. User remains on upload page
    2. Input data remains
7. If User clicks button "Upload":
    1. App checks, that both files have the same name
    2. If names are not the same:
        1. App informs User that both files need the same name
        2. User remains on upload page
        3. Input data remains
    3. If names are the same:
        1. App checks if a package with that name already exists
        2. If package already exists:
            1. App informs User, that a package with that name already exists
            2. App informs User, that they either use a different name or upload an update
        3. If package does not exist:
            1. App runs automatic tests on package to check that it works
            2. If tests fail:
                1. App informs User, that some tests failed and tells them what tests
            3. If tests are successful:
                1. App creates new package entry with given data
                2. User is set as Maintainer of that package
                3. App informs User that package has been uploaded
                4. App checks Role of User
                5. If User has Role "Not Trusted":
                    1. App sends email to Admin, that a new package has been uploaded
                    2. App informs User, that they have to wait for Admin to publish the package
                6. If User has Role "Trusted":
                    1. App publishes package
                    2. App informs User that package has been published
