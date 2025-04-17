Masala: The Repository of Curry Packages
========================================

This Curry package contains the implementation of
[Masala](https://cpm.curry-lang.org/masala/run.cgi),
a web-based system to upload and publish software packages
written in the declarative multi-paradigm language
[Curry](http://www.curry-lang.org/).

The initial version of this implementation has been generated
with the web framework
[Spicey](https://cpm.curry-lang.org/pkgs/spicey.html).


Installation
------------

1. Before you compile the generated web application for the first time,
   execute the command

       > make install

   to install all packages required by this application
   (where the executable `cypm` of the Curry Package Manager should
   be in your path).

2. Before you deploy your web application, you should
   define the variable WEBDIR in the `Makefile` according to
   your desired location.

   Then you can deploy your web application by the command

       > make SYSTEM=/opt/kics2 deploy

3. In order to enable code management with the Curry language server
   (e.g., using VisualCode), all SQL database queries are contained
   as integrated code in the file `src/Model/Queries.curry.pp`.
   Therefore, new SQL queries should be added to this file.
   During the make process, this file is translated into the pure Curry
   program `src/Model/Queries.curry` (which should not be modified).

4. In the default deployment, the web application runs as a __test system__.
   In a test system (the test status is also indicated in the main
   header of the web page), mails are not really sent but its contents is
   just shown in the web page which sent it (see `Controller.Mail`).
   Moreover, the source files of packages are not downloaded and
   nothing is sent to CPM when a package is published.
   In order to run the deployed system as a real system, set the
   environment variable `MASALATEST` to `no` in script execution,
   which can be done by adding the line

       export MASALATEST=no
  
   in the generated `run.cgi` script.

5. To install a tar file with the complete contents of the directory
   with the web application, run

       > make tar

   This generates a tar file `MASALA.tgz`.

   Now copy and unpack it in the desired directory of the web server
   (`.../masala`).

   Note: if necessary, add the following `.htaccess` file after unpacking:

       Options +ExecCGI
       AddHandler cgi-script .cgi


Files
-----

The directory structure of this package is as follows
(where <MODEL> is the name of your entity-relationship model).

* `src/Model/`
  This directory contains the implementation of the data model, i.e.,
  it contains the Curry module `<MODEL>.curry` implementing the access
  to the database which are generated from the ER description.
  In addition to the Curry program, this directory also contains
  the Curry file of the original ER description (file `<MODEL>_ERD.curry`),
  the term files of the transformed ER description (`<MODEL>_ERDT.term`),
  and the info file `<MODEL>_SQLCode.info` which is used by
  the Curry preprocessor when SQL queries are embedded in the source
  code.

* `src/Controller/`
  This directory contains the implementation of the various
  controllers that are responsible to react on user interactions.  It
  also contains a module `AuthorizedControllers.curry` that contains the
  authorization rules that should usually be adapted according to the
  customer requirements.

* `src/View/`
  This directory contains the implementation of the views of the
  different entities. These views are called from the corresponding
  controllers.  Moreover, it also contains the module
  `<MODEL>EntitiesToHtml.curry` which contains the code to translate all
  entities into an HTML representation. Usually, this code should be
  adapted according to the customer requirements.

* `src/Config/`
  This directory contains modules to configure the overall access to
  the functionality provided by the system.  For instance, the
  information about the routes, i.e., the URLs supported by the system
  and their mapping to individual controllers, is defined in the
  module `RoutesData.curry`. The module `ControllerMapping.curry` defines
  the actual mapping of controller references to the operations
  implementing the controllers. The module `UserProcesses.curry`
  contains the definition of the processes that can be selected by the
  user.

* `src/System/`
  This directory contains global modules implementing session
  management, authentication, authorization, user processes, etc.

* `public/`
  This directory contains images, style files, and HTML documents
  used by the installed system.
