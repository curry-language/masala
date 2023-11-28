How packages are uploaded and published to CPM with Masala
==========================================================

When a package with name PKG and version identifier VERS
is uploaded to Masala, some checks are performed
(implemented in `System.PackageHelpers`):

1. Has the package specification a 'source' field?
2. Is the 'source' field either `Http _` or a `Git url ...`?

If these checks are satisfied, the package source is downloaded
into the local Masala directory (which is the main directory of
the web application, i.e., `/var/www/masala` at server
`cpm@informatik.uni-kiel.de`):

1. The package specification is stored at `data/packages/PKG/VERS/package.json`.
2. The package source is stored in directory `data/downloads/PKG-VERS`.
3. The tar file to be stored in CPM is written to `TARFILES/PKG-VERS.tar.gz`.

If everything works, the package upload is further checked
(w.r.t. login, maintainer, version,...) and if everything is fine,
it is added to the Masala DB.

If the package should be published (either during upload or by setting
the visibility is `Public` in Masala), the publication to CPM
is scheduled via the operation `System.PackageHelpers.uploadPackageToCPM`.
This sends the package specifcation on stdin to the URL

    https://www-ps.informatik.uni-kiel.de/~cpm/cpm-upload-masala.cgi

which is done by the command

    cat package.json | curl --data-binary @- .../cpm-upload-masala.cgi

The script `cpm-upload-masala.cgi` basically calls the command `cpm-upload`
for the user `cpm`, i.e., it is implemented by

    ssh -p 55055 cpm@cpm.informatik.uni-kiel.de .cpm/bin/cpm-upload --masala

The command `cpm-upload --masala` checks whether the tar file for the
package is available under `/var/www/masala/TARFILES` and stores
these package sources and the package specification in the CPM
directories and schedules a CPM job for generating the package documentation.

