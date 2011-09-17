Instructions for installation of PIE site

- Runs on Oracle + AOLserver + Tcl
- Does not require ACS

- this directory, admin, assessement, content contain the tcl scripts for the site
- directory docs cotanis various documents including pie.html that describes the site
- directory config contains the following:
  - util.tcl, zz-10-databases-procs.tcl, zz-00-proc-procs.tcl, zz-utilities.tcl are the
    library scripts that have to be loaded at AOLserver startup
  - datamodel.sql is the data file for oracle
  - config.tcl is the AOLserver config file