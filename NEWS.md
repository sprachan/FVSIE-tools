# rFVSIEtools (development version)

# rFVSIEtools 0.1.1

Bug fixes and docs improvements:
* Fix CYCLEAT bug in multistand keyword files
* Fix missing END keyword after ESTAB keyword
* Remove get stand/tree column functions which are unnecessary with DB integration
* More concise docs

# rFVSIEtools 0.1.0

Full set of basic package functions:
* Get FVS-ready tables from the FIA database (FVS_<Stand/Tree>Init_<PLOT/COND>),
  optionally filtered by a condition table and optionally written to a database.
* Add persistent unique Plot and Tree identifier columns to FVS-ready tables.
* Write FVS keyword files for single- and multi-stand runs using a SQL database for data input.
* Run FVS using single- or multi-stand keyword files.
