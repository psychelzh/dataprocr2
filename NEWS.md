# dataproc.iquizoo (development version)

* Add `calculate()` to calculate indices based on given data.

# dataproc.iquizoo 0.2.4

* Just as `complexswitch()`, now `switchcost()` will also return abnormal result (all `NA`s) if at least one block has no responses.

# dataproc.iquizoo 0.2.3

* Fix a bug occured in function `multisense()` when users did not respond to sound trials.

# dataproc.iquizoo 0.2.2

* Fix a bug occured in function `wxpred()` after filling `NA` values to `name_block` (mostly is just `Block`) column.
* Fix a bug occured in function `driving()` when minus signs, which are confused with hyphens, are logged into data.

# dataproc.iquizoo 0.2.1

* Now `complexswitch()` will return abnormal result (all `NA`s) if at least one block has no responses.

# dataproc.iquizoo 0.2.0

* Rename package from "dataprocr2" to "dataproc.iquizoo". This is recommended because all the work is related to www.iquizoo.com.

# dataproc.iquizoo 0.1.4

* Added a utility function `calc_sdt()` to calculate signal detection theory related indices.
* Fixed an issue related to perfect accuracy in `drm()`.

# dataproc.iquizoo 0.1.3

* Added a `NEWS.md` file to track changes to the package.
* Added `pc_all` index, meaning overal percent of correct, to `wxpred` function.
