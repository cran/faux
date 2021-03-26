# faux 1.0.0

* Ready for a new release.
* Fixed a typo in faceratings for one face_eth ("eats asian/white" => "east asian/white")

# faux 0.0.1.65 (2021-03-23)

* Fixed a bug in `rnorm_pre()` when simulating a vector with correlations to more than 2 pre-existing vectors.
* Updated vignettes for new functions and changes.

# faux 0.0.1.64 (2021-03-19)

* `sim_design()` should no longer mangle level values in long format if they have underscores
* `sim_design()` should play better with different separator. FOr example, if you set `faux_options(sep = ".")` and have within-subject factors A and B with levels A_1/A_2 and B_1/B_2, your wide data will have columns A_1.B_1, A_1.B_2, A_2.B_1, A_2.B_2

# faux 0.0.1.63 (2021-03-14)

* fixed bug in `sim_design()` where parameters specified as a named vector couldn't be in a different order unless both between and within factors were specified (e.g., `mu = c(A2 = 2, A1 = 1)` resulted in a mu of 2 for A1 and 1 for A2).

# faux 0.0.1.62 (2021-03-13)

* new `sim_joint_dist()` function to simulate the joint distribution of categories
* `sim_df()` no longer breaks if there are NAs in the DV columns
* `sim_df()` now has an option to include missing data, it simulates the joint distribution of missingness for each between-subject cell
* some functions (`sim_df()` and `messy()`) can choose columns with unquoted names now (e.g., `messy(mtcars, .5, mpg)`)
* `messy()` now takes a vector of proportions so you can simulate different amounts of missing data per selected column
* `sample_from_pop()` is now vectorised

# faux 0.0.1.61 (2021-02-06)

* `get_params()` doesn't require within and between set for data made with faux (that has a "design" attribute)
* fixed bug in `get_params()` where the var column was alphabetised, but the corresponding columns for the correlation table were in factor order
* several new distribution conversion functions and a vignette
* this version isn't on CRAN yet

# faux 0.0.1.6 (2021-01-05)

* `nested_list()` updated to match scienceverse version and handle edge cases better
* changes to tests for changes in CRAN development environments
* `rnorm_multi()` can get column names from mu, sd, or r names

# faux 0.0.1.5 (2020-09-11)

* Removed a test using markdown that failed on Solaris (causing faux to be pulled from CRAN) Back on CRAN!
* `seed` arguments reinstated as deprecated and produce a warning

# faux 0.0.1.4 (2020-08-12)

* Even more fixes for CRAN (on CRAN from 2009-08-19!)
* Removed all `seed` arguments (at the request of CRAN)
* Faster unit tests

# faux 0.0.1.3 (2020-08-10)

* More fixes for CRAN (fingers crossed!)
* Added a `seed` argument to `rnorm_multi()`
* User options are no longer changed in vignettes

# faux 0.0.1.2 (2020-08-03)

* Removed some dependencies
* Various bug fixes
* Submitting to CRAN again!

# faux 0.0.1.1 (2020-06-25)

* Still not accepted to CRAN (but haven't worked on it in a long time)
* `nested_list` function for printing nested lists in Rmd
* New `codebook` function and vignette
* Interactive mode for codebooks (experimental)
* Updated for new tidyverse versions

# faux 0.0.1.0 (2020-02-27)

* Submitting to CRAN (wish me luck!)

# faux 0.0.0.9019 (2020-02-26)

* [Andrew Heiss](https://github.com/andrewheiss) added `norm2beta` function
* Added contributors (Andrew Heiss and Anna Krystali)
* `trunc2norm` now works if `min` or `max` are omitted.

# faux 0.0.0.9017 (2019-12-26)

* Added `rep` argument to `sim_design()` and `sim_data()`. If rep > 1, returns a nested data frame with `rep` simulated datasets.
* Fixed warnings on `get_params()`
* Improved `make_id()` function

# faux 0.0.0.9016 (2019-12-10)

* Removed ANOVApower

# faux 0.0.0.9015 (2019-07-22)

* Added distribution conversion functions (still experimental)

# faux 0.0.0.9014 (2019-06-09)

* More flexible parameter specification
* Set plotting default as a global option `faux_options(plot = TRUE)`
* Minor bug fixes and efficiency increases

# faux 0.0.0.9013 (2019-05-27)

* multi-factor cell names now in a more intuitive order (**will break old code that inputs parameters with unnamed vectors**)
* More flexible plotting styles (violin, jitter, box, pointrange with SD or SE, and combos)
* Minor bug fixes

# faux 0.0.0.9012 (2019-05-22)

* More flexible plots and data plots
* Setting seed doesn't change the seed in your global environment
* Plot labels for dv and id (set them like `dv = list(colname = "Name for Plots")`)

# faux 0.0.0.9011 (2019-05-19)

* Changed default DV column name from 'val' to 'y'
* Changed default ID column name from 'sub_id' to 'id'
* `sim_design()` can take intercept-only designs
* `rnorm_multi()` can take vars = 1 for intercept-only designs
* `json_design()` to output or save design specs in JSON format
* Added functions for converting between distributions

# faux 0.0.0.9010

* Fixed bugs in interactive specification
* Added `messy()` (thanks Emily)
* Better interactive specification (thanks Daniël)

# faux 0.0.0.9009

* Bug fix for `long2wide()` (handle designs with no between or no within factors)
* `sim_df()` returns subject IDs and takes data in long format
* Renamed `check_sim_stats()` to `get_params()`, which now returns the design
* Added interactive option to `sim_design()`

# faux 0.0.0.9008

* added `sim_mixed_cc()` to simulate null cross-classified mixed effect designs by subject, item and error SDs
* `sim_design()`, `sim_df()`, `sim_mixed_cc()` and `sim_mixed_df()` take a `seed` argument now for reproducible datasets

# faux 0.0.0.9007

* Added a plot option to `check_design()` and `sim_design()`
* Design lists returned by `check_design()` have a more consistent format 
    - n, mu, and sd are all data frames with between-cells as rows and within-cells as columns
    - `within` and `between` are named lists; factors and labels are no longer separately named

# faux 0.0.0.9006

* Changes to argument order and names (more consistent, but may break old scripts)
* Updated vignettes

# faux 0.0.0.9005

* Bug fixes for `sim_design()` (failed when within or between factor number was 0)

# faux 0.0.0.9004

* Added a `NEWS.md` file to track changes to the package.
* Added `sim_design()` to simulate data for mixed ANOVA designs.

