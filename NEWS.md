# mlr3 0.1.7

* Row ids now must be numeric. It was previously allowed to have character row
  ids, but this lead to confusion and unnecessary code bloat. Row identifiers
  (e.g., to be used in plots) can still be part of the task, with row role
  `"name"`.
* Removed deprecated `character()` -> object converters.
* Added new method `$filter()` to filter `ResampleResult`s to a subset of
  iterations.
* Empty test sets are now handled separately by learners (#421). An empty
  prediction object is returned for all learners.

# mlr3 0.1.6

* We have published an article about mlr3 in the Journal of Open Source
  Software: <https://joss.theoj.org/papers/10.21105/joss.01903>.
  See `citation("mlr3")` for the citation info.
* New method `Learner$reset()`.
* New method `BenchmarkResult$filter()`.
* Learners returned by `BenchmarkResult$learners` are reset to encourage the
  safer alternative `BenchmarkResult$score()` to access trained models.
* Fix ordering of levels in `PredictionClassif$set_threshold()` (triggered an
  assertion).

# mlr3 0.1.5

* Switched from package `Metrics` to package `mlr3measures`.
* Measures can now calculate all scores using micro or macro averaging (#400).
* Measures can now be configured to return a customizable performance score
  (instead of `NA`) in case the score cannot be calculated.
* Character columns are now treated differently from factor columns.
  In the long term, `character()` columns are supposed to store text.
* Fixed a bug triggered by integer grouping variables in `Task` (#396).
* `benchmark_grid()` now accepts instantiated resamplings under certain
  conditions.

# mlr3 0.1.4

* `Task$set_col_roles()` and `Task$set_row_roles()` are now deprecated.
  Instead it is recommended for now to work with the lists `Task$col_roles` and
  `Task$row_roles` directly.
* `Learner$predict_newdata()` now works without argument `task` if the learner
  has been fitted with `Learner$train()` (#375).
* Names of column roles have been unified (`"weights"`, `"label"`,
  `"stratify"` and `"groups"` have been renamed).
* Replaced `MeasureClassifF1` with `MeasureClassifFScore` and fixed a bug in the
  F1 performance calculation (#353). Thanks to @001ben for reporting.
* Stratification is now controlled via a task column role (was a parameter of
  class `Resampling` before).
* Added a S3 `predict()` method for class `Learner` to increase
  interoperability with other packages.
* Many objects now come with a `$help()` which opens the respective manual page.


# mlr3 0.1.3

* It is now possible to predict and score results on the training set or on both
  training and test set.
  Learners can be instructed to predict on multiple sets by setting
  `predict_sets` (default: `"test"`). Measures operate on all sets specified in
  their field `predict_sets` (default: `"test"`.

* `ResampleResult$prediction` and `ResampleResult$predictions()` are now methods
  instead of fields, and allow to extract predictions for different predict
  sets.

* `ResampleResult$performance()` has been renamed to `ResampleResult$score()`
  for consistency.

* `BenchmarkResult$performance()` has been renamed to `BenchmarkResult$score()`
  for consistency.

* Changed API for (internal) constructors accepting `paradox::ParamSet()`.
  Instead of passing the initial values separately, the initial values must now
  be set directly in the `ParamSet`.


# mlr3 0.1.2

* Deprecated support of automatically creating objects from strings.
  Instead, `mlr3` provides the following helper functions intended to ease the
  creation of objects stored in dictionaries:
  `tsk()`, `tgen()`, `lrn()`, `rsmp()`, `msr()`.

* `BenchmarkResult` now ensures that the stored `ResampleResult`s are in a
  persistent order. Thus, `ResampleResult`s can now be addressed by their
  position instead of their hash.

* New field `BenchmarkResult$n_resample_results`.

* New field `BenchmarkResult$hashes`.

* New method `Task$rename()`.

* New S3 generic `as_benchmark_result()`.

* Renamed `Generator` to `TaskGenerator`.

* Removed the control object `mlr_control()`.

* Removed `ResampleResult$combine()`.

* Removed `BenchmarkResult$best()`.


# mlr3 0.1.1

* Initial upload to CRAN.
