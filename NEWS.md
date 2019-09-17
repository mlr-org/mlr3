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
