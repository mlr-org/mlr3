# mlr3 1.0.0

* BREAKING CHANGE: The mlr3 ecosystem has a base logger now which is named `mlr3`.
  The `mlr3/core` logger is a child of the `mlr3` logger and is used for logging messages from the `mlr3` package.
  Some extension packages have their own loggers which are children of the mlr3 logger e.g. mlr3/mlr3pipelines and mlr3/bbotk for tuning.
* BREAKING CHANGE: `weights` property and functionality is split into `weights_learner` and `weights_measure`:

  * `weights_learner`: Weights used during training by the Learner.
  * `weights_measure`: Weights used during scoring predictions via measures.

  Each of these can be disabled via the new field `use_weights` in `Learner` and `Measure` objects.
* feat: Add `$confusion_weighted` field to `PredictionClassif`.
* feat: Add `$weights` field to `Prediction`. It contains the `weights_measure` weights from the `Task` that was used for prediction.
* feat: Add `"macro_weighted"` option to `Measure$average` field.
* feat: `MeasureRegrRSQ` and `MeasureClassifCost` gain `"weights"` property.
* feat: `LearnerClassifFeatureless`, `LearnerRegrFeatureless`, `LearnerClassifDebug`, `LearnerRegrDebug` gain `"weights"` property.
* feat: `Learner` printer now prints information about encapsulation and weights use.
* feat: Add `score_roc_measures()` to score a prediction on various roc measures.
* feat: A better error message is thrown, which often happens when incorrectly configuring the `validate` field
  of a `GraphLearner`
* feat: Added method `$set_threshold()` to `BenchmarkResult` and `ResamplingResult`, which allows to set the threshold for the response prediction of classification learners, given they have output a probability prediction (#1270).
* feat: Added field `$uhash_table` to `BenchmarkResult` and functions `uhash()` and `uhashes()`
  to easily compute uhashes for given learner, task, or resampling ids (#1270).
* feat: You can now change the default predict type of classification learners to `"prob"` by setting
  the option `mlr3.prob_as_default` to `TRUE` (#1273).
* feat: `benchmark_grid()` will now throw a warning if you mix different predict types in the
  design (#1273).
* feat: Converting a `BenchmarkResult` to a `data.table` now includes the `task_id`, `learner_id`, and `resampling_id` columns (#1275).
* fix: Add missing parameters for `"regr.pinball"` and `"sim.phi"` measures.

# mlr3 0.23.0

* feat: Add new `col_role` offset in `Task` and offset `Learner` property.
  A warning is produced if a learner that doesn't support offsets is trained with a task that has an offset column.
* fix: The `$predict_newdata()` method of `Learner` now automatically conducts type conversions (#685).
* BREAKING_CHANGE: Predicting on a `Task` with the wrong column information is now an error and not a warning.
* Column names with UTF-8 characters are now allowed by default.
  The option `mlr3.allow_utf8_names` is removed.
* BREAKING CHANGE: `Learner$predict_types` is read-only now.
* docs: Clear up behavior of `Learner$predict_type` after training.
* feat: Add callbacks to `resample()` and `benchmark()`.
* fix: Internal tuning and validation now works when the model requires marshaling (#1256)

# mlr3 0.22.1

* fix: Extend `assert_measure()` with checks for trained models in `assert_scorable()`.

# mlr3 0.22.0

* fix: Quantiles must not ascend with probabilities.
* refactor: Replace `tsk("boston_housing")` with `tsk("california_housing")`.
* feat: Require unique learner ids in `benchmark_grid()`.
* BREAKING CHANGE: Remove ``$loglik()`` method from all learners.
* fix: Ignore `future.globals.maxSize` when `future::plan("sequential")` is used.
* feat: Add `$characteristics` field to `Task` to store additional information.

# mlr3 0.21.1

* feat: Throw warning when prediction and measure type do not match.
* fix: The `mlr_reflections` were broken when an extension package was not loaded on the workers.
  Extension packages must now register themselves in the `mlr_reflections$loaded_packages` field.

# mlr3 0.21.0

* BREAKING CHANGE: Deprecated `data_format` and `data_formats` for `Learner`, `Task`, and `DataBackend` classes.
* feat: The `partition()` function creates training, test and validation sets now.
* perf: Optimize the runtime of fixing factor levels.
* perf: Optimize the runtime of setting row roles.
* perf: Optimize the runtime of marshalling.
* perf: Optimize the runtime of `Task$col_info`.
* fix: column info is now checked for compatibility during `Learner$predict` (#943).
* BREAKING CHANGE: The predict time of the learner now stores the cumulative duration for all predict sets (#992).
* feat: `$internal_valid_task` can now be set to an `integer` vector.
* feat: Measures can now have an empty `$predict_sets` (#1094).
  This is relevant for measures that only extract information from the model of a learner (such as internal validation scores or AIC / BIC)
* BREAKING CHANGE: Deprecated the `$divide()` method
* fix: `Task$cbind()` now works with non-standard primary keys for `data.frames` (#961).
* fix: Triggering of fallback learner now has log-level `"info"` instead of `"debug"` (#972).
* feat: Added new measure `regr.pinball` here and in mlr3measures.
* feat: Added new measure `mu_auc` here and in mlr3measures.
* feat: Add option to calculate the mean of the true values on the train set in `msr("regr.rsq")`.
* feat: Default fallback learner is set when encapsulation is activated.
* feat: Learners `classif.debug` and `regr.debug` have new methods `$importance()` and `$selected_features()` for testing, also in downstream packages.
* feat: Create default fallback learner with `default_fallback()`.
* feat: Check column roles when using `$set_col_roles()` and `$col_roles`.
* fix: Add predict set to learner hash.
* BREAKING CHANGE: Encapsulation and the fallback learner are now set with the `$encapsulate(method, fallback)` method.
  The `$fallback` field is read-only now and the encapsulate status can be retrieved from the `$encapsulation` field.

# mlr3 0.20.2

* refactor: Move RhpcBLASctl to suggest.
* feat: Added resampling property `"primary_iters"`
* feat: Added possibility to access observation-wise losses via function `$obs_loss`.
  This is possible for `Prediction`,  `ResampleResult` and `BenchmarkResult`.
* feat: `Measure`s now also return a vector of numerics.

# mlr3 0.20.1
* feat: Add multiclass Matthews correlation coefficient `msr("classif.mcc")`.

# mlr3 0.20.0

* Added support for learner-internal validation and tuning.

# mlr3 0.19.0

* Added support for `"marshal"` property, which allows learners to process models so they can be serialized.
This happens automatically during `resample()` and `benchmark()`.
* Encapsulation methods use the same RNG state now.
* Fix missing values in `default_values.Learner()` function.
* Encapsulated error messages are now printed with the `lgr` package.

# mlr3 0.18.0

* Prepare compatibility with new paradox version.
* Dictionary conversion of `mlr_learners` respects prototype arguments recently added in mlr3misc.
* Skip unnecessary clone of learner's state in `resample()`.

# mlr3 0.17.2

* Skip new `data.table` tests on mac.

# mlr3 0.17.1

* Remove `data_prototype` when resampling from `learner$state` to reduce memory consumption.
* Reduce number of threads used by `data.table` and BLAS to 1 when running `resample()` or `benchmark()` in parallel.
* Optimize runtime of `resample()` and `benchmark()` by reducing the number of hashing operations.

# mlr3 0.17.0

* Learners cannot be added to the `HotstartStack` anymore when the model is missing.
* Learners bellow the `hotstart_threshold` are not added to the `HotstartStack` anymore.
* The `learner$state$train_time` in hotstarted learners is now only the time of the last training.
* Added debug messages to the hotstart stack.
* Fixed bug where the `HotstartStack` did not work with column roles set in the task.
* The `design` of `benchmark()` can now include parameter settings.
* Speed up resampling by removing unnecessary calls to `packageVersion()`.
* Fix boston housing data set.
* Export generic function `col_info` to allow adding new methods for backends.
* Task printer includes row roles now.
* Add `"mlr3.exec_chunk_bins"` option to split the resampling iterations into a number of bins.

# mlr3 0.16.1

* Function `data.table()` is now re-exported.
* Fixed a test which randomly failed.
* Improved documentation.
* Add encapsulation mode `"try"`, which works similar to `"none"` but captures errors


# mlr3 0.16.0

* Added argument `paired` to `benchmark_grid()` function, which can be used to create a benchmark design, where
  resamplings have been instantiated on tasks.
* Added S3 method for `ResultData` for `as_resample_result()` converter.
* Added S3 method for `list` for `as_resample_result()` converter.
* The featureless classification learner now returns proper probabilities
  (#918).


# mlr3 0.15.0

* Many returned tables are now assigned a class for a `print` method to make the output
  more readable.
* Fixed some typos

# mlr3 0.14.1

* Removed dependency on package `distr6`.
* Fixed reassembling of `GraphLearner`.
* Fixed bug where the measured elapsed time was 0:
  https://stackoverflow.com/questions/73797845/mlr3-benchmarking-with-elapsed-time-measure
* Fixed `as_prediction_classif()` for `data.frame()` input (#872).
* Improved the error message when predict type of fallback learner does not
  match the predict type of the learner (mlr-org/mlr3extralearners#241).
* The test set is now available to the `Learner` during train for early
  stopping.

# mlr3 0.14.0

* Added multiclass measures: `mauc_aunu`, `mauc_aunp`, `mauc_au1u`, `mauc_au1p`.
* Measure `classif.costs` does not require a `Task` anymore.
* New converter: `as_task_unsupervised()`
* Refactored the task types in `mlr_reflections`.


# mlr3 0.13.4

* Added new options for parallelization (`"mlr3.exec_random"` and
  `"mlr3.exec_chunk_size"`). These options are passed down to the respective map
  functions in package `future.apply`.
* Fixed runtime measures depending on specific predict types (#832).
* Added `head()` and `tail()` methods for `Task`.
* Improved printing of multiple objects.


# mlr3 0.13.3

* Most objects now have a new (optional) field `label`, i.e. `Task`,
  `TaskGenerator`, `Learner`, `Resampling`, and `Measure`.
* `as.data.table()` methods for objects of class `Dictonary` have been extended
  with additional columns.
* `as_task_classif.formula()` and `as_task_regr.formula()` now remove additional
  atrributes attached to the data which caused some some learners to break.
* Packages are now loaded prior to calling the `$train()` and `$predict()`
  methods of a `Learner`. This ensures that package loading errors are properly
  propagated and not affected by encapsulation (#771).


# mlr3 0.13.2

* Setting a fallback learner for a learner with encapsulation in its default
  settings now automatically sets encapsulation to `"evaluate"` (#763).
* `as_task_classif()` and `as_task_regr()` now support the construction of tasks
  using the formula interface, e.g. `as_task_regr(mpg ~ ., data = mtcars)`
  (#761).
* Added `default_values()` function to extract parameter default values from
  `Learner` objects.
* The row role `"validation"` has been renamed to `"holdout"`.
  In the next release, `mlr3` will start switching to the now more common terms
  `"train"`/`"validation"` instead of `"train"`/`"test"` for the sets created
  during resampling.


# mlr3 0.13.1

* Improved performance for many operations on `ResampleResult` and
  `BenchmarkResult`.
* `resample()` and `benchmark()` got a new argument `clone` to control which
  objects to clone before performing computations.
* Tasks are checked for infinite values during the conversion from `data.frame`
  to `Task` in `as_task_classif()` and `as_task_regr()`. A warning is signaled
  if any column contains infinite values.

# mlr3 0.13.0

* Learners which are capable of resuming/continuing (e.g.,
  learner `(classif|regr|surv).xgboost` with hyperparameter `nrounds` updated)
  can now optionally store a stack of trained learners to be used to hotstart
  their training. Note that this feature is still somewhat experimental.
  See `HotstartStack` and #719.
* New measures to score similarity of selected feature sets:
  `sim.jaccard` (Jaccard Index) and `sim.phi` (Phi coefficient) (#690).
* `predict_newdata()` now also supports `DataBackend` as input.
* New function `install_pkgs()` to install required packages. This generic works
  for all objects with a `packages` field as well as `ResampleResult` and
  `BenchmarkResult` (#728).
* New learner `regr.debug` for debugging.
* New `Task` method `$set_levels()` to control how data with factor columns
  is returned, independent of the used `DataBackend`.
* Measures now return `NA` if prerequisite are not met (#699).
  This allows to conveniently score your experiments with  multiple measures
  having different requirements.
* Feature names may no longer contain the special character `%`.

# mlr3 0.12.0

* New method to assign labels to columns in tasks: `Task$label()`.
  These will be used in visualizations in the future.
* New method to add stratification variables: `Task$add_strata()`.
* New helper function `partition()` to split a task into a training and test
  set.
* New standardized getter `loglik()` for class `Learner`.
* New measures `"aic"` and `"bic"` to compute the Akaike Information Criterion
  or the Bayesian Information Criterion, respectively.
* New Resampling method: `ResamplingCustomCV`. Creates a custom resampling split
  based on the levels of a user-provided factor variable.
* New argument `encapsulate` for `resample()` and `benchmark()` to conveniently
  enable encapsulation and also set the fallback learner to the
  featureless learner. This is simply for convenience, configuring each learner
  individually is still possible and allows a more fine-grained control (#634,
  #642).
* New field `parallel_predict` for `Learner` to enable parallel predictions via
  the future backend. This currently is only enabled while calling the
  `$predict()` or `$predict_newdata` methods and is disabled during `resample()`
  and `benchmark()` where you have other means to parallelize.
* Deprecated public (and already documented as internal) field `$data` in
  `ResampleResult` and `BenchmarkResult` to simplify the API and avoid
  confusion. The converter `as.data.table()` can be used instead to access the
  internal data.
* Measures now have formal hyperparameters. A popular example where this is
  required is the F1 score, now implemented with customizable `beta`.
* Changed default of argument `ordered` in `Task$data()` from `TRUE` to `FALSE`.
* Fixed getter `ResamplingRepeatedCV$folds()` (#643).
* Fixed hashing of some measures.
* Removed experimental column role `uri`. This role be split up into multiple
  roles by the `mlr3keras` package.
* Update paramtest to error on extra parameters


# mlr3 0.11.0

* Added a `as.data.table.Resampling` method.
* Renamed column `"row_id"` to `"row_ids"` in the `as.data.table()` methods
  for `PredictionClassif` and `PredictionRegr` (#547).
* Added converters `as_prediction_classif()` and `as_prediction_regr()` to
  reverse the operation of `as.data.table.PredictionClassif()` and
  `as.data.table.PredictionRegr()`.
* Specifying a weight column during `learner$predict_newdata()` is not mandatory
  anymore (#563).
* `Task$data()` defaults to return only active rows and columns, instead of
  asserting to only return rows and columns. As a result, the `$data()` method
  can now also be used to query inactive rows and cols from the `DataBackend`.
* New (experimental) column role `uri` which is intended to point to external
  resources, e.g.  images on the file system.
* New helper `set_threads()` to control the number of threads during calls to
  external packages. All objects will be migrated to have threading disabled in
  their defaults to avoid conflicting parallelization techniques (#605).
* New option `mlr3.debug`: avoid calls to `future` in `resample()` and
  `benchmark()` to improve the readability of tracebacks.
* New experimental option `mlr3.allow_utf8_names`: allow non-ascii characters in
  column names in tasks.


# mlr3 0.10.0

* Result containers `ResampleResult` and `BenchmarkResult` now optionally remove
  the DataBackend of the Tasks in order to reduce file size and memory
  footprint after serialization.  To remove the backends from the containers,
  set `store_backends` to `FALSE` in `resample()` or `benchmark()`,
  respectively. Note that this behavior will eventually will be the default for
  future releases.
* Prediction objects generated by `Learner$predict_newdata()` now have row ids
  starting from 1 instead auto incremented row ids of the training task.
* `as.data.table.DictionaryTasks` now returns an additional column `properties`.
* Added flag `conditions` to `ResampleResult$score()` and
  `BenchmarkResult$score()` to allow to work with failing learners more
  conveniently.

# mlr3 0.9.0

* New methods for `Task`: `$set_col_roles` and `$set_row_roles` as a replacement
  for the deprecated and less flexible `$set_col_role` and `$set_row_role`.
* Learners can now have a timeout (#556).
* Removed S3 method `friedman.test.BenchmarkResult()` in favor of the new
  `mlr3benchmark` package.

# mlr3 0.8.0

* `MeasureOOBError` now has set property `minimize` to `TRUE`.
* New learner property `"featureless"` to tag learners which can operate on
  featureless tasks.
* Fixed [ResampleResult] ignoring argument `predict_sets` for returned
  [Prediction] objects.
* Compatibility with new version of `lgr`.

# mlr3 0.7.0

* Updated properties of featureless learners to apply it on all feature types
  (did not work on POSIXct columns).
* Fixed measures being calculated as `NaN` for `BenchmarkResult` for resamplings
  with a single iteration (#551).
* Fixed a bug where a broken heuristic disabled nested parallelization via
  package `future` (mlr3tuning#270).
* `ResampleResult` and `BenchmarkResult` now share a common interface to store
  the experiment results. Manual construction is still possible with helper
  function `as_result_data()`
* Fixed deep cloning of `ResamplingCV` and `ResamplingRepeatedCV`.
* New measure `classif.prauc` (area under precision-recall curve).
* Removed dependency on orphaned package `bibtex`.

# mlr3 0.6.0

* Compact in-memory representation of R6 objects to save space when
  saving objects via `saveRDS()` or `serialize()`.
* Objects in containers like `ResampleResult` or `BenchmarkResult` are now
  de-duplicated for an optimized serialization.
* Fixed data set `breast_cancer`: all factor features are now
  correctly stored as ordered factors.
* Added a new utility function `convert_task()`.

# mlr3 0.5.0

* Added classification task `breast_cancer`
* Added `ResamplingLOO` for leave-one-out resampling.
* Regression now supports predict type `"distr"` using the `distr6` package.
* Fixed `ResamplingBootstrap` in combination with grouping (#514).
* Fixed plot method of `TaskGeneratorMoons`.
* Added hyperparameter `keep_model` to learners `"classif.rpart"` and
  `"regr.rpart"`.

# mlr3 0.4.0

* Added new task generators (`"cassini"`, `"circle"`, `"simplex"`, `"spirals"`,
  and `"moons"`).
* Columns in tasks generated by task generators have been renamed to be more
  consistent.
* Added a `plot()` method for most task generators.
* Corrected data in task `german_credit` (#514).

# mlr3 0.3.0

* Package `future.apply` is now imported (instead of suggested).
  This is necessary to ensure reproducibility: This way exactly the same result
  is calculated, independent of the parallel backend.
* Fixed a bug where prediction on new data for a task with blocking information
  raised an exception (#496).
* New binding: `Task$order`.

# mlr3 0.2.0

* Some handy cheat sheets can now be downloaded from the project homepage.
* Added new measures `classif.bbrier` (binary Brier score) and `classif.mbrier`
  (multi-class Brier score).
* Added new Resampling: `ResamplingInsample`.
* Added base class for unsupervised tasks: `TaskUnsupervised`.

# mlr3 0.1.8

* Added S3 methods to combine `ResampleResult`s and `BenchmarkResult`s with
  `c()`.
* Fixed a bug where automatic generation of row ids could lead to duplicated ids
  via `Task$predict_newdata()`/`Task$rbind()` (#423).

# mlr3 0.1.7

* Switched to new `roxygen2` documentation format for R6 classes.

* `resample()` and `benchmark()` now support progress bars via the package
  `progressr`.

* Row ids now must be numeric. It was previously allowed to have character row
  ids, but this lead to confusion and unnecessary code bloat. Row identifiers
  (e.g., to be used in plots) can still be part of the task, with row role
  `"name"`.

* Row names can now be queried with `Task$row_names`.

* `DataBackendMatrix` now supports to store an optional (numeric) dense part.

* Added new method `$filter()` to filter `ResampleResult`s to a subset of
  iterations.

* Removed deprecated `character()` -> object converters.

* Empty test sets are now handled separately by learners (#421). An empty
  prediction object is returned for all learners.

* The internal train and predict function of `Learner` now should be implemented
  as private method: instead of public methods `train_internal` and
  `predict_internal`, private methods `.train` and `.predict` are now
  encouraged.

* It is now encouraged to move some internal methods from public to private:
  - `Learner$train_internal` should now be private method `$.train`.
  - `Learner$predict_internal` should now be private method `$.predict`.
  - `Measure$score_internal` should now be private method `$.score`.
  The public methods will be deprecated in a future release.

* Removed arguments from the constructor of measures `classif.debug` and
  `classif.costs`. These can be set directly by `msr()`.


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
  their field `predict_sets` (default: `"test"`).

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
