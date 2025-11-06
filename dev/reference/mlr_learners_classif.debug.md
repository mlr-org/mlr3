# Classification Learner for Debugging

A simple
[LearnerClassif](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
used primarily in the unit tests and for debugging purposes. If no
hyperparameter is set, it simply constantly predicts a randomly selected
label. The following hyperparameters trigger the following actions:

- error_predict::

  Probability to raise an exception during predict.

- error_train::

  Probability to raises an exception during train.

- message_predict::

  Probability to output a message during predict.

- message_train::

  Probability to output a message during train.

- predict_missing::

  Ratio of predictions which will be NA.

- predict_missing_type::

  To to encode missingness. “na” will insert NA values, “omit” will just
  return fewer predictions than requested.

- save_tasks::

  Saves input task in `model` slot during training and prediction.

- segfault_predict::

  Probability to provokes a segfault during predict.

- segfault_train::

  Probability to provokes a segfault during train.

- sleep_train::

  Function returning a single number determining how many seconds to
  sleep during `$train()`.

- sleep_predict::

  Function returning a single number determining how many seconds to
  sleep during `$predict()`.

- threads::

  Number of threads to use. Has no effect.

- warning_predict::

  Probability to signal a warning during predict.

- warning_train::

  Probability to signal a warning during train.

- x::

  Numeric tuning parameter. Has no effect.

- iter::

  Integer parameter for testing hotstarting.

- count_marshaling::

  If `TRUE`, `marshal_model` will increase the `marshal_count` by 1 each
  time it is called. The default is `FALSE`.

- check_pid::

  If `TRUE`, the `$predict()` function will throw an error if the model
  was not unmarshaled in the same session that is used for prediction.)

Note that segfaults may not be triggered reliably on your operating
system. Also note that if they work as intended, they will tear down
your R session immediately!

## Dictionary

This [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)
or with the associated sugar function
[`lrn()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_learners$get("classif.debug")
    lrn("classif.debug")

## Meta Information

- Task type: “classif”

- Predict Types: “response”, “prob”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|                      |           |         |             |                  |
|----------------------|-----------|---------|-------------|------------------|
| Id                   | Type      | Default | Levels      | Range            |
| error_predict        | numeric   | 0       |             | \\\[0, 1\]\\     |
| error_train          | numeric   | 0       |             | \\\[0, 1\]\\     |
| message_predict      | numeric   | 0       |             | \\\[0, 1\]\\     |
| message_train        | numeric   | 0       |             | \\\[0, 1\]\\     |
| predict_missing      | numeric   | 0       |             | \\\[0, 1\]\\     |
| predict_missing_type | character | na      | na, omit    | \-               |
| save_tasks           | logical   | FALSE   | TRUE, FALSE | \-               |
| segfault_predict     | numeric   | 0       |             | \\\[0, 1\]\\     |
| segfault_train       | numeric   | 0       |             | \\\[0, 1\]\\     |
| sleep_train          | untyped   | \-      |             | \-               |
| sleep_predict        | untyped   | \-      |             | \-               |
| threads              | integer   | \-      |             | \\\[1, \infty)\\ |
| warning_predict      | numeric   | 0       |             | \\\[0, 1\]\\     |
| warning_train        | numeric   | 0       |             | \\\[0, 1\]\\     |
| x                    | numeric   | \-      |             | \\\[0, 1\]\\     |
| iter                 | integer   | 1       |             | \\\[1, \infty)\\ |
| early_stopping       | logical   | FALSE   | TRUE, FALSE | \-               |
| count_marshaling     | logical   | FALSE   | TRUE, FALSE | \-               |
| check_pid            | logical   | TRUE    | TRUE, FALSE | \-               |
| config_error         | logical   | FALSE   | TRUE, FALSE | \-               |

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-learners>

- Package
  [mlr3learners](https://CRAN.R-project.org/package=mlr3learners) for a
  solid collection of essential learners.

- Package
  [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners) for
  more learners.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Learners](https://mlr3.mlr-org.com/dev/reference/Learner.md):
  [mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)

- `as.data.table(mlr_learners)` for a table of available
  [Learners](https://mlr3.mlr-org.com/dev/reference/Learner.md) in the
  running session (depending on the loaded packages).

- [mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines) to
  combine learners with pre- and postprocessing steps.

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

- [mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning) for tuning
  of hyperparameters,
  [mlr3tuningspaces](https://CRAN.R-project.org/package=mlr3tuningspaces)
  for established default tuning spaces.

Other Learner:
[`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md),
[`LearnerClassif`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md),
[`LearnerRegr`](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md),
[`mlr_learners`](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md),
[`mlr_learners_classif.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.featureless.md),
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.rpart.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
-\> `LearnerClassifDebug`

## Active bindings

- `marshaled`:

  (`logical(1)`)  
  Whether the learner has been marshaled.

- `internal_valid_scores`:

  Retrieves the internal validation scores as a named
  [`list()`](https://rdrr.io/r/base/list.html). Returns `NULL` if
  learner is not trained yet.

- `internal_tuned_values`:

  Retrieves the internally tuned values as a named
  [`list()`](https://rdrr.io/r/base/list.html). Returns `NULL` if
  learner is not trained yet.

- `validate`:

  How to construct the internal validation data. This parameter can be
  either `NULL`, a ratio in \$(0, 1)\$, `"test"`, or `"predefined"`.

## Methods

### Public methods

- [`LearnerClassifDebug$new()`](#method-LearnerClassifDebug-new)

- [`LearnerClassifDebug$marshal()`](#method-LearnerClassifDebug-marshal)

- [`LearnerClassifDebug$unmarshal()`](#method-LearnerClassifDebug-unmarshal)

- [`LearnerClassifDebug$importance()`](#method-LearnerClassifDebug-importance)

- [`LearnerClassifDebug$selected_features()`](#method-LearnerClassifDebug-selected_features)

- [`LearnerClassifDebug$clone()`](#method-LearnerClassifDebug-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-reset)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-train)
- [`mlr3::LearnerClassif$predict_newdata_fast()`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassifDebug$new()

------------------------------------------------------------------------

### Method `marshal()`

Marshal the learner's model.

#### Usage

    LearnerClassifDebug$marshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`marshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

------------------------------------------------------------------------

### Method `unmarshal()`

Unmarshal the learner's model.

#### Usage

    LearnerClassifDebug$unmarshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`unmarshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

------------------------------------------------------------------------

### Method `importance()`

Returns 0 for each feature seen in training.

#### Usage

    LearnerClassifDebug$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Always returns character(0).

#### Usage

    LearnerClassifDebug$selected_features()

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifDebug$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
learner = lrn("classif.debug")
learner$param_set$set_values(message_train = 1, save_tasks = TRUE)

# this should signal a message
task = tsk("penguins")
learner$train(task)
#> Message from classif.debug->train()
learner$predict(task)
#> 
#> ── <PredictionClassif> for 344 observations: ───────────────────────────────────
#>  row_ids     truth response
#>        1    Adelie   Adelie
#>        2    Adelie   Adelie
#>        3    Adelie   Adelie
#>      ---       ---      ---
#>      342 Chinstrap   Adelie
#>      343 Chinstrap   Adelie
#>      344 Chinstrap   Adelie

# task_train and task_predict are the input tasks for train() and predict()
names(learner$model)
#> [1] "response"      "pid"           "id"            "random_number"
#> [5] "iter"          "task_train"    "task_predict" 
```
