# Create Evaluation Callback

Function to create a
[CallbackResample](https://mlr3.mlr-org.com/dev/reference/CallbackResample.md).
Predefined callbacks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html).

Evaluation callbacks are called at different stages of the resampling
process. Each stage is called once per resampling iteration. The stages
are prefixed with `on_resample_*`. The text in brackets indicates what
happens between the stages in the internal `workhorse()` function and
which accesses to the
[ContextResample](https://mlr3.mlr-org.com/dev/reference/ContextResample.md)
(`ctx`) are typical for the stage.

    Start Resampling Iteration on Worker
     - on_resample_begin
       (Split `ctx$task` into training and test set with `ctx$resampling` and `ctx$iteration`)
     - on_resample_before_train
       (Train the learner `ctx$learner` on training data)
     - on_resample_before_predict
       (Predict on predict sets and store prediction data `ctx$pdatas`)
     - on_resample_end
       (Erase model `ctx$learner$model` if requested and return results)
    End Resampling Iteration on Worker

The callback can store data in `ctx$learner$state` or `ctx$data_extra`.
The data in `ctx$data_extra` is stored in the
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
or
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).
See also the section on parameters for more information on the stages.

## Usage

``` r
callback_resample(
  id,
  label = NA_character_,
  man = NA_character_,
  on_resample_begin = NULL,
  on_resample_before_train = NULL,
  on_resample_before_predict = NULL,
  on_resample_end = NULL
)
```

## Arguments

- id:

  (`character(1)`)  
  Identifier for the new instance.

- label:

  (`character(1)`)  
  Label for the new instance.

- man:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

- on_resample_begin:

  (`function()`)  
  Stage called at the beginning of an evaluation. Called in
  `workhorse()` (internal).

- on_resample_before_train:

  (`function()`)  
  Stage called before training the learner. Called in `workhorse()`
  (internal).

- on_resample_before_predict:

  (`function()`)  
  Stage called before predicting. Called in `workhorse()` (internal).

- on_resample_end:

  (`function()`)  
  Stage called at the end of an evaluation. Called in `workhorse()`
  (internal).

## Details

When implementing a callback, each function must have two arguments
named `callback` and `context`. A callback can write data to the state
(`$state`), e.g. settings that affect the callback itself. We highly
discourage changing the task, learner and resampling objects via the
callback.

## Examples

``` r
learner = lrn("classif.rpart")
task = tsk("pima")
resampling = rsmp("cv", folds = 3)

# save selected features callback
callback = callback_resample("selected_features",
 on_resample_end = function(callback, context) {
    context$learner$state$selected_features = context$learner$selected_features()
  }
)

rr = resample(task, learner, resampling, callbacks = callback)
rr$learners[[1]]$state$selected_features
#> [1] "glucose"  "age"      "mass"     "pedigree" "pressure"

# holdout task callback
callback = callback_resample("holdout_task",
  on_resample_before_predict = function(callback, context) {
    pred = context$learner$predict(callback$state$task)
    context$data_extra = list(prediction_holdout = pred)
  }
)

task_holdout = tsk("pima")
splits = partition(task, 0.7)
task$filter(splits$train)
task_holdout$filter(splits$test)

callback$state$task = task_holdout

rr = resample(task, learner, resampling, callbacks = callback)
rr$data_extra
#> Key: <uhash, iteration>
#>                                   uhash iteration data_extra
#>                                  <char>     <int>     <list>
#> 1: 1de7231d-3c4f-4ba2-92ab-48c9f0c987d4         1  <list[1]>
#> 2: 1de7231d-3c4f-4ba2-92ab-48c9f0c987d4         2  <list[1]>
#> 3: 1de7231d-3c4f-4ba2-92ab-48c9f0c987d4         3  <list[1]>
```
