# Resample a Learner on a Task

Runs a resampling (possibly in parallel): Repeatedly apply
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) `learner`
on a training set of
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) `task` to train a
model, then use the trained model to predict observations of a test set.
Training and test sets are defined by the
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
`resampling`.

## Usage

``` r
resample(
  task,
  learner,
  resampling,
  store_models = FALSE,
  store_backends = TRUE,
  encapsulate = NA_character_,
  allow_hotstart = FALSE,
  clone = c("task", "learner", "resampling"),
  unmarshal = TRUE,
  callbacks = NULL
)
```

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- learner:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

- resampling:

  ([Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)).

- store_models:

  (`logical(1)`)  
  Store the fitted model in the resulting object= Set to `TRUE` if you
  want to further analyse the models or want to extract information like
  variable importance.

- store_backends:

  (`logical(1)`)  
  Keep the
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  of the [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) in the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)?
  Set to `TRUE` if your performance measures require a
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md), or to analyse
  results more conveniently. Set to `FALSE` to reduce the file size and
  memory footprint after serialization. The current default is `TRUE`,
  but this eventually will be changed in a future release.

- encapsulate:

  (`character(1)`)  
  If not `NA`, enables encapsulation by setting the field
  `Learner$encapsulate` to one of the supported values: `"none"`
  (disable encapsulation), `"try"` (captures errors but output is
  printed to the console and not logged), `"evaluate"` (execute via
  [evaluate](https://CRAN.R-project.org/package=evaluate)) and `"callr"`
  (start in external session via
  [callr](https://CRAN.R-project.org/package=callr)). If `NA`,
  encapsulation is not changed, i.e. the settings of the individual
  learner are active. Additionally, if encapsulation is set to
  `"evaluate"` or `"callr"`, the fallback learner is set to the
  featureless learner if the learner does not already have a fallback
  configured.

- allow_hotstart:

  (`logical(1)`)  
  Determines if learner(s) are hot started with trained models in
  `$hotstart_stack`. See also
  [HotstartStack](https://mlr3.mlr-org.com/dev/reference/HotstartStack.md).

- clone:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Select the input objects to be cloned before proceeding by providing a
  set with possible values `"task"`, `"learner"` and `"resampling"` for
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md),
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) and
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md),
  respectively. Per default, all input objects are cloned.

- unmarshal:

  [`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md)  
  Whether to unmarshal learners that were marshaled during the
  execution. If `TRUE` all models are stored in unmarshaled form. If
  `FALSE`, all learners (that need marshaling) are stored in marshaled
  form.

- callbacks:

  (list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
  \| `NULL`)  
  Callbacks to be executed during the resampling process. See
  [CallbackResample](https://mlr3.mlr-org.com/dev/reference/CallbackResample.md)
  and
  [ContextResample](https://mlr3.mlr-org.com/dev/reference/ContextResample.md)
  for details.

## Value

[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

## Note

The fitted models are discarded after the predictions have been computed
in order to reduce memory consumption. If you need access to the models
for later analysis, set `store_models` to `TRUE`.

## Stochasticity

Note that uninstantiated
[`Resampling`](https://mlr3.mlr-org.com/dev/reference/Resampling.md)s
are instantiated on the task, making the procedure stochastic even in
case of a deterministic learner.

## Predict Sets

If you want to compare the performance of a learner on the training with
the performance on the test set, you have to configure the
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) to predict
on multiple sets by setting the field `predict_sets` to
`c("train", "test")` (default is `"test"`). Each set yields a separate
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
object during resampling. In the next step, you have to configure the
measures to operate on the respective Prediction object:

    m1 = msr("classif.ce", id = "ce.train", predict_sets = "train")
    m2 = msr("classif.ce", id = "ce.test", predict_sets = "test")

The (list of) created measures can finally be passed to `$aggregate()`
or `$score()`.

## Parallelization

This function can be parallelized with the
[future](https://CRAN.R-project.org/package=future) or
[mirai](https://CRAN.R-project.org/package=mirai) package. One job is
one resampling iteration. All jobs are send to an apply function from
[future.apply](https://CRAN.R-project.org/package=future.apply) or
[`mirai::mirai_map()`](https://mirai.r-lib.org/reference/mirai_map.html)
in a single batch. To select a parallel backend, use
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
To use `mirai`, call `mirai::daemons(.compute = "mlr3_parallelization")`
before calling this function. The `future` package guarantees
reproducible results independent of the parallel backend. The results of
`mirai` will not be the same but can be made reproducible by setting a
`seed` when calling
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
More on parallelization can be found in the book:
<https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html>

## Progress Bars

This function supports progress bars via the package
[progressr](https://CRAN.R-project.org/package=progressr). Simply wrap
the function call in
[`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html)
to enable them. Alternatively, call
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
with `global = TRUE` to enable progress bars globally. We recommend the
[progress](https://CRAN.R-project.org/package=progress) package as
backend which can be enabled with `progressr::handlers("progress")`.

## Logging

The [mlr3](https://CRAN.R-project.org/package=mlr3) uses the
[lgr](https://CRAN.R-project.org/package=lgr) package for logging.
[lgr](https://CRAN.R-project.org/package=lgr) supports multiple log
levels which can be queried with `getOption("lgr.log_levels")`.

To suppress output and reduce verbosity, you can lower the log from the
default level `"info"` to `"warn"`:

    lgr::get_logger("mlr3")$set_threshold("warn")

To get additional log output for debugging, increase the log level to
`"debug"` or `"trace"`:

    lgr::get_logger("mlr3")$set_threshold("debug")

To log to a file or a data base, see the documentation of
[lgr::lgr-package](https://s-fleck.github.io/lgr/reference/lgr-package.html).

## See also

- [`as_benchmark_result()`](https://mlr3.mlr-org.com/dev/reference/as_benchmark_result.md)
  to convert to a
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-resampling>

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

Other resample:
[`ResampleResult`](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)

## Examples

``` r
task = tsk("penguins")
learner = lrn("classif.rpart")
resampling = rsmp("cv")

# Explicitly instantiate the resampling for this task for reproducibility
set.seed(123)
resampling$instantiate(task)

rr = resample(task, learner, resampling)
print(rr)
#> 
#> ── <ResampleResult> with 10 resampling iterations ──────────────────────────────
#>   task_id    learner_id resampling_id iteration     prediction_test warnings
#>  penguins classif.rpart            cv         1 <PredictionClassif>        0
#>  penguins classif.rpart            cv         2 <PredictionClassif>        0
#>  penguins classif.rpart            cv         3 <PredictionClassif>        0
#>  penguins classif.rpart            cv         4 <PredictionClassif>        0
#>  penguins classif.rpart            cv         5 <PredictionClassif>        0
#>  penguins classif.rpart            cv         6 <PredictionClassif>        0
#>  penguins classif.rpart            cv         7 <PredictionClassif>        0
#>  penguins classif.rpart            cv         8 <PredictionClassif>        0
#>  penguins classif.rpart            cv         9 <PredictionClassif>        0
#>  penguins classif.rpart            cv        10 <PredictionClassif>        0
#>  errors
#>       0
#>       0
#>       0
#>       0
#>       0
#>       0
#>       0
#>       0
#>       0
#>       0

# Retrieve performance
rr$score(msr("classif.ce"))
#>      task_id    learner_id resampling_id iteration classif.ce
#>       <char>        <char>        <char>     <int>      <num>
#>  1: penguins classif.rpart            cv         1 0.00000000
#>  2: penguins classif.rpart            cv         2 0.00000000
#>  3: penguins classif.rpart            cv         3 0.02857143
#>  4: penguins classif.rpart            cv         4 0.00000000
#>  5: penguins classif.rpart            cv         5 0.17647059
#>  6: penguins classif.rpart            cv         6 0.05882353
#>  7: penguins classif.rpart            cv         7 0.05882353
#>  8: penguins classif.rpart            cv         8 0.02941176
#>  9: penguins classif.rpart            cv         9 0.11764706
#> 10: penguins classif.rpart            cv        10 0.05882353
#> Hidden columns: task, learner, resampling, prediction_test
rr$aggregate(msr("classif.ce"))
#> classif.ce 
#> 0.05285714 

# merged prediction objects of all resampling iterations
pred = rr$prediction()
pred$confusion
#>            truth
#> response    Adelie Chinstrap Gentoo
#>   Adelie       145         6      0
#>   Chinstrap      7        59      2
#>   Gentoo         0         3    122

# Repeat resampling with featureless learner
rr_featureless = resample(task, lrn("classif.featureless"), resampling)

# Convert results to BenchmarkResult, then combine them
bmr1 = as_benchmark_result(rr)
bmr2 = as_benchmark_result(rr_featureless)
print(bmr1$combine(bmr2))
#> 
#> ── <BenchmarkResult> of 20 rows with 2 resampling run ──────────────────────────
#>  nr  task_id          learner_id resampling_id iters warnings errors
#>   1 penguins       classif.rpart            cv    10        0      0
#>   2 penguins classif.featureless            cv    10        0      0
```
