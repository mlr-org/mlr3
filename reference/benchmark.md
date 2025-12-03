# Benchmark Multiple Learners on Multiple Tasks

Runs a benchmark on arbitrary combinations of tasks
([Task](https://mlr3.mlr-org.com/reference/Task.md)), learners
([Learner](https://mlr3.mlr-org.com/reference/Learner.md)), and
resampling strategies
([Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)),
possibly in parallel.

For large-scale benchmarking we recommend to use the
[mlr3batchmark](https://CRAN.R-project.org/package=mlr3batchmark)
package. This package runs benchmark experiments on high-performance
computing clusters and handles failed experiments.

## Usage

``` r
benchmark(
  design,
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

- design:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Data frame (or
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  with three columns: "task", "learner", and "resampling". Each row
  defines a resampling by providing a
  [Task](https://mlr3.mlr-org.com/reference/Task.md),
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md) and an
  instantiated
  [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)
  strategy. The helper function
  [`benchmark_grid()`](https://mlr3.mlr-org.com/reference/benchmark_grid.md)
  can assist in generating an exhaustive design (see examples) and
  instantiate the
  [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)s per
  [Task](https://mlr3.mlr-org.com/reference/Task.md). Additionally, you
  can set the additional column 'param_values', see
  [`benchmark_grid()`](https://mlr3.mlr-org.com/reference/benchmark_grid.md).

- store_models:

  (`logical(1)`)  
  Store the fitted model in the resulting object= Set to `TRUE` if you
  want to further analyse the models or want to extract information like
  variable importance.

- store_backends:

  (`logical(1)`)  
  Keep the
  [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md) of
  the [Task](https://mlr3.mlr-org.com/reference/Task.md) in the
  [ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)?
  Set to `TRUE` if your performance measures require a
  [Task](https://mlr3.mlr-org.com/reference/Task.md), or to analyse
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
  [HotstartStack](https://mlr3.mlr-org.com/reference/HotstartStack.md).

- clone:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Select the input objects to be cloned before proceeding by providing a
  set with possible values `"task"`, `"learner"` and `"resampling"` for
  [Task](https://mlr3.mlr-org.com/reference/Task.md),
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md) and
  [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md),
  respectively. Per default, all input objects are cloned.

- unmarshal:

  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.md)  
  Whether to unmarshal learners that were marshaled during the
  execution. If `TRUE` all models are stored in unmarshaled form. If
  `FALSE`, all learners (that need marshaling) are stored in marshaled
  form.

- callbacks:

  (List of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))  
  Callbacks to be executed during the resampling process. See
  [CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)
  and
  [ContextResample](https://mlr3.mlr-org.com/reference/ContextResample.md)
  for details.

## Value

[BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.md).

## Note

The fitted models are discarded after the predictions have been scored
in order to reduce memory consumption. If you need access to the models
for later analysis, set `store_models` to `TRUE`.

## Stochasticity

Note that uninstantiated
[`Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md)s are
instantiated on the task, making the function stochastic even in case of
deterministic learners.

## Predict Sets

If you want to compare the performance of a learner on the training with
the performance on the test set, you have to configure the
[Learner](https://mlr3.mlr-org.com/reference/Learner.md) to predict on
multiple sets by setting the field `predict_sets` to
`c("train", "test")` (default is `"test"`). Each set yields a separate
[Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) object
during resampling. In the next step, you have to configure the measures
to operate on the respective Prediction object:

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

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-benchmarking>

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- [mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark) for
  post-hoc analysis of benchmark results.

Other benchmark:
[`BenchmarkResult`](https://mlr3.mlr-org.com/reference/BenchmarkResult.md),
[`benchmark_grid()`](https://mlr3.mlr-org.com/reference/benchmark_grid.md)

## Examples

``` r
# benchmarking with benchmark_grid()
tasks = lapply(c("penguins", "sonar"), tsk)
learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
resamplings = rsmp("cv", folds = 3)

design = benchmark_grid(tasks, learners, resamplings)
print(design)
#>        task             learner resampling
#>      <char>              <char>     <char>
#> 1: penguins classif.featureless         cv
#> 2: penguins       classif.rpart         cv
#> 3:    sonar classif.featureless         cv
#> 4:    sonar       classif.rpart         cv

set.seed(123)
bmr = benchmark(design)

## Data of all resamplings
head(as.data.table(bmr))
#>                                   uhash                   task
#>                                  <char>                 <list>
#> 1: 003057dc-396b-46ba-b13c-3db4f530b135 <TaskClassif:penguins>
#> 2: 003057dc-396b-46ba-b13c-3db4f530b135 <TaskClassif:penguins>
#> 3: 003057dc-396b-46ba-b13c-3db4f530b135 <TaskClassif:penguins>
#> 4: 057fe396-253b-48ca-8102-849fb0eca9c5 <TaskClassif:penguins>
#> 5: 057fe396-253b-48ca-8102-849fb0eca9c5 <TaskClassif:penguins>
#> 6: 057fe396-253b-48ca-8102-849fb0eca9c5 <TaskClassif:penguins>
#>                                            learner     resampling iteration
#>                                             <list>         <list>     <int>
#> 1: <LearnerClassifFeatureless:classif.featureless> <ResamplingCV>         1
#> 2: <LearnerClassifFeatureless:classif.featureless> <ResamplingCV>         2
#> 3: <LearnerClassifFeatureless:classif.featureless> <ResamplingCV>         3
#> 4:             <LearnerClassifRpart:classif.rpart> <ResamplingCV>         1
#> 5:             <LearnerClassifRpart:classif.rpart> <ResamplingCV>         2
#> 6:             <LearnerClassifRpart:classif.rpart> <ResamplingCV>         3
#>             prediction  task_id          learner_id resampling_id
#>                 <list>   <char>              <char>        <char>
#> 1: <PredictionClassif> penguins classif.featureless            cv
#> 2: <PredictionClassif> penguins classif.featureless            cv
#> 3: <PredictionClassif> penguins classif.featureless            cv
#> 4: <PredictionClassif> penguins       classif.rpart            cv
#> 5: <PredictionClassif> penguins       classif.rpart            cv
#> 6: <PredictionClassif> penguins       classif.rpart            cv

## Aggregated performance values
aggr = bmr$aggregate()
print(aggr)
#>       nr  task_id          learner_id resampling_id iters classif.ce
#>    <int>   <char>              <char>        <char> <int>      <num>
#> 1:     1 penguins classif.featureless            cv     3  0.5580219
#> 2:     2 penguins       classif.rpart            cv     3  0.0756420
#> 3:     3    sonar classif.featureless            cv     3  0.4661146
#> 4:     4    sonar       classif.rpart            cv     3  0.2836439
#> Hidden columns: resample_result

## Extract predictions of first resampling result
rr = aggr$resample_result[[1]]
as.data.table(rr$prediction())
#>      row_ids     truth response
#>        <int>    <fctr>   <fctr>
#>   1:       1    Adelie   Adelie
#>   2:       3    Adelie   Adelie
#>   3:       4    Adelie   Adelie
#>   4:       9    Adelie   Adelie
#>   5:      11    Adelie   Adelie
#>  ---                           
#> 340:     326 Chinstrap   Adelie
#> 341:     327 Chinstrap   Adelie
#> 342:     329 Chinstrap   Adelie
#> 343:     338 Chinstrap   Adelie
#> 344:     342 Chinstrap   Adelie

# Benchmarking with a custom design:
# - fit classif.featureless on penguins with a 3-fold CV
# - fit classif.rpart on sonar using a holdout
tasks = list(tsk("penguins"), tsk("sonar"))
learners = list(lrn("classif.featureless"), lrn("classif.rpart"))
resamplings = list(rsmp("cv", folds = 3), rsmp("holdout"))

design = data.table::data.table(
  task = tasks,
  learner = learners,
  resampling = resamplings
)

## Instantiate resamplings
design$resampling = Map(
  function(task, resampling) resampling$clone()$instantiate(task),
  task = design$task, resampling = design$resampling
)

## Run benchmark
bmr = benchmark(design)
print(bmr)
#> 
#> ── <BenchmarkResult> of 4 rows with 2 resampling run ───────────────────────────
#>  nr  task_id          learner_id resampling_id iters warnings errors
#>   1 penguins classif.featureless            cv     3        0      0
#>   2    sonar       classif.rpart       holdout     1        0      0

## Get the training set of the 2nd iteration of the featureless learner on penguins
rr = bmr$aggregate()[learner_id == "classif.featureless"]$resample_result[[1]]
rr$resampling$train_set(2)
#>   [1]   5   7   8   9  12  13  17  19  22  25  28  35  36  40  46  48  49  50
#>  [19]  52  53  54  60  61  62  63  67  69  72  73  74  75  76  78  81  84  85
#>  [37]  88  92  97 101 103 104 109 110 114 119 122 127 129 130 131 136 145 147
#>  [55] 156 160 162 163 166 170 172 173 177 179 180 184 185 188 190 191 193 194
#>  [73] 205 212 213 217 218 221 228 229 233 234 238 239 245 250 252 255 256 258
#>  [91] 259 263 264 267 271 278 281 282 287 291 295 296 297 300 302 307 309 317
#> [109] 319 321 325 327 331 337 341   3   4  10  11  15  23  24  31  32  38  39
#> [127]  41  42  43  45  56  57  58  64  68  77  79  87  93  99 100 102 111 112
#> [145] 113 115 118 120 124 126 128 132 133 134 135 137 142 143 146 148 149 151
#> [163] 153 154 155 159 161 164 169 171 174 182 186 189 195 196 197 199 200 203
#> [181] 204 207 210 215 219 220 223 224 226 227 230 240 242 243 246 247 251 253
#> [199] 261 266 269 273 274 275 276 277 279 284 286 288 289 290 292 293 301 306
#> [217] 308 313 314 315 316 318 323 330 335 336 338 343 344
```
