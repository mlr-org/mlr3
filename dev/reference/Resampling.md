# Resampling Class

This is the abstract base class for resampling objects like
[ResamplingCV](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_cv.md)
and
[ResamplingBootstrap](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_bootstrap.md).

The objects of this class define how a task is partitioned for
resampling (e.g., in
[`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md) or
[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md)),
using a set of hyperparameters such as the number of folds in
cross-validation.

Resampling objects can be instantiated on a
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md), which applies
the strategy on the task and manifests in a fixed partition of `row_ids`
of the [Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

Predefined resamplings are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md),
e.g.
[`cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_cv.md) or
[`bootstrap`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_bootstrap.md).

## Stochasticity & Reproducibility

The `Resampling` class only defines an abstract resampling strategy.
Concrete data splits are obtained by calling `$instantiate()` on a
[`Task`](https://mlr3.mlr-org.com/dev/reference/Task.md). To ensure
reproducibility of results, you need to call `set.seed` before doing so.
Note that
[`benchmark_grid`](https://mlr3.mlr-org.com/dev/reference/benchmark_grid.md)
internally does instantiate resamplings, so you need to set the seed
before calling it.

## Stratification

All derived classes support stratified sampling. The stratification
variables are assumed to be discrete and must be stored in the
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) with column role
`"stratum"`. In case of multiple stratification variables, each
combination of the values of the stratification variables forms a
strata.

First, the observations are divided into subpopulations based one or
multiple stratification variables (assumed to be discrete), c.f.
`task$strata`.

Second, the sampling is performed in each of the `k` subpopulations
separately. Each subgroup is divided into `iter` training sets and
`iter` test sets by the derived `Resampling`. These sets are merged
based on their iteration number: all training sets from all
subpopulations with iteration 1 are combined, then all training sets
with iteration 2, and so on. Same is done for all test sets. The merged
sets can be accessed via `$train_set(i)` and `$test_set(i)`,
respectively. Note that this procedure can lead to set sizes that are
slightly different from those without stratification.

## Grouping / Blocking

All derived classes support grouping of observations. The grouping
variable is assumed to be discrete and must be stored in the
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) with column role
`"group"`.

Observations in the same group are treated like a "block" of
observations which must be kept together. These observations either all
go together into the training set or together into the test set.

The sampling is performed by the derived Resampling on the grouping
variable. Next, the grouping information is replaced with the respective
row ids to generate training and test sets. The sets can be accessed via
`$train_set(i)` and `$test_set(i)`, respectively.

## Inheriting

It is possible to overwrite both `private$.get_instance()` to have full
control, or only `private$.sample()` when one wants to use the
pre-defined mechanism for stratification and grouping.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-resampling>

- Package
  [mlr3spatiotempcv](https://CRAN.R-project.org/package=mlr3spatiotempcv)
  for spatio-temporal resamplings.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of Resamplings:
  [mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md)

- `as.data.table(mlr_resamplings)` for a table of available Resamplings
  in the running session (depending on the loaded packages).

- [mlr3spatiotempcv](https://CRAN.R-project.org/package=mlr3spatiotempcv)
  for additional Resamplings for spatio-temporal tasks.

Other Resampling:
[`mlr_resamplings`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md),
[`mlr_resamplings_bootstrap`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_bootstrap.md),
[`mlr_resamplings_custom`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_custom.md),
[`mlr_resamplings_custom_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_custom_cv.md),
[`mlr_resamplings_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_cv.md),
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_subsampling.md)

## Public fields

- `instance`:

  (any)  
  During `instantiate()`, the instance is stored in this slot in an
  arbitrary format. Note that if a grouping variable is present in the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md), a Resampling
  may operate on the group ids internally instead of the row ids (which
  may lead to confusion).

  It is advised to not work directly with the `instance`, but instead
  only use the getters `$train_set()` and `$test_set()`.

## Active bindings

- `id`:

  (`character(1)`)  
  Identifier of the object. Used in tables, plot and text output.

- `is_instantiated`:

  (`logical(1)`)  
  Is `TRUE` if the resampling has been instantiated.

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object. If the object has not been
  instantiated yet, `NA_character_` is returned. The hash is calculated
  based on the class name, the id, the parameter set, and the instance.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `task_hash`:

  (`character(1)`)  
  The hash of the [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)
  which was passed to `r$instantiate()`.

- `task_row_hash`:

  (`character(1)`)  
  The hash of the row ids of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) which was
  passed to `r$instantiate()`.

- `task_nrow`:

  (`integer(1)`)  
  The number of observations of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) which was
  passed to `r$instantiate()`.

- `duplicated_ids`:

  (`logical(1)`)  
  If `TRUE`, duplicated rows can occur within a single training set or
  within a single test set. E.g., this is `TRUE` for Bootstrap, and
  `FALSE` for cross-validation. Only used internally.

- `man`:

  (`character(1)` \| `NULL`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. Defaults to `NA`, but can be set by child classes.

## Methods

### Public methods

- [`Resampling$new()`](#method-Resampling-initialize)

- [`Resampling$format()`](#method-Resampling-format)

- [`Resampling$print()`](#method-Resampling-print)

- [`Resampling$help()`](#method-Resampling-help)

- [`Resampling$instantiate()`](#method-Resampling-instantiate)

- [`Resampling$train_set()`](#method-Resampling-train_set)

- [`Resampling$test_set()`](#method-Resampling-test_set)

- [`Resampling$clone()`](#method-Resampling-clone)

------------------------------------------------------------------------

### `Resampling$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Resampling$new(
      id,
      param_set = ps(),
      duplicated_ids = FALSE,
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `duplicated_ids`:

  (`logical(1)`)  
  Set to `TRUE` if this resampling strategy may have duplicated row ids
  in a single training set or test set.

  Note that this object is typically constructed via a derived classes,
  e.g.
  [ResamplingCV](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_cv.md)
  or
  [ResamplingHoldout](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_holdout.md).

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### `Resampling$format()`

Helper for print outputs.

#### Usage

    Resampling$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### `Resampling$print()`

Printer.

#### Usage

    Resampling$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### `Resampling$help()`

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Resampling$help()

------------------------------------------------------------------------

### `Resampling$instantiate()`

Materializes fixed training and test splits for a given task and stores
them in `r$instance` in an arbitrary format.

#### Usage

    Resampling$instantiate(task)

#### Arguments

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  Task used for instantiation.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keep the
object in its previous state.

#### Examples

    task = tsk("penguins")
    resampling = rsmp("holdout")
    resampling$instantiate(task)

------------------------------------------------------------------------

### `Resampling$train_set()`

Returns the row ids of the i-th training set.

#### Usage

    Resampling$train_set(i)

#### Arguments

- `i`:

  (`integer(1)`)  
  Iteration.

#### Returns

([`integer()`](https://rdrr.io/r/base/integer.html)) of row ids.

#### Examples

    task = tsk("penguins")
    resampling = rsmp("holdout")$instantiate(task)
    resampling$train_set(1)

------------------------------------------------------------------------

### `Resampling$test_set()`

Returns the row ids of the i-th test set.

#### Usage

    Resampling$test_set(i)

#### Arguments

- `i`:

  (`integer(1)`)  
  Iteration.

#### Returns

([`integer()`](https://rdrr.io/r/base/integer.html)) of row ids.

#### Examples

    task = tsk("penguins")
    resampling = rsmp("holdout")$instantiate(task)
    resampling$test_set(1)

------------------------------------------------------------------------

### `Resampling$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Resampling$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
r = rsmp("subsampling")

# Default parametrization
r$param_set$values
#> $ratio
#> [1] 0.6666667
#> 
#> $repeats
#> [1] 30
#> 

# Do only 3 repeats on 10% of the data
r$param_set$set_values(ratio = 0.1, repeats = 3)
r$param_set$values
#> $ratio
#> [1] 0.1
#> 
#> $repeats
#> [1] 3
#> 

# Instantiate on penguins task
task = tsk("penguins")
r$instantiate(task)

# Extract train/test sets
train_set = r$train_set(1)
print(train_set)
#>  [1] 339  58 251 232  16 266 315 319 148  18  38 231 287 290 117  76  28 178 156
#> [20] 172   7 234 333  93  74  46  32  85  95 186 147 322  73 136
intersect(train_set, r$test_set(1))
#> integer(0)

# Another example: 10-fold CV
r = rsmp("cv")$instantiate(task)
r$train_set(1)
#>   [1]   6  12  41  50  53  58  76  79  80  81  83  87 107 120 121 128 134 143
#>  [19] 154 174 175 181 183 190 195 198 199 224 230 239 278 289 299 334 336   4
#>  [37]  10  31  38  54  66  69  75  82  89  92  94 100 126 141 152 153 157 177
#>  [55] 187 202 209 229 233 238 245 281 290 291 300 301 332 335 339 343  15  16
#>  [73]  21  24  25  27  30  46  57  63  78  98  99 108 117 118 122 144 149 156
#>  [91] 158 164 204 225 247 257 258 264 277 287 305 312 315 337 340  14  20  22
#> [109]  32  44  65  67  72  85 116 123 131 146 150 161 173 179 200 212 216 222
#> [127] 242 267 269 272 275 280 282 283 294 295 306 307 310   1   2  29  49  59
#> [145]  68  71  74  88  93  96  97 102 111 127 138 139 151 176 185 188 189 193
#> [163] 196 218 226 231 236 265 285 288 293 330 331  13  18  19  36  39  52 101
#> [181] 132 145 155 170 184 192 205 206 207 213 228 244 251 262 263 266 270 271
#> [199] 292 297 298 303 304 317 322 324 325   3   5   7  33  37  42  47  62  64
#> [217]  84  86 103 106 115 140 147 167 180 186 191 194 208 220 237 243 261 296
#> [235] 302 320 326 328 329 338 344  28  48  55  56  95 109 113 114 130 133 136
#> [253] 137 142 148 163 165 214 217 219 223 234 241 248 249 250 252 254 260 274
#> [271] 309 316 323 333 342   8   9  23  26  34  35  40  45  60  73  77  90  91
#> [289] 105 110 112 119 124 125 135 160 162 166 168 182 210 211 215 268 286 311
#> [307] 313 319 327

# Stratification
task = tsk("sonar")
prop.table(table(task$truth())) # moderately unbalanced
#> 
#>         M         R 
#> 0.5336538 0.4663462 
task$col_roles$stratum = task$target_names

r = rsmp("subsampling")
r$instantiate(task)
prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion
#> 
#>         M         R 
#> 0.5323741 0.4676259 

## ------------------------------------------------
## Method `Resampling$instantiate()`
## ------------------------------------------------

task = tsk("penguins")
resampling = rsmp("holdout")
resampling$instantiate(task)

## ------------------------------------------------
## Method `Resampling$train_set()`
## ------------------------------------------------

task = tsk("penguins")
resampling = rsmp("holdout")$instantiate(task)
resampling$train_set(1)
#>   [1]   2   4   5   7   8   9  10  11  13  15  16  17  18  19  20  21  22  24
#>  [19]  26  27  31  32  33  38  39  40  42  44  46  47  49  50  51  52  53  55
#>  [37]  56  57  62  64  65  66  67  68  69  70  72  73  75  76  77  79  80  83
#>  [55]  85  86  87  88  90  91  93  94  95  97 101 102 103 104 107 109 111 112
#>  [73] 113 114 115 116 117 118 123 124 125 126 127 129 132 133 134 135 137 139
#>  [91] 142 143 144 146 147 150 151 152 153 154 155 157 158 160 161 163 164 165
#> [109] 166 169 173 174 178 179 180 181 182 183 184 185 187 190 192 193 195 199
#> [127] 200 203 204 206 208 209 211 212 213 214 215 216 217 218 219 220 221 222
#> [145] 223 225 226 227 229 230 231 233 235 238 240 241 243 245 246 247 249 250
#> [163] 251 252 253 257 258 259 260 261 262 263 264 265 266 267 268 270 271 275
#> [181] 276 277 278 279 280 281 283 284 286 287 288 290 291 292 294 295 296 297
#> [199] 299 300 301 302 303 304 306 307 308 309 310 311 313 320 323 324 326 327
#> [217] 328 329 330 331 332 336 337 338 339 340 342 343 344

## ------------------------------------------------
## Method `Resampling$test_set()`
## ------------------------------------------------

task = tsk("penguins")
resampling = rsmp("holdout")$instantiate(task)
resampling$test_set(1)
#>   [1]   4   6   7   9  14  17  22  24  28  30  31  36  38  39  44  45  46  48
#>  [19]  49  50  53  59  61  62  68  70  75  76  80  83  86  87  88  89  91  96
#>  [37]  98 101 103 104 106 111 112 114 116 121 123 125 126 127 131 132 134 136
#>  [55] 140 141 142 148 150 154 155 159 160 162 163 164 166 167 168 170 177 181
#>  [73] 186 187 188 192 193 206 211 221 225 227 228 229 231 235 238 239 240 241
#>  [91] 250 251 254 259 260 268 272 273 284 286 288 295 297 300 306 309 316 325
#> [109] 329 330 332 336 337 342 343
```
