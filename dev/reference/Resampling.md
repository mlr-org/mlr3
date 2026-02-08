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
repdocubility of results, you need to call `set.seed` before doing so.
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

- [`Resampling$new()`](#method-Resampling-new)

- [`Resampling$format()`](#method-Resampling-format)

- [`Resampling$print()`](#method-Resampling-print)

- [`Resampling$help()`](#method-Resampling-help)

- [`Resampling$instantiate()`](#method-Resampling-instantiate)

- [`Resampling$train_set()`](#method-Resampling-train_set)

- [`Resampling$test_set()`](#method-Resampling-test_set)

- [`Resampling$clone()`](#method-Resampling-clone)

------------------------------------------------------------------------

### Method `new()`

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

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Resampling$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Resampling$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Resampling$help()

------------------------------------------------------------------------

### Method `instantiate()`

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
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    resampling = rsmp("holdout")
    resampling$instantiate(task)

------------------------------------------------------------------------

### Method `train_set()`

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

### Method `test_set()`

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

### Method `clone()`

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
#>  [1] 172   7 234 231  93  74 328  46  32  85  95 186 147 341  73 136  98 218  41
#> [20]  42 295 279 155 260 290  47 192 199 145 127 223 228 166  60
intersect(train_set, r$test_set(1))
#> integer(0)

# Another example: 10-fold CV
r = rsmp("cv")$instantiate(task)
r$train_set(1)
#>   [1]  16  25  34  37  39  61  64  65  68  96  97 104 108 120 132 136 156 162
#>  [19] 173 175 182 184 221 223 227 230 245 268 272 309 313 317 324 331 333   6
#>  [37]  12  22  50  53  60  76  78 110 118 130 138 158 178 187 190 196 198 199
#>  [55] 202 203 228 231 232 242 244 261 279 289 302 308 310 322 330 334   1   4
#>  [73]   5   8  11  30  41  47  55  63  69  82  84  93  94  98 124 127 137 139
#>  [91] 145 189 234 238 275 291 297 301 304 315 323 327 337 340 342   2  14  28
#> [109]  49  51  56  70  79  92  99 107 125 128 165 179 183 185 201 205 217 229
#> [127] 235 240 250 253 255 256 259 263 285 326 328 332 339  10  15  33  38  42
#> [145]  43  52  59  73  77  80  81  87  88  95 103 115 116 133 142 168 171 177
#> [163] 207 209 210 248 251 282 287 300 305 307 318  20  23  36  83  91 105 122
#> [181] 135 151 155 157 176 206 222 233 237 247 252 254 260 262 264 265 278 280
#> [199] 286 294 295 296 298 299 303 335 344  17  21  26  31  46  48  58  66  71
#> [217]  72 112 117 146 161 164 169 172 180 204 208 216 236 241 249 266 270 274
#> [235] 276 283 292 311 320 321 336   9  13  32  40  67  86  89  90 102 106 109
#> [253] 114 119 121 126 144 166 170 174 191 193 200 213 220 225 243 269 290 293
#> [271] 314 316 319 338 343   3   7  18  19  24  29  44  57  62  75  85 100 101
#> [289] 111 129 134 141 143 147 149 163 195 211 215 218 226 239 257 258 267 271
#> [307] 277 284 325

# Stratification
task = tsk("pima")
prop.table(table(task$truth())) # moderately unbalanced
#> 
#>       pos       neg 
#> 0.3489583 0.6510417 
task$col_roles$stratum = task$target_names

r = rsmp("subsampling")
r$instantiate(task)
prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion
#> 
#>       pos       neg 
#> 0.3496094 0.6503906 

## ------------------------------------------------
## Method `Resampling$instantiate`
## ------------------------------------------------

task = tsk("penguins")
resampling = rsmp("holdout")
resampling$instantiate(task)

## ------------------------------------------------
## Method `Resampling$train_set`
## ------------------------------------------------

task = tsk("penguins")
resampling = rsmp("holdout")$instantiate(task)
resampling$train_set(1)
#>   [1]   2   5   6   7   8   9  10  11  12  13  14  16  17  18  19  20  21  23
#>  [19]  24  25  26  27  30  33  34  35  37  38  39  40  41  42  43  45  46  47
#>  [37]  49  50  52  53  54  55  56  57  58  59  63  64  65  66  67  68  69  70
#>  [55]  71  75  76  78  79  80  82  83  84  85  86  87  88  90  91  92  93  94
#>  [73]  95  96  97  98  99 100 101 102 103 109 111 112 115 116 118 120 123 124
#>  [91] 126 128 129 130 131 132 136 138 140 141 143 144 145 146 147 148 150 152
#> [109] 153 156 157 158 159 160 161 163 164 165 166 167 170 175 176 178 179 180
#> [127] 182 183 184 185 187 188 189 191 192 193 195 196 197 198 200 201 202 203
#> [145] 206 207 208 209 210 211 212 214 216 218 219 221 223 224 227 228 229 230
#> [163] 232 233 237 240 241 244 245 246 247 248 252 255 256 257 260 261 262 264
#> [181] 265 267 268 269 270 271 274 277 278 279 281 282 284 285 286 290 292 293
#> [199] 294 295 296 299 301 302 305 308 309 311 313 314 316 318 319 320 321 325
#> [217] 327 329 330 332 333 335 336 337 338 339 340 341 344

## ------------------------------------------------
## Method `Resampling$test_set`
## ------------------------------------------------

task = tsk("penguins")
resampling = rsmp("holdout")$instantiate(task)
resampling$test_set(1)
#>   [1]   2   3   7   8  10  12  13  16  18  22  25  26  31  40  46  47  49  50
#>  [19]  57  58  60  66  67  70  71  72  74  76  79  83  85  86  88  90  93  94
#>  [37]  95  99 104 106 109 110 111 112 116 121 122 133 146 149 151 155 157 158
#>  [55] 159 160 162 167 170 171 172 174 177 178 179 181 183 186 187 189 202 207
#>  [73] 210 213 214 217 218 221 224 225 226 231 237 246 251 252 255 258 259 263
#>  [91] 265 269 274 285 291 292 293 297 299 304 305 308 311 316 318 319 321 323
#> [109] 329 333 334 337 339 340 344
```
