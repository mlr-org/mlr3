# Syntactic Sugar for Object Construction

Functions to retrieve objects, set hyperparameters and assign to fields
in one go. Relies on
[`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
to extract objects from the respective
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html):

- `tsk()` for a [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)
  from [mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md).

- `tsks()` for a list of
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s from
  [mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md).

- `tgen()` for a
  [TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
  from
  [mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md).

- `tgens()` for a list of
  [TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)s
  from
  [mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md).

- `lrn()` for a
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) from
  [mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md).

- `lrns()` for a list of
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)s from
  [mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md).

- `rsmp()` for a
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
  from
  [mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md).

- `rsmps()` for a list of
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)s
  from
  [mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md).

- `msr()` for a
  [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) from
  [mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md).

- `msrs()` for a list of
  [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)s from
  [mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md).

Helper function to configure the `$validate` field(s) of a
[`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md).

This is especially useful for learners such as `AutoTuner` of
[mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning) or
`GraphLearner` of
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines) which
have multiple levels of `$validate` fields., where the `$validate`
fields need to be configured on multiple levels.

## Usage

``` r
tsk(.key, ...)

tsks(.keys, ...)

tgen(.key, ...)

tgens(.keys, ...)

lrn(.key, ...)

lrns(.keys, ...)

rsmp(.key, ...)

rsmps(.keys, ...)

msr(.key, ...)

msrs(.keys, ...)

set_validate(learner, validate, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve the object.

- ...:

  (any)  
  Additional arguments.

- .keys:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Keys passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve multiple objects.

- learner:

  (any)  
  The learner.

- validate:

  (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)  
  Which validation set to use.

## Value

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object of the
respective type, or a list of
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) objects for
the plural versions.

Modified [`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md)

## Examples

``` r
# penguins task with new id
tsk("penguins", id = "penguins2")
#> 
#> ── <TaskClassif> (344x8): Palmer Penguins ──────────────────────────────────────
#> • Target: species
#> • Target classes: Adelie (44%), Gentoo (36%), Chinstrap (20%)
#> • Properties: multiclass
#> • Features (7):
#>   • int (3): body_mass, flipper_length, year
#>   • dbl (2): bill_depth, bill_length
#>   • fct (2): island, sex

# classification tree with different hyperparameters
# and predict type set to predict probabilities
lrn("classif.rpart", cp = 0.1, predict_type = "prob")
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: -
#> • Parameters: cp=0.1, xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: response and [prob]
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use'

# multiple learners with predict type 'prob'
lrns(c("classif.featureless", "classif.rpart"), predict_type = "prob")
#> $classif.featureless
#> 
#> ── <LearnerClassifFeatureless> (classif.featureless): Featureless Classification
#> • Model: -
#> • Parameters: method=mode
#> • Packages: mlr3
#> • Predict Types: response and [prob]
#> • Feature Types: logical, integer, numeric, character, factor, ordered,
#> POSIXct, and Date
#> • Encapsulation: none (fallback: -)
#> • Properties: featureless, importance, missings, multiclass, selected_features,
#> twoclass, and weights
#> • Other settings: use_weights = 'use'
#> 
#> $classif.rpart
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: -
#> • Parameters: xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: response and [prob]
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use'
#> 
learner = lrn("classif.debug")
set_validate(learner, 0.2)
learner$validate
#> [1] 0.2
```
