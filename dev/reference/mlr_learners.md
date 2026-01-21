# Dictionary of Learners

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md). Each
learner has an associated help page, see `mlr_learners_[id]`.

This dictionary can get populated with additional learners by add-on
packages. For an opinionated set of solid classification and regression
learners, install and load the
[mlr3learners](https://CRAN.R-project.org/package=mlr3learners) package.
More learners are connected via
<https://github.com/mlr-org/mlr3extralearners>.

For a more convenient way to retrieve and construct learners, see
[`lrn()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md)/[`lrns()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with fields "key", "label", "task_type", "feature_types", "packages",
  "properties", and "predict_types" as columns. If `objects` is set to
  `TRUE`, the constructed objects are returned in the list column named
  `object`.

## See also

Sugar functions:
[`lrn()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md),
[`lrns()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md)

Extension Packages:
[mlr3learners](https://CRAN.R-project.org/package=mlr3learners)

Other Dictionary:
[`mlr_measures`](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md),
[`mlr_resamplings`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md),
[`mlr_task_generators`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md)

Other Learner:
[`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md),
[`LearnerClassif`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md),
[`LearnerRegr`](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md),
[`mlr_learners_classif.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.debug.md),
[`mlr_learners_classif.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.featureless.md),
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.rpart.md)

## Examples

``` r
as.data.table(mlr_learners)
#> Key: <key>
#>                    key                              label task_type
#>                 <char>                             <char>    <char>
#> 1:       classif.debug   Debug Learner for Classification   classif
#> 2: classif.featureless Featureless Classification Learner   classif
#> 3:       classif.rpart                Classification Tree   classif
#> 4:          regr.debug       Debug Learner for Regression      regr
#> 5:    regr.featureless     Featureless Regression Learner      regr
#> 6:          regr.rpart                    Regression Tree      regr
#>                                              feature_types   packages
#>                                                     <list>     <list>
#> 1:        logical,integer,numeric,character,factor,ordered       mlr3
#> 2: logical,integer,numeric,character,factor,ordered,...[8]       mlr3
#> 3:                  logical,integer,numeric,factor,ordered mlr3,rpart
#> 4:        logical,integer,numeric,character,factor,ordered mlr3,stats
#> 5: logical,integer,numeric,character,factor,ordered,...[8] mlr3,stats
#> 6:                  logical,integer,numeric,factor,ordered mlr3,rpart
#>                                                                      properties
#>                                                                          <list>
#> 1: hotstart_forward,internal_tuning,marshal,missings,multiclass,twoclass,...[8]
#> 2: featureless,importance,missings,multiclass,selected_features,twoclass,...[7]
#> 3:            importance,missings,multiclass,selected_features,twoclass,weights
#> 4:                                                             missings,weights
#> 5:                    featureless,importance,missings,selected_features,weights
#> 6:                                importance,missings,selected_features,weights
#>            predict_types
#>                   <list>
#> 1:         response,prob
#> 2:         response,prob
#> 3:         response,prob
#> 4: response,se,quantiles
#> 5: response,se,quantiles
#> 6:              response
mlr_learners$get("classif.featureless")
#> 
#> ── <LearnerClassifFeatureless> (classif.featureless): Featureless Classification
#> • Model: -
#> • Parameters: method=mode
#> • Packages: mlr3
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, character, factor, ordered,
#> POSIXct, and Date
#> • Encapsulation: none (fallback: -)
#> • Properties: featureless, importance, missings, multiclass, selected_features,
#> twoclass, and weights
#> • Other settings: use_weights = 'use'
lrn("classif.rpart")
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: -
#> • Parameters: xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use'
```
