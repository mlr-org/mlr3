# Reflections for mlr3

Environment which stores various information to allow objects to examine
and introspect their structure and properties (c.f.
[Reflections](https://www.wikiwand.com/en/Reflection_(computer_programming))).

This environment be modified by third-party packages, e.g. by adding
information about new task types or by extending the set of allowed
feature types.

Third-party packages that modify the reflections must register
themselves in the `loaded_packages` field.

The following objects are set by
[mlr3](https://CRAN.R-project.org/package=mlr3):

- `task_types`
  ([`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Table with task type (`"type"`), the implementing package (`"pkg"`),
  and the names of the generators of the corresponding
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) (`"task"`),
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)
  (`"learner"`),
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
  (`"prediction"`),
  [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)
  (`"measure"`) and fallback
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md). The
  column `"type"` must be unique for each row.

- `task_feature_types` (named
  [`character()`](https://rdrr.io/r/base/character.html))  
  Vector of base R types supported as
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) features, named
  with a 3 letter abbreviation.

- `task_row_roles`
  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of supported row roles for a
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

- `task_col_roles` (list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of vectors of supported column roles for a
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md), named by their
  task type.

- `task_properties` (list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of vectors of supported
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) properties,
  named by their task type.

- `task_mandatory_properties` (list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of vectors of
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) properties
  which necessarily must be supported by the
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md). I.e., if
  the task property is not found in the set of the learner properties,
  an exception is raised.

- `task_print_col_roles` (list of named
  [`character()`](https://rdrr.io/r/base/character.html))  
  Vector of column roles to print via `print(task)` if the role is not
  empty, either before or after the task's target, properties and
  features. The names of the column roles are the values, the names
  correspond to the labels to use in the printer.

- `learner_properties` (list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of vectors of supported
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)
  properties, named by their task type.

- `learner_predict_types` (list of list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of lists of supported
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)
  predict_types, named by their task type. The inner list translates the
  `"predict_type"` to all predict types returned, e.g. predict type
  `"prob"` for a
  [LearnerClassif](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
  provides the probabilities as well as the predicted labels, therefore
  `"prob"` maps to `c("response", "prob")`.

- `learner_predict_types` (list of list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of lists of supported
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)
  predict_types, named by their task type.

- `learner_param_tags`
  ([`character()`](https://rdrr.io/r/base/character.html))  
  Character vector of allowed 'tags' for the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)s
  of a [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md).

- `predict_sets`
  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of possible predict sets. Currently supported are `"train"`,
  `"test"` and `"holdout"`.

- `measure_properties` (list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of vectors of supported
  [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)
  properties, named by their task type.

- `default_measures` (list of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of keys for the default
  [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)s, named
  by their task type.

- `rr_names` ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of the objects stored in a
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

- `auto_converters`
  ([`environment()`](https://rdrr.io/r/base/environment.html))  
  Environment of converter functions used for `rbind`-ing data to tasks.
  Functions are named using the pattern `"[from_type]___[to_type]"`. Can
  be extended by third-party with additional converters.

## Usage

``` r
mlr_reflections
```

## Format

[environment](https://rdrr.io/r/base/environment.html).

## Examples

``` r
ls.str(mlr_reflections)
#> auto_converters : <environment: 0x557bdf6790f8> 
#> default_measures : List of 3
#>  $ classif     : chr "classif.ce"
#>  $ regr        : chr "regr.mse"
#>  $ unsupervised: chr NA
#> learner_param_tags :  chr [1:7] "train" "predict" "hotstart" "importance" "threads" "required" ...
#> learner_predict_types : List of 3
#>  $ classif     :List of 2
#>  $ regr        :List of 4
#>  $ unsupervised: NULL
#> learner_properties : List of 2
#>  $ classif: chr [1:14] "featureless" "missings" "weights" "importance" ...
#>  $ regr   : chr [1:12] "featureless" "missings" "weights" "importance" ...
#> loaded_packages :  chr "mlr3"
#> measure_properties : List of 2
#>  $ classif: chr [1:9] "na_score" "requires_task" "requires_learner" "requires_model" ...
#>  $ regr   : chr [1:9] "na_score" "requires_task" "requires_learner" "requires_model" ...
#> package_version : Classes 'package_version', 'numeric_version'  hidden list of 1
#>  $ : int [1:4] 1 2 0 9000
#> predict_sets :  chr [1:3] "train" "test" "internal_valid"
#> resampling_properties :  chr [1:2] "duplicated_ids" "weights"
#> rr_names :  chr [1:4] "task" "learner" "resampling" "iteration"
#> task_col_roles : List of 3
#>  $ regr        : chr [1:9] "feature" "target" "name" "order" ...
#>  $ classif     : chr [1:9] "feature" "target" "name" "order" ...
#>  $ unsupervised: chr [1:3] "feature" "name" "order"
#> task_col_roles_optional_newdata : List of 2
#>  $ classif: chr [1:6] "weights_learner" "weights_measure" "name" "order" ...
#>  $ regr   : chr [1:6] "weights_learner" "weights_measure" "name" "order" ...
#> task_feature_types :  Named chr [1:8] "logical" "integer" "numeric" "character" "factor" ...
#> task_mandatory_properties : List of 1
#>  $ classif: chr [1:2] "twoclass" "multiclass"
#> task_print_col_roles : List of 2
#>  $ before: chr(0) 
#>  $ after : Named chr [1:6] "order" "stratum" "group" "offset" ...
#> task_properties : List of 3
#>  $ classif     : chr [1:7] "strata" "groups" "offset" "weights_learner" ...
#>  $ regr        : chr [1:5] "strata" "groups" "offset" "weights_learner" ...
#>  $ unsupervised: chr(0) 
#> task_row_roles :  chr "use"
#> task_types : Classes ‘data.table’ and 'data.frame':  3 obs. of  7 variables:
#>  $ type           : chr  "classif" "regr" "unsupervised"
#>  $ package        : chr  "mlr3" "mlr3" "mlr3"
#>  $ task           : chr  "TaskClassif" "TaskRegr" "TaskUnsupervised"
#>  $ learner        : chr  "LearnerClassif" "LearnerRegr" "Learner"
#>  $ prediction     : chr  "PredictionClassif" "PredictionRegr" NA
#>  $ prediction_data: chr  "PredictionDataClassif" "PredictionDataRegr" NA
#>  $ measure        : chr  "MeasureClassif" "MeasureRegr" NA
```
