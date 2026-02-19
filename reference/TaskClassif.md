# Classification Task

This task specializes [Task](https://mlr3.mlr-org.com/reference/Task.md)
and
[TaskSupervised](https://mlr3.mlr-org.com/reference/TaskSupervised.md)
for classification problems. The target column is assumed to be a factor
or ordered factor. The `task_type` is set to `"classif"`.

Additional task properties include:

- `"twoclass"`: The task is a binary classification problem.

- `"multiclass"`: The task is a multiclass classification problem.

It is recommended to use
[`as_task_classif()`](https://mlr3.mlr-org.com/reference/as_task_classif.md)
for construction. Predefined tasks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.md).

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html>

- Package [mlr3data](https://CRAN.R-project.org/package=mlr3data) for
  more toy tasks.

- Package [mlr3oml](https://CRAN.R-project.org/package=mlr3oml) for
  downloading tasks from <https://www.openml.org>.

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Tasks](https://mlr3.mlr-org.com/reference/Task.md):
  [mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.md)

- `as.data.table(mlr_tasks)` for a table of available
  [Tasks](https://mlr3.mlr-org.com/reference/Task.md) in the running
  session (depending on the loaded packages).

- [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect) and
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters) for
  feature selection and feature filtering.

- Extension packages for additional task types:

  - Unsupervised clustering:
    [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

  - Probabilistic supervised regression and survival analysis:
    <https://mlr3proba.mlr-org.com/>.

Other Task: [`Task`](https://mlr3.mlr-org.com/reference/Task.md),
[`TaskRegr`](https://mlr3.mlr-org.com/reference/TaskRegr.md),
[`TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.md),
[`TaskUnsupervised`](https://mlr3.mlr-org.com/reference/TaskUnsupervised.md),
[`california_housing`](https://mlr3.mlr-org.com/reference/california_housing.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/reference/mlr_tasks.md),
[`mlr_tasks_breast_cancer`](https://mlr3.mlr-org.com/reference/mlr_tasks_breast_cancer.md),
[`mlr_tasks_german_credit`](https://mlr3.mlr-org.com/reference/mlr_tasks_german_credit.md),
[`mlr_tasks_iris`](https://mlr3.mlr-org.com/reference/mlr_tasks_iris.md),
[`mlr_tasks_mtcars`](https://mlr3.mlr-org.com/reference/mlr_tasks_mtcars.md),
[`mlr_tasks_penguins`](https://mlr3.mlr-org.com/reference/mlr_tasks_penguins.md),
[`mlr_tasks_pima`](https://mlr3.mlr-org.com/reference/mlr_tasks_pima.md),
[`mlr_tasks_sonar`](https://mlr3.mlr-org.com/reference/mlr_tasks_sonar.md),
[`mlr_tasks_spam`](https://mlr3.mlr-org.com/reference/mlr_tasks_spam.md),
[`mlr_tasks_wine`](https://mlr3.mlr-org.com/reference/mlr_tasks_wine.md),
[`mlr_tasks_zoo`](https://mlr3.mlr-org.com/reference/mlr_tasks_zoo.md)

## Super classes

[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.md) -\>
[`mlr3::TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.md)
-\> `TaskClassif`

## Active bindings

- `class_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all class labels of the target column.

- `positive`:

  (`character(1)`)  
  Stores the positive class for binary classification tasks, and `NA`
  for multiclass tasks. To switch the positive class, assign a level to
  this field.

- `negative`:

  (`character(1)`)  
  Stores the negative class for binary classification tasks, and `NA`
  for multiclass tasks.

## Methods

### Public methods

- [`TaskClassif$new()`](#method-TaskClassif-new)

- [`TaskClassif$truth()`](#method-TaskClassif-truth)

- [`TaskClassif$droplevels()`](#method-TaskClassif-droplevels)

- [`TaskClassif$clone()`](#method-TaskClassif-clone)

Inherited methods

- [`mlr3::Task$add_strata()`](https://mlr3.mlr-org.com/reference/Task.html#method-add_strata)
- [`mlr3::Task$cbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-cbind)
- [`mlr3::Task$data()`](https://mlr3.mlr-org.com/reference/Task.html#method-data)
- [`mlr3::Task$divide()`](https://mlr3.mlr-org.com/reference/Task.html#method-divide)
- [`mlr3::Task$filter()`](https://mlr3.mlr-org.com/reference/Task.html#method-filter)
- [`mlr3::Task$format()`](https://mlr3.mlr-org.com/reference/Task.html#method-format)
- [`mlr3::Task$formula()`](https://mlr3.mlr-org.com/reference/Task.html#method-formula)
- [`mlr3::Task$head()`](https://mlr3.mlr-org.com/reference/Task.html#method-head)
- [`mlr3::Task$help()`](https://mlr3.mlr-org.com/reference/Task.html#method-help)
- [`mlr3::Task$levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-levels)
- [`mlr3::Task$materialize_view()`](https://mlr3.mlr-org.com/reference/Task.html#method-materialize_view)
- [`mlr3::Task$missings()`](https://mlr3.mlr-org.com/reference/Task.html#method-missings)
- [`mlr3::Task$print()`](https://mlr3.mlr-org.com/reference/Task.html#method-print)
- [`mlr3::Task$rbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-rbind)
- [`mlr3::Task$rename()`](https://mlr3.mlr-org.com/reference/Task.html#method-rename)
- [`mlr3::Task$select()`](https://mlr3.mlr-org.com/reference/Task.html#method-select)
- [`mlr3::Task$set_col_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_col_roles)
- [`mlr3::Task$set_levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_levels)
- [`mlr3::Task$set_row_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_row_roles)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. The function
[`as_task_classif()`](https://mlr3.mlr-org.com/reference/as_task_classif.md)
provides an alternative way to construct classification tasks.

#### Usage

    TaskClassif$new(
      id,
      backend,
      target,
      positive = NULL,
      label = NA_character_,
      extra_args = list()
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `backend`:

  ([DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md))  
  Either a
  [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md), or
  any object which is convertible to a
  [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md) with
  [`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.md).
  E.g., a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) will
  be converted to a
  [DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.md).

- `target`:

  (`character(1)`)  
  Name of the target column.

- `positive`:

  (`character(1)`)  
  Only for binary classification: Name of the positive class. The levels
  of the target columns are reordered accordingly, so that the first
  element of `$class_names` is the positive class, and the second
  element is the negative class.

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `extra_args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named list of constructor arguments, required for converting task
  types via
  [`convert_task()`](https://mlr3.mlr-org.com/reference/convert_task.md).

------------------------------------------------------------------------

### Method `truth()`

True response for specified `row_ids`. Format depends on the task type.
Defaults to all rows with role `"use"`.

#### Usage

    TaskClassif$truth(rows = NULL)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

#### Returns

[`factor()`](https://rdrr.io/r/base/factor.html).

------------------------------------------------------------------------

### Method [`droplevels()`](https://rdrr.io/r/base/droplevels.html)

Updates the cache of stored factor levels, removing all levels not
present in the current set of active rows. `cols` defaults to all
columns with storage type "factor" or "ordered". Also updates the task
property `"twoclass"`/`"multiclass"`.

#### Usage

    TaskClassif$droplevels(cols = NULL)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Vector of column names.

#### Returns

Modified `self`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskClassif$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data("Sonar", package = "mlbench")
task = as_task_classif(Sonar, target = "Class", positive = "M")

task$task_type
#> [1] "classif"
task$formula()
#> Class ~ .
#> NULL
task$truth()
#>   [1] R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R
#>  [38] R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R R
#>  [75] R R R R R R R R R R R R R R R R R R R R R R R M M M M M M M M M M M M M M
#> [112] M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M
#> [149] M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M M
#> [186] M M M M M M M M M M M M M M M M M M M M M M M
#> Levels: M R
task$class_names
#> [1] "M" "R"
task$positive
#> [1] "M"
task$data(rows = 1:3, cols = task$feature_names[1:2])
#>        V1    V10
#>     <num>  <num>
#> 1: 0.0200 0.2111
#> 2: 0.0453 0.2872
#> 3: 0.0262 0.6194
```
