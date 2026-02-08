# Regression Task

This task specializes
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) and
[TaskSupervised](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md)
for regression problems. The target column is assumed to be numeric. The
`task_type` is set to `"regr"`.

It is recommended to use
[`as_task_regr()`](https://mlr3.mlr-org.com/dev/reference/as_task_regr.md)
for construction. Predefined tasks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md).

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
  of [Tasks](https://mlr3.mlr-org.com/dev/reference/Task.md):
  [mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md)

- `as.data.table(mlr_tasks)` for a table of available
  [Tasks](https://mlr3.mlr-org.com/dev/reference/Task.md) in the running
  session (depending on the loaded packages).

- [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect) and
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters) for
  feature selection and feature filtering.

- Extension packages for additional task types:

  - Unsupervised clustering:
    [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

  - Probabilistic supervised regression and survival analysis:
    <https://mlr3proba.mlr-org.com/>.

Other Task: [`Task`](https://mlr3.mlr-org.com/dev/reference/Task.md),
[`TaskClassif`](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md),
[`TaskSupervised`](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md),
[`TaskUnsupervised`](https://mlr3.mlr-org.com/dev/reference/TaskUnsupervised.md),
[`california_housing`](https://mlr3.mlr-org.com/dev/reference/california_housing.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md),
[`mlr_tasks_breast_cancer`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_breast_cancer.md),
[`mlr_tasks_german_credit`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_german_credit.md),
[`mlr_tasks_iris`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_iris.md),
[`mlr_tasks_mtcars`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_mtcars.md),
[`mlr_tasks_penguins`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_penguins.md),
[`mlr_tasks_pima`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_pima.md),
[`mlr_tasks_sonar`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_sonar.md),
[`mlr_tasks_spam`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_spam.md),
[`mlr_tasks_wine`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_wine.md),
[`mlr_tasks_zoo`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_zoo.md)

## Super classes

[`mlr3::Task`](https://mlr3.mlr-org.com/dev/reference/Task.md) -\>
[`mlr3::TaskSupervised`](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md)
-\> `TaskRegr`

## Methods

### Public methods

- [`TaskRegr$new()`](#method-TaskRegr-new)

- [`TaskRegr$truth()`](#method-TaskRegr-truth)

- [`TaskRegr$clone()`](#method-TaskRegr-clone)

Inherited methods

- [`mlr3::Task$add_strata()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-add_strata)
- [`mlr3::Task$cbind()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-cbind)
- [`mlr3::Task$data()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-data)
- [`mlr3::Task$divide()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-divide)
- [`mlr3::Task$droplevels()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-droplevels)
- [`mlr3::Task$filter()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-filter)
- [`mlr3::Task$format()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-format)
- [`mlr3::Task$formula()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-formula)
- [`mlr3::Task$head()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-head)
- [`mlr3::Task$help()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-help)
- [`mlr3::Task$levels()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-levels)
- [`mlr3::Task$materialize_view()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-materialize_view)
- [`mlr3::Task$missings()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-missings)
- [`mlr3::Task$print()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-print)
- [`mlr3::Task$rbind()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-rbind)
- [`mlr3::Task$rename()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-rename)
- [`mlr3::Task$select()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-select)
- [`mlr3::Task$set_col_roles()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-set_col_roles)
- [`mlr3::Task$set_levels()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-set_levels)
- [`mlr3::Task$set_row_roles()`](https://mlr3.mlr-org.com/dev/reference/Task.html#method-set_row_roles)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. The function
[`as_task_regr()`](https://mlr3.mlr-org.com/dev/reference/as_task_regr.md)
provides an alternative way to construct regression tasks.

#### Usage

    TaskRegr$new(id, backend, target, label = NA_character_, extra_args = list())

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `backend`:

  ([DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md))  
  Either a
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md),
  or any object which is convertible to a
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  with
  [`as_data_backend()`](https://mlr3.mlr-org.com/dev/reference/as_data_backend.md).
  E.g., a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) will
  be converted to a
  [DataBackendDataTable](https://mlr3.mlr-org.com/dev/reference/DataBackendDataTable.md).

- `target`:

  (`character(1)`)  
  Name of the target column.

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `extra_args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named list of constructor arguments, required for converting task
  types via
  [`convert_task()`](https://mlr3.mlr-org.com/dev/reference/convert_task.md).

------------------------------------------------------------------------

### Method `truth()`

True response for specified `row_ids`. Format depends on the task type.
Defaults to all rows with role "use".

#### Usage

    TaskRegr$truth(rows = NULL)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskRegr$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = as_task_regr(mtcars, target = "mpg")
task$task_type
#> [1] "regr"
task$formula()
#> mpg ~ .
#> NULL
task$truth()
#>  [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3 15.2 10.4
#> [16] 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3 26.0 30.4 15.8 19.7
#> [31] 15.0 21.4
task$data(rows = 1:3, cols = task$feature_names[1:2])
#>       am  carb
#>    <num> <num>
#> 1:     1     4
#> 2:     1     4
#> 3:     1     1
```
