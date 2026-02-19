# Supervised Task

This is the abstract base class for task objects like
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.md) and
[TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.md). It extends
[Task](https://mlr3.mlr-org.com/reference/Task.md) with methods to
handle a target column. Supervised tasks for probabilistic regression
(including survival analysis) can be found in
[mlr3proba](https://CRAN.R-project.org/package=mlr3proba).

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
[`TaskClassif`](https://mlr3.mlr-org.com/reference/TaskClassif.md),
[`TaskRegr`](https://mlr3.mlr-org.com/reference/TaskRegr.md),
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

## Super class

[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.md) -\>
`TaskSupervised`

## Methods

### Public methods

- [`TaskSupervised$new()`](#method-TaskSupervised-new)

- [`TaskSupervised$truth()`](#method-TaskSupervised-truth)

- [`TaskSupervised$clone()`](#method-TaskSupervised-clone)

Inherited methods

- [`mlr3::Task$add_strata()`](https://mlr3.mlr-org.com/reference/Task.html#method-add_strata)
- [`mlr3::Task$cbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-cbind)
- [`mlr3::Task$data()`](https://mlr3.mlr-org.com/reference/Task.html#method-data)
- [`mlr3::Task$divide()`](https://mlr3.mlr-org.com/reference/Task.html#method-divide)
- [`mlr3::Task$droplevels()`](https://mlr3.mlr-org.com/reference/Task.html#method-droplevels)
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
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskSupervised$new(
      id,
      task_type,
      backend,
      target,
      label = NA_character_,
      extra_args = list()
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `task_type`:

  (`character(1)`)  
  Type of task, e.g. `"regr"` or `"classif"`. Must be an element of
  [mlr_reflections\$task_types\$type](https://mlr3.mlr-org.com/reference/mlr_reflections.md).

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
Defaults to all rows with role "use".

#### Usage

    TaskSupervised$truth(rows = NULL)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskSupervised$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
TaskSupervised$new("penguins", task_type = "classif", backend = palmerpenguins::penguins,
  target = "species")
#> 
#> ── <TaskSupervised> (344x8) ────────────────────────────────────────────────────
#> • Target: species
#> • Properties: -
#> • Features (7):
#>   • int (3): body_mass_g, flipper_length_mm, year
#>   • dbl (2): bill_depth_mm, bill_length_mm
#>   • fct (2): island, sex
```
