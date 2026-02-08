# Task Class

This is the abstract base class for
[TaskSupervised](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md)
and
[TaskUnsupervised](https://mlr3.mlr-org.com/dev/reference/TaskUnsupervised.md).
[TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md) and
[TaskRegr](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md) inherit
from
[TaskSupervised](https://mlr3.mlr-org.com/dev/reference/TaskSupervised.md).
More supervised tasks are implemented in
[mlr3proba](https://CRAN.R-project.org/package=mlr3proba), unsupervised
cluster tasks in package
[mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster).

Tasks serve two purposes:

1.  Tasks wrap a
    [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md),
    an object to transparently interface different data storage types.

2.  Tasks store meta-information, such as the role of the individual
    columns in the
    [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md).
    For example, for a classification task a single column must be
    marked as target column, and others as features.

Predefined (toy) tasks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md), e.g.
[`penguins`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks_penguins.md)
or
[`california_housing`](https://mlr3.mlr-org.com/dev/reference/california_housing.md).
More toy tasks can be found in the dictionary after loading
[mlr3data](https://CRAN.R-project.org/package=mlr3data).

## S3 methods

- `as.data.table(t)`  
  Task -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns the complete data as
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

- `head(t)`  
  Calls [`head()`](https://rdrr.io/r/utils/head.html) on the task's
  data.

- `summary(t)`  
  Calls [`summary()`](https://rdrr.io/r/base/summary.html) on the task's
  data.

## Task mutators

The following methods change the task in-place:

- Any modification of the lists `$col_roles` or `$row_roles`. This
  provides a different "view" on the data without altering the data
  itself. This may affects, e.g., `$data`, `$nrow`, `$ncol`,
  `n_features`, `row_ids`, and `$feature_names`. Altering `$col_roles`
  may affect, e.g., `$data`, `$ncol`, `$n_features`, and
  `$feature_names`. Altering `$row_roles` may affect, e.g., `$data`,
  `$nrow`, and `$row_ids`.

- Modification of column or row roles via `$set_col_roles()` or
  `$set_row_roles()`, respectively. They are an alternative to directly
  accessing `$col_roles` or `$row_roles`, with the same side effects.

- `$select()` and `$filter()` subset the set of active features or rows
  in `$col_roles` or `$row_roles`, respectively.

- `$cbind()` and `$rbind()` change the task in-place by binding new
  columns or rows to the data.

- `$rename()` changes column names.

- `$set_levels()` and `$droplevels()` update the field `$col_info()` to
  automatically repair factor levels while querying data with `$data()`.

- `$materialize_view()` creates a new
  [DataBackendDataTable](https://mlr3.mlr-org.com/dev/reference/DataBackendDataTable.md)
  which keeps only the data in the currently active view possibly
  freeing some memory consumed by the
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  stored in the `Task`.

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
  of Tasks:
  [mlr_tasks](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md)

- `as.data.table(mlr_tasks)` for a table of available Tasks in the
  running session (depending on the loaded packages).

- [mlr3fselect](https://CRAN.R-project.org/package=mlr3fselect) and
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters) for
  feature selection and feature filtering.

- Extension packages for additional task types:

  - Unsupervised clustering:
    [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster)

  - Probabilistic supervised regression and survival analysis:
    <https://mlr3proba.mlr-org.com/>.

Other Task:
[`TaskClassif`](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md),
[`TaskRegr`](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md),
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

## Public fields

- `backend`:

  ([DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md))  
  Abstract interface to the data of the task.

- `col_info`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Table with with 4 columns, mainly for internal purposes:

  - `"id"` ([`character()`](https://rdrr.io/r/base/character.html))
    stores the name of the column.

  - `"type"` ([`character()`](https://rdrr.io/r/base/character.html))
    holds the storage type of the variable, e.g. `integer`, `numeric` or
    `character`. See
    [mlr_reflections\$task_feature_types](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md)
    for a complete list of allowed types.

  - `"levels"` ([`list()`](https://rdrr.io/r/base/list.html)) stores a
    vector of distinct values (levels) for ordered and unordered factor
    variables.

  - `"label"` ([`character()`](https://rdrr.io/r/base/character.html))
    stores a vector of prettier, formated column names.

  - `"fix_factor_levels"`
    ([`logical()`](https://rdrr.io/r/base/logical.html)) stores flags
    which determine if the levels of the respective variable need to be
    reordered after querying the data from the
    [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md).

  Note that all columns of the
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md),
  also columns which are not selected or have any role, are listed in
  this table.

- `mlr3_version`:

  (`package_version`)  
  Package version of `mlr3` used to create the task.

## Active bindings

- `id`:

  (`character(1)`)  
  Identifier of the object. Used in tables, plot and text output.

- `internal_valid_task`:

  (`Task` or [`integer()`](https://rdrr.io/r/base/integer.html) or
  `NULL`)  
  Optional validation task that can, e.g., be used for early stopping
  with learners such as XGBoost. See also the `$validate` field of
  [`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md). If
  integers are assigned they are removed from the primary task and an
  internal validation task with those ids is created from the primary
  task using only those ids. When assigning a new task, it is always
  cloned.

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object. The hash is calculated based
  on the complete task object and `$row_ids`. If an internal validation
  task is set, the hash is recalculated.

- `row_hash`:

  (`character(1)`)  
  Hash (unique identifier) calculated based on the row ids.

- `row_ids`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html))  
  Returns the row ids of the
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  for observations with role "use".

- `row_names`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns a table with two columns:

  - `"row_id"` ([`integer()`](https://rdrr.io/r/base/integer.html)), and

  - `"row_name"`
    ([`character()`](https://rdrr.io/r/base/character.html)).

- `feature_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all column names with `role == "feature"`.

  Note that this vector determines the default order of columns for
  `task$data(cols = NULL, ...)`. However, it is recommended to **not**
  rely on the order of columns, but instead always address columns by
  their name. The default order is not well defined after some
  operations, e.g. after `task$cbind()` or after processing via
  [mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines).

- `target_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all column names with role "target".

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of task properties. Possible properties are are stored in
  [mlr_reflections\$task_properties](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).
  The following properties are currently standardized and understood by
  tasks in [mlr3](https://CRAN.R-project.org/package=mlr3):

  - `"strata"`: The task is resampled using one or more stratification
    variables (role `"stratum"`).

  - `"groups"`: The task comes with grouping/blocking information (role
    `"group"`).

  - `"weights_learner"`: If the task has observation weights with this
    role, they are passed to the
    [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
    train. The use of weights can be disabled via by setting the
    learner's hyperparameter `use_weights` to `FALSE`.

  - `"weights_measure"`: If the task has observation weights with this
    role, they are passed to the
    [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) for
    weighted scoring. The use of weights can be disabled via by setting
    the measure's hyperparameter `use_weights` to `FALSE`.

  - `"offset"`: The task includes one or more offset columns specifying
    fixed adjustments for model training and possibly for prediction
    (role `"offset"`).

  - `"ordered"`: The task has columns which define the row order (role
    `"order"`).

  Note that above listed properties are calculated from the
  `$col_roles`, and may not be set explicitly.

- `row_roles`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Each row (observation) can have an arbitrary number of roles in the
  learning task:

  - `"use"`: Use in train / predict / resampling.

  `row_roles` is a named list whose elements are named by row role and
  each element is an [`integer()`](https://rdrr.io/r/base/integer.html)
  vector of row ids. To alter the roles, just modify the list, e.g. with
  R's set functions ([`intersect()`](https://rdrr.io/r/base/sets.html),
  [`setdiff()`](https://rdrr.io/r/base/sets.html),
  [`union()`](https://rdrr.io/r/base/sets.html), ...).

- `col_roles`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Each column can be in one or more of the following groups to fulfill
  different roles:

  - `"feature"`: Regular feature used in the model fitting process.

  - `"target"`: Target variable. Most tasks only accept a single target
    column.

  - `"name"`: Row names / observation labels. To be used in plots. Can
    be queried with `$row_names`. Not more than a single column can be
    associated with this role.

  - `"order"`: Data returned by `$data()` is ordered by this column (or
    these columns). Columns must be sortable with
    [`order()`](https://rdrr.io/r/base/order.html).

  - `"group"`: During resampling, observations with the same value of
    the variable with role "group" are marked as "belonging together".
    For each resampling iteration, observations of the same group will
    be exclusively assigned to be either in the training set or in the
    test set. Not more than a single column can be associated with this
    role.

  - `"stratum"`: Stratification variables. Multiple discrete columns may
    have this role.

  - `"weights_learner"`: If the task has observation weights with this
    role, they are passed to the
    [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
    train. The use of weights can be disabled via by setting the
    learner's hyperparameter `use_weights` to `FALSE`.

  - `"weights_measure"`: If the task has observation weights with this
    role, they are passed to the
    [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) for
    weighted scoring. The use of weights can be disabled via by setting
    the measure's hyperparameter `use_weights` to `FALSE`.

  - `"offset"`: Numeric columns used to specify fixed adjustments for
    model training. Some models use offsets to simply shift predictions,
    while others incorporate them to boost predictions from a baseline
    model. For learners supporting offsets in multiclass settings, an
    offset column must be provided for each target class. These columns
    must follow the naming convention `"offset_{target_class_name}"`.
    For an example of a learner that supports offsets, see
    `LearnerClassifXgboost` of
    [mlr3learners](https://CRAN.R-project.org/package=mlr3learners).

  `col_roles` is a named list whose elements are named by column role
  and each element is a
  [`character()`](https://rdrr.io/r/base/character.html) vector of
  column names. To alter the roles, just modify the list, e.g. with R's
  set functions ([`intersect()`](https://rdrr.io/r/base/sets.html),
  [`setdiff()`](https://rdrr.io/r/base/sets.html),
  [`union()`](https://rdrr.io/r/base/sets.html), ...). The method
  `$set_col_roles` provides a convenient alternative to assign columns
  to roles.

  The roles `weights_learner` and `weights_measure` may only point to a
  single numeric column, but they can all point to the same column or
  different columns. Weights must be non-negative numerics with at least
  one weight being \> 0. They don't necessarily need to sum up to 1.

- `nrow`:

  (`integer(1)`)  
  Returns the total number of rows with role "use".

- `ncol`:

  (`integer(1)`)  
  Returns the total number of columns with role "target" or "feature".

- `n_features`:

  (`integer(1)`)  
  Returns the total number of columns with role "feature" (i.e. the
  number of "active" features in the task).

- `feature_types`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns a table with columns `id` and `type` where `id` are the column
  names of "active" features of the task and `type` is the storage type.

- `strata`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  If the task has columns designated with role `"stratum"`, returns a
  table with one subpopulation per row and two columns:

  - `N` ([`integer()`](https://rdrr.io/r/base/integer.html)) with the
    number of observations in the subpopulation, and

  - `row_id` (list of
    [`integer()`](https://rdrr.io/r/base/integer.html)) as list column
    with the row ids in the respective subpopulation. Returns `NULL` if
    there are is no stratification variable. See
    [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
    for more information on stratification.

- `groups`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  If the task has a column with designated role `"group"`, a table with
  two columns:

  - `row_id` ([`integer()`](https://rdrr.io/r/base/integer.html)), and

  - grouping variable `group`
    ([`vector()`](https://rdrr.io/r/base/vector.html)).

  Returns `NULL` if there are is no grouping column. See
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md) for
  more information on grouping.

- `order`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  If the task has at least one column with designated role `"order"`, a
  table with two columns:

  - `row_id` ([`integer()`](https://rdrr.io/r/base/integer.html)), and

  - ordering vector `order`
    ([`integer()`](https://rdrr.io/r/base/integer.html)).

  Returns `NULL` if there are is no order column.

- `weights`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Deprecated, use `$weights_learner` instead.

- `weights_learner`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns the observation weights used for training a
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) (column
  role `weights_learner`) as a `data.table` with the following columns:

  - `row_id` ([`integer()`](https://rdrr.io/r/base/integer.html)), and

  - `weight` ([`numeric()`](https://rdrr.io/r/base/numeric.html)).

  Returns `NULL` if there are is no column with the designated role.

- `weights_measure`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns the observation weights used for scoring a prediction with a
  [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) (column
  role `weights_measure`) as a `data.table` with the following columns:

  - `row_id` ([`integer()`](https://rdrr.io/r/base/integer.html)), and

  - `weight` ([`numeric()`](https://rdrr.io/r/base/numeric.html)).

  Returns `NULL` if there are is no column with the designated role.

- `offset`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  If the task has a column with designated role `"offset"`, a table with
  two or more columns:

  - `row_id` ([`integer()`](https://rdrr.io/r/base/integer.html)), and

  - offset variable(s)
    ([`numeric()`](https://rdrr.io/r/base/numeric.html)).

  For regression or binary classification tasks, there will be only a
  single-column offset. For multiclass tasks, it may return multiple
  offset columns, one for each target class. If there is only one offset
  column, it will be named as `offset`.

  If there are no columns with the `"offset"` role, `NULL` is returned.

- `labels`:

  (named [`character()`](https://rdrr.io/r/base/character.html))  
  Retrieve `labels` (prettier formated names) from columns. Internally
  queries the column `label` of the table in field `col_info`. Columns
  ids referenced by the name of the vector, the labels are the actual
  string values.

  Assigning to this column update the task by reference. You have to
  provide a character vector of labels, named with column ids. To remove
  a label, set it to `NA`. Alternatively, you can provide a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) with the two
  columns `"id"` and `"label"`.

- `col_hashes`:

  (named `character`)  
  Hash (unique identifier) for all columns except the `primary_key`: A
  `character` vector, named by the columns that each element refers
  to.  
  Columns of different `Task`s or
  [`DataBackend`](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)s
  that have agreeing `col_hashes` always represent the same data, given
  that the same `row`s are selected. The reverse is not necessarily
  true: There can be columns with the same content that have different
  `col_hashes`.

- `characteristics`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of characteristics of the task, e.g. `list(n = 5, p = 7)`.

- `row_ids_backend`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Returns all row ids from the backend, regardless of their roles. This
  is different from `$row_ids` which only returns rows with role "use".

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `task_type`:

  (`character(1)`)  
  Task type, e.g. `"classif"` or `"regr"`.

  For a complete list of possible task types (depending on the loaded
  packages), see
  [`mlr_reflections$task_types$type`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. Defaults to `NA`, but can be set by child classes.

- `extra_args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Additional arguments set during construction. Required for
  [`convert_task()`](https://mlr3.mlr-org.com/dev/reference/convert_task.md).

## Methods

### Public methods

- [`Task$new()`](#method-Task-new)

- [`Task$divide()`](#method-Task-divide)

- [`Task$help()`](#method-Task-help)

- [`Task$format()`](#method-Task-format)

- [`Task$print()`](#method-Task-print)

- [`Task$data()`](#method-Task-data)

- [`Task$formula()`](#method-Task-formula)

- [`Task$head()`](#method-Task-head)

- [`Task$levels()`](#method-Task-levels)

- [`Task$missings()`](#method-Task-missings)

- [`Task$filter()`](#method-Task-filter)

- [`Task$select()`](#method-Task-select)

- [`Task$rbind()`](#method-Task-rbind)

- [`Task$cbind()`](#method-Task-cbind)

- [`Task$rename()`](#method-Task-rename)

- [`Task$set_row_roles()`](#method-Task-set_row_roles)

- [`Task$set_col_roles()`](#method-Task-set_col_roles)

- [`Task$set_levels()`](#method-Task-set_levels)

- [`Task$droplevels()`](#method-Task-droplevels)

- [`Task$add_strata()`](#method-Task-add_strata)

- [`Task$materialize_view()`](#method-Task-materialize_view)

- [`Task$clone()`](#method-Task-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that this object is typically constructed via a derived classes,
e.g.
[TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md) or
[TaskRegr](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md).

#### Usage

    Task$new(id, task_type, backend, label = NA_character_, extra_args = list())

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `task_type`:

  (`character(1)`)  
  Type of task, e.g. `"regr"` or `"classif"`. Must be an element of
  [mlr_reflections\$task_types\$type](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

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

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `extra_args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named list of constructor arguments, required for converting task
  types via
  [`convert_task()`](https://mlr3.mlr-org.com/dev/reference/convert_task.md).

------------------------------------------------------------------------

### Method `divide()`

Deprecated.

#### Usage

    Task$divide(ratio = NULL, ids = NULL, remove = TRUE)

#### Arguments

- `ratio`:

  (`numeric(1)`)  
  The proportion of datapoints to use as validation data.

- `ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The row ids to use as validation data.

- `remove`:

  (`logical(1)`)  
  If `TRUE` (default), the `row_ids` are removed from the primary task's
  active `"use"` rows, ensuring a disjoint split between the train and
  validation data.

#### Returns

Modified `Self`.

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Task$help()

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Task$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Task$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`data()`](https://rdrr.io/r/utils/data.html)

Returns a slice of the data from the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md) as
a `data.table`. Rows default to observations with role `"use"`, and
columns default to features with roles `"target"` or `"feature"`. Rows
must be a subset of `$row_ids`. If `rows` or `cols` are specified which
do not exist in the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md), an
exception is raised.

Rows and columns are returned in the order specified via the arguments
`rows` and `cols`. If `rows` is `NULL`, rows are returned in the order
of `task$row_ids`. If `cols` is `NULL`, the column order defaults to
`c(task$target_names, task$feature_names)`. Note that it is recommended
to **not** rely on the order of columns, and instead always address
columns with their respective column name.

#### Usage

    Task$data(rows = NULL, cols = NULL, ordered = FALSE)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of column names.

- `ordered`:

  (`logical(1)`)  
  If `TRUE`, data is ordered according to the columns with column role
  `"order"`.

#### Returns

Depending on the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md),
but usually a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

#### Examples

    task = tsk("penguins")
    task$data(rows = 1:5, cols = c("species", "sex"))

------------------------------------------------------------------------

### Method [`formula()`](https://rdrr.io/r/stats/formula.html)

Constructs a [`formula()`](https://rdrr.io/r/stats/formula.html), e.g.
`[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`, using the
features provided in argument `rhs` (defaults to all columns with role
`"feature"`, symbolized by `"."`).

Note that it is currently not possible to change the formula. However,
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines)
provides a pipe operator interfacing
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) for
this purpose: `"modelmatrix"`.

#### Usage

    Task$formula(rhs = ".")

#### Arguments

- `rhs`:

  (`character(1)`)  
  Right hand side of the formula. Defaults to `"."` (all features of the
  task).

#### Returns

[`formula()`](https://rdrr.io/r/stats/formula.html).

#### Examples

    task = tsk("penguins")
    task$formula()

------------------------------------------------------------------------

### Method [`head()`](https://rdrr.io/r/utils/head.html)

Get the first `n` observations with role `"use"` of all columns with
role `"target"` or `"feature"`.

#### Usage

    Task$head(n = 6L)

#### Arguments

- `n`:

  (`integer(1)`).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with `n` rows.

#### Examples

    task = tsk("penguins")
    task$head(3)

------------------------------------------------------------------------

### Method [`levels()`](https://rdrr.io/r/base/levels.html)

Returns the distinct values for columns referenced in `cols` with
storage type "factor" or "ordered". Argument `cols` defaults to all such
columns with role `"target"` or `"feature"`.

Note that this function ignores the row roles, it returns all levels
available in the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md). To
update the stored level information, e.g. after subsetting a task with
`$filter()`, call `$droplevels()`.

#### Usage

    Task$levels(cols = NULL)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of column names.

#### Returns

named [`list()`](https://rdrr.io/r/base/list.html).

#### Examples

    task = tsk("penguins")
    task$levels()

------------------------------------------------------------------------

### Method `missings()`

Returns the number of missing observations for columns referenced in
`cols`. Considers only active rows with row role `"use"`. Argument
`cols` defaults to all columns with role "target" or "feature".

#### Usage

    Task$missings(cols = NULL)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of column names.

#### Returns

Named [`integer()`](https://rdrr.io/r/base/integer.html).

#### Examples

    task = tsk("penguins")
    task$missings()

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Subsets the task, keeping only the rows specified via row ids `rows`.

This operation mutates the task in-place. See the section on task
mutators for more information.

#### Usage

    Task$filter(rows)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    task$filter(1:10)
    task$nrow

------------------------------------------------------------------------

### Method `select()`

Subsets the task, keeping only the features specified via column names
`cols`. Note that you cannot deselect the target column, for obvious
reasons.

This operation mutates the task in-place. See the section on task
mutators for more information.

#### Usage

    Task$select(cols)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of column names.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    task$select(c("bill_length", "bill_depth"))
    task$feature_names

------------------------------------------------------------------------

### Method [`rbind()`](https://rdrr.io/r/base/cbind.html)

Adds additional rows to the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
stored in `$backend`. New row ids are automatically created, unless
`data` has a column whose name matches the primary key of the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
(`task$backend$primary_key`). In case of name clashes of row ids, rows
in `data` have higher precedence and virtually overwrite the rows in the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md).

All columns roles `"target"`, `"feature"`, `"weights_learner"`,
`"weights_measure"`, `"group"`, `"stratum"`, and `"order"` must be
present in `data`. Columns only present in `data` but not in the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md) of
`task` will be discarded.

This operation mutates the task in-place. See the section on task
mutators for more information.

#### Usage

    Task$rbind(data)

#### Arguments

- `data`:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html)).

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    extra = task$data(rows = 1:2)
    task$rbind(extra)

------------------------------------------------------------------------

### Method [`cbind()`](https://rdrr.io/r/base/cbind.html)

Adds additional columns to the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
stored in `$backend`.

The row ids must be provided as column in `data` (with column name
matching the primary key name of the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)).
If this column is missing, it is assumed that the rows are exactly in
the order of `$row_ids`. In case of name clashes of column names in
`data` and
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md),
columns in `data` have higher precedence and virtually overwrite the
columns in the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md).

This operation mutates the task in-place. See the section on task
mutators for more information.

#### Usage

    Task$cbind(data)

#### Arguments

- `data`:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html)).

#### Examples

    task = tsk("penguins")
    task$cbind(data.table(extra_col = seq_len(task$nrow)))
    head(task$data(cols = "extra_col"))

------------------------------------------------------------------------

### Method `rename()`

Renames columns by mapping column names in `old` to new column names in
`new` (element-wise).

This operation mutates the task in-place. See the section on task
mutators for more information.

#### Usage

    Task$rename(old, new)

#### Arguments

- `old`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Old names.

- `new`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  New names.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    task$rename("body_mass", "mass")
    task$feature_names

------------------------------------------------------------------------

### Method `set_row_roles()`

Modifies the roles in `$row_roles` **in-place**.

#### Usage

    Task$set_row_roles(rows, roles = NULL, add_to = NULL, remove_from = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids for which to change the roles for.

- `roles`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Exclusively set rows to the specified `roles` (remove from other
  roles).

- `add_to`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Add rows with row ids `rows` to roles specified in `add_to`. Rows keep
  their previous roles.

- `remove_from`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Remove rows with row ids `rows` from roles specified in `remove_from`.
  Other row roles are preserved.

#### Details

Roles are first set exclusively (argument `roles`), then added (argument
`add_to`) and finally removed (argument `remove_from`) from different
roles. Duplicated row ids are explicitly allowed, so you can add
replicate an observation by repeating its `row_id`.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    task$set_row_roles(1:5, remove_from = "use")

------------------------------------------------------------------------

### Method `set_col_roles()`

Modifies the roles in `$col_roles` **in-place**. See `$col_roles` for a
list of possible roles.

#### Usage

    Task$set_col_roles(cols, roles = NULL, add_to = NULL, remove_from = NULL)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Column names for which to change the roles for.

- `roles`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Exclusively set columns to the specified `roles` (remove from other
  roles).

- `add_to`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Add columns with column names `cols` to roles specified in `add_to`.
  Columns keep their previous roles.

- `remove_from`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Remove columns with columns names `cols` from roles specified in
  `remove_from`. Other column roles are preserved.

#### Details

Roles are first set exclusively (argument `roles`), then added (argument
`add_to`) and finally removed (argument `remove_from`) from different
roles. Duplicated columns are removed from the same role. For tasks that
only allow one target, the target column cannot be set with
`$set_col_roles()`. Use the `$col_roles` field to swap the target
column.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    task = tsk("penguins")
    task$set_col_roles("sex", roles = "stratum")
    task$col_roles$stratum

------------------------------------------------------------------------

### Method `set_levels()`

Set levels for columns of type `factor` and `ordered` in field
`col_info`. You can add, remove or reorder the levels, affecting the
data returned by `$data()` and `$levels()`. If you just want to remove
unused levels, use `$droplevels()` instead.

Note that factor levels which are present in the data but not listed in
the task as valid levels are converted to missing values.

#### Usage

    Task$set_levels(levels)

#### Arguments

- `levels`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [`character()`](https://rdrr.io/r/base/character.html))  
  List of character vectors of new levels, named by column names.

#### Returns

Modified `self`.

#### Examples

    task = tsk("penguins")
    task$set_levels(list(sex = c("male", "female", "unknown")))
    task$levels("sex")

------------------------------------------------------------------------

### Method [`droplevels()`](https://rdrr.io/r/base/droplevels.html)

Updates the cache of stored factor levels, removing all levels not
present in the current set of active rows. `cols` defaults to all
columns with storage type "factor" or "ordered".

#### Usage

    Task$droplevels(cols = NULL)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector of column names.

#### Returns

Modified `self`.

#### Examples

    task = tsk("penguins")
    task$set_levels(list(sex = c("male", "female", "unknown")))
    task$levels("sex")

------------------------------------------------------------------------

### Method `add_strata()`

Cuts numeric variables into new factors columns which are added to the
task with role `"stratum"`. This ensures that all training and test
splits contain observations from all bins. The columns are named
`"..stratum_[col_name]"`.

#### Usage

    Task$add_strata(cols, bins = 3L)

#### Arguments

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of columns to operate on.

- `bins`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of bins to cut into (passed to
  [`cut()`](https://rdrr.io/r/base/cut.html) as `breaks`). Replicated to
  have the same length as `cols`.

#### Returns

self (invisibly).

#### Examples

    task = tsk("penguins")
    task$add_strata("flipper_length", bins = 4)

------------------------------------------------------------------------

### Method `materialize_view()`

Certain operations change the view on the data, e.g., `$filter()` or
`$select()`. This operation queries the
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md) for
all data required in the active view and replaces the internal
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
with the new one. In some scenarios this helps to free up memory or
speeds up accesses to the data, especially after several `$rbind()` and
`$cbind()` operations.

#### Usage

    Task$materialize_view(internal_valid_task = TRUE)

#### Arguments

- `internal_valid_task`:

  (`logical(1)`)  
  Also materialize the internal validation task. Default is `TRUE`.

#### Details

For tasks containing the same observation more than once (duplicates in
`$row_ids`), the resulting backend contains it only once.

#### Returns

self (invisibly).

#### Examples

    task = tsk("iris")
    task$backend$nrow
    task$filter(1:120)
    task$backend$nrow

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Task$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# We use the inherited class TaskClassif here,
# because the base class `Task` is not intended for direct use
task = TaskClassif$new("penguings", palmerpenguins::penguins, target = "species")

task$nrow
#> [1] 344
task$ncol
#> [1] 8
task$feature_names
#> [1] "bill_depth_mm"     "bill_length_mm"    "body_mass_g"      
#> [4] "flipper_length_mm" "island"            "sex"              
#> [7] "year"             
task$formula()
#> species ~ .
#> NULL

# de-select "year"
task$select(setdiff(task$feature_names, "year"))

task$feature_names
#> [1] "bill_depth_mm"     "bill_length_mm"    "body_mass_g"      
#> [4] "flipper_length_mm" "island"            "sex"              

# Add new column "foo"
task$cbind(data.frame(foo = 1:344))
head(task)
#>    species bill_depth_mm bill_length_mm body_mass_g flipper_length_mm    island
#>     <fctr>         <num>          <num>       <int>             <int>    <fctr>
#> 1:  Adelie          18.7           39.1        3750               181 Torgersen
#> 2:  Adelie          17.4           39.5        3800               186 Torgersen
#> 3:  Adelie          18.0           40.3        3250               195 Torgersen
#> 4:  Adelie            NA             NA          NA                NA Torgersen
#> 5:  Adelie          19.3           36.7        3450               193 Torgersen
#> 6:  Adelie          20.6           39.3        3650               190 Torgersen
#>       sex   foo
#>    <fctr> <int>
#> 1:   male     1
#> 2: female     2
#> 3: female     3
#> 4:   <NA>     4
#> 5: female     5
#> 6:   male     6

## ------------------------------------------------
## Method `Task$data`
## ------------------------------------------------

task = tsk("penguins")
task$data(rows = 1:5, cols = c("species", "sex"))
#>    species    sex
#>     <fctr> <fctr>
#> 1:  Adelie   male
#> 2:  Adelie female
#> 3:  Adelie female
#> 4:  Adelie   <NA>
#> 5:  Adelie female

## ------------------------------------------------
## Method `Task$formula`
## ------------------------------------------------

task = tsk("penguins")
task$formula()
#> species ~ .
#> NULL

## ------------------------------------------------
## Method `Task$head`
## ------------------------------------------------

task = tsk("penguins")
task$head(3)
#>    species bill_depth bill_length body_mass flipper_length    island    sex
#>     <fctr>      <num>       <num>     <int>          <int>    <fctr> <fctr>
#> 1:  Adelie       18.7        39.1      3750            181 Torgersen   male
#> 2:  Adelie       17.4        39.5      3800            186 Torgersen female
#> 3:  Adelie       18.0        40.3      3250            195 Torgersen female
#>     year
#>    <int>
#> 1:  2007
#> 2:  2007
#> 3:  2007

## ------------------------------------------------
## Method `Task$levels`
## ------------------------------------------------

task = tsk("penguins")
task$levels()
#> $island
#> [1] "Biscoe"    "Dream"     "Torgersen"
#> 
#> $sex
#> [1] "female" "male"  
#> 
#> $species
#> [1] "Adelie"    "Chinstrap" "Gentoo"   
#> 

## ------------------------------------------------
## Method `Task$missings`
## ------------------------------------------------

task = tsk("penguins")
task$missings()
#>        species     bill_depth    bill_length      body_mass flipper_length 
#>              0              2              2              2              2 
#>         island            sex           year 
#>              0             11              0 

## ------------------------------------------------
## Method `Task$filter`
## ------------------------------------------------

task = tsk("penguins")
task$filter(1:10)
task$nrow
#> [1] 10

## ------------------------------------------------
## Method `Task$select`
## ------------------------------------------------

task = tsk("penguins")
task$select(c("bill_length", "bill_depth"))
task$feature_names
#> [1] "bill_depth"  "bill_length"

## ------------------------------------------------
## Method `Task$rbind`
## ------------------------------------------------

task = tsk("penguins")
extra = task$data(rows = 1:2)
task$rbind(extra)

## ------------------------------------------------
## Method `Task$cbind`
## ------------------------------------------------

task = tsk("penguins")
task$cbind(data.table(extra_col = seq_len(task$nrow)))
head(task$data(cols = "extra_col"))
#>    extra_col
#>        <int>
#> 1:         1
#> 2:         2
#> 3:         3
#> 4:         4
#> 5:         5
#> 6:         6

## ------------------------------------------------
## Method `Task$rename`
## ------------------------------------------------

task = tsk("penguins")
task$rename("body_mass", "mass")
task$feature_names
#> [1] "bill_depth"     "bill_length"    "mass"           "flipper_length"
#> [5] "island"         "sex"            "year"          

## ------------------------------------------------
## Method `Task$set_row_roles`
## ------------------------------------------------

task = tsk("penguins")
task$set_row_roles(1:5, remove_from = "use")

## ------------------------------------------------
## Method `Task$set_col_roles`
## ------------------------------------------------

task = tsk("penguins")
task$set_col_roles("sex", roles = "stratum")
task$col_roles$stratum
#> [1] "sex"

## ------------------------------------------------
## Method `Task$set_levels`
## ------------------------------------------------

task = tsk("penguins")
task$set_levels(list(sex = c("male", "female", "unknown")))
task$levels("sex")
#> $sex
#> [1] "male"    "female"  "unknown"
#> 

## ------------------------------------------------
## Method `Task$droplevels`
## ------------------------------------------------

task = tsk("penguins")
task$set_levels(list(sex = c("male", "female", "unknown")))
task$levels("sex")
#> $sex
#> [1] "male"    "female"  "unknown"
#> 

## ------------------------------------------------
## Method `Task$add_strata`
## ------------------------------------------------

task = tsk("penguins")
task$add_strata("flipper_length", bins = 4)

## ------------------------------------------------
## Method `Task$materialize_view`
## ------------------------------------------------

task = tsk("iris")
task$backend$nrow
#> [1] 150
task$filter(1:120)
task$backend$nrow
#> [1] 150
```
