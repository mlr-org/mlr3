#' @section Fields:
#' * `backend` :: [DataBackend].
#'
#' * `col_info` :: [data.table::data.table()]\cr
#'   Table with with 3 columns:
#'   Column names of [DataBackend] are stored in column`id`.
#'   Column `type` holds the storage type of the variables, e.g. `integer`, `numeric` or `character`.
#'   Column `levels` keeps a list of possible levels for factor and character variables.
#'
#' * `col_roles` :: named `list()`\cr
#'   Each column (feature) can have an arbitrary number of roles in the learning task:
#'     - `"target"`: Labels to predict.
#'     - `"feature"`: Regular feature.
#'     - `"order"`: Data returned by `data()` is ordered by this column (or these columns).
#'     - `"group"`: During resampling, observations with the same value of the variable with role "group"
#'          are marked as "belonging together". They will be exclusively assigned to be either in the training set
#'          or the test set for each resampling iteration.
#'     - `"weights"`: Observation weights.
#'   `col_roles` keeps track of the roles with a named list of vectors of feature names.
#'   To alter the roles, use `t$set_col_role()`.
#'
#' * `row_roles` :: named `list()`\cr
#'   Each row (observation) can have an arbitrary number of roles in the learning task:
#'     - `"use"`: Use in train / predict / resampling.
#'     - `"validation"`: Hold the observations back unless explicitly requested.
#'   `row_roles` keeps track of the roles with a named list of vectors of feature names.
#'   To alter the role, use `set_row_role()`.
#'
#' * `feature_names` :: `character()`\cr
#'   Returns all column names with `role == "feature"`.
#'
#' * `feature_types` :: [data.table::data.table()]\cr
#'   Returns a table with columns `id` and `type` where `id` are the column names of "active" features of the task
#'   and `type` is the storage type.
#'
#' * `formula` :: `formula()`\cr
#'   Constructs a [stats::formula], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`, using
#'   the active features of the task.
#'
#' * `group` :: [data.table::data.table()]\cr
#'   Returns a table with columns `row_id` and `group` where `row_id` are the row ids and group is the value of the
#'   grouping variable. Returns `NULL` if there is no grouping.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) of the task.
#'
#' * `id` :: `character(1)`\cr
#'   Stores the identifier of the Task.
#'
#' * `measures` :: `list()` of [Measure]\cr
#'   Stores the measures to use for this task.
#'
#' * `ncol` :: `integer(1)`\cr
#'   Returns the total number of cols with role "target" or "feature".
#'
#' * `nrow` :: `integer(1)`\cr
#'   Return the total number of rows with role "use".
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Returns the row ids of the [DataBackend] for observations with with role "use".
#'
#' * `target_names` :: `character()`\cr
#'   Returns all column names with role "target".
#'
#' * `task_type` :: `character(1)`\cr
#'   <%= if (TaskClass %in% c("Base", "Supervised")) "Stores the type of the [Task]." else paste("Set to \"", tolower(TaskClass), "\" for this class.") %>
#'
#'
#' @section Methods:
#' * `data(rows = NULL, cols = NULL, format = NULL)`\cr
#'   (`integer()` | `character()`, `character()`, `character(1)`) -> `any`\cr
#'   Returns a slice of the data from the [DataBackend] in the format specified by `format`
#'   (depending on the [DataBackend], but usually a [data.table::data.table()]).
#'   Rows are subsetted to only contain observations with role "use".
#'   Columns are filtered to only contain features with roles "target" and "feature".
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' * `cbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Extends the [DataBackend] with additional columns.
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]). If this column is missing, it is assumed that the rows are exactly in the order of
#'   `t$row_ids`.
#'
#' * `rbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Extends the [DataBackend] with additional rows.
#'   The new row ids must be provided as column in `data`.
#'   If this column is missing, new row ids are constructed automatically.
#'
#' * `filter(rows)`\cr
#'   (`integer()` | `character()`) -> `self`\cr
#'  Subsets the task, reducing it to only keep the rows specified.
#'
#' * `select(cols)`\cr
#'   `character()` -> `self`\cr
#'   Subsets the task, reducing it to only keep the columns specified.
#'
#' * `levels(col)`\cr
#'   `character()` -> named `list()`\cr
#'   Returns  the distinct levels of the column `col`.
#'   Only applicable for features with type "character",  "factor" or "ordered".
#'   This function ignores the row roles, it returns all levels available in the [DataBackend].
#'
#' * `head(n = 6)`\cr
#'   `integer()` -> [data.table::data.table()]\cr
#'   Get the first `n` observations with role "use".
#'
#' * `replace_features(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Replaces some features of the task by constructing a completely new [DataBackendDataTable].
#'   This operation is similar to calling `select()` and `cbind()`, but explicitly copies the data.
#'
#' * `set_col_role(cols, new_roles, exclusive = TRUE)`
#'   (`character()`, `character()`, `logical(1)`) -> `self`.
#'   Adds the roles `new_roles` to columns referred to by `cols`.
#'   If `exclusive` is `TRUE`, the referenced columns will be removed from all other roles.
#'
#' * `set_row_role(rows, new_roles, exclusive = TRUE)`
#'   (`character()`, `character()`, `logical(1)`) -> `self`.
#'   Adds the roles `new_roles` to rows referred to by `rows`.
#'   If `exclusive` is `TRUE`, the referenced rows will be removed from all other roles.
#'
#' @section Task mutators:
#' The methods `filter()`, `select()`, `rbind()`, and `cbind()` change the task in-place,
#' but without modifying the [DataBackend].
#' `filter()` and `select()` just reduce the set of active rows or columns, providing a different view on the data.
#' `rbind()` and `cbind()` first create a new [DataBackendDataTable] from the provided new data, and then
#' merge both backends into an abstract [DataBackend] which combines the results on-demand.
