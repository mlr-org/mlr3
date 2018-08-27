#' @field id (`character(1)`)\cr
#'  Identifier of the task.
#' @field backend (`Backend`)\cr
#'  Internal abstraction for data access. Leave this alone unless you know exactly what you are doing.
#' @field get (`function(rows = NULL, cols = NULL)`)\cr
#'  Get (a subset of) the task data. Rows must be specified by row ids (defaulting to integers if not specified otherwise), columns must be addressed by name.
#' @field head (`function(n = 6L)`)\cr
#'  Fetches the first \code{n} rows of the data and returns a \code{data.table}.
#' @field features [character]\cr
#'  Names of the features.
#' @field formula [character]\cr
#'  Formula describing the learning task.
#' @field subset (`function(rows = NULL, cols = NULL)`)\cr
#'  Subset this task to contain only the specified rows and columns.
#' @field nrow (`integer(1)`)\cr
#'  Number of rows in the current view of the task.
#' @field ncol (`integer(1)`)\cr
#'  Number of columns in the current view of the task.
#' @field col.types (`named character`)\cr
#'  Column class information.
#' @field missing.values (`named integer`)\cr
#'  Number of missing values per column.
