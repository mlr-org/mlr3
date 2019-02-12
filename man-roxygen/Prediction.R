#' @section Fields:
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Vector of row ids for which predictions are stored.
#'
#' * `truth` :: `any`\cr
#'   Vector of true labels.
#'
#' * `response` :: `any`\cr
#'   Vector of predicted labels.
#'
#' * `task_type` :: `character(1)`\cr
#'   <%= if (PredictionClass == "Base") "Stores the type of the [Task]." else paste("Set to \"", tolower(PredictionClass), "\" for this class.") %>
#'
#' * `predict_types` :: `character()`\cr
#'   Vector of predict types this object stores.
#'
