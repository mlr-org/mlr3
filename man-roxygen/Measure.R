#' @section Construction:
#' ```
#' m = Measure<%= if (MeasureClass != "Base") MeasureClass %>$new(id, <%= if (MeasureClass == "Base") "task_type, " %>range, minimize, predict_type = "response",
#'      task_properties = character(0L), packages = character(0L))
#' ```
#' * `id` :: `character(1)\cr
#'   Identifier for the measure.
#'
#' <%= if (MeasureClass == "Base") "
#' * `task_type` :: `character(1)`\\cr
#'   Type of the task the measure can operator on. E.g., `\"classif\"` or `\"regr\"`.
#' " %>
#'
#' * `range` :: `numeric(2)`\cr
#'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
#'
#' * `minimize` :: `logical(1)`\cr
#'   Set to `TRUE` if good predictions correspond to small values.
#'
#' * `predict_type` :: `character(1)`\cr
#'   Required predict type of the [Learner].
#'
#' * `task_properties` :: `character()`\cr
#'   Required task properties, see [Task].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' @section Public:
#' * `id` :: `character(1)`\cr
#'   Stores the identifier of the measure.
#'
#' * `minimize` :: `logical(1)`\cr
#'   Is `TRUE` if the best value is reached via minimization and `FALSE` by maximization.
#'
#' * `packages` :: `character()`\cr
#'   Stores the names of required packages.
#'
#' * `range` :: `numeric(2)`\cr
#'   Stores the feasible range of the measure.
#'
#' * `task_type` :: `character(1)`\cr
#'   <%= if (MeasureClass == "Base") "Stores the required type of the [Task]." else paste("Set to \"", tolower(MeasureClass), "\" for this class.") %>
#'
#' * `task_properties` :: `character()`\cr
#'   Stores required properties of the [Task].
#'
#'
#' @section Methods:
#' * `aggregate(rr)`\cr
#'   [ResampleResult] -> `numeric(1)`\cr
#'   Aggregates multiple performance scores into a single score using the `aggregate` function of the measure.
#'   Operates on a [ResampleResult] as returned by [resample].
#'
#' * `calculate(e)`\cr
#'   [Experiment] -> `numeric(1)`\cr
#'   Takes an [Experiment], extracts the predictions (as well as other possibly needed objects), and calculates
#'   a score.
