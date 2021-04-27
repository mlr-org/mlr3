#' @title Rolling Forecast Resampling
#'
#' @name mlr_resamplings_rolling_forecast
#' @include Resampling.R
#'
#' @description
#' TODO
#'
#' @templateVar id rolling_forecast
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `test_size` (`integer(1)`)\cr
#'   Number of elements in the test windows, measured in terms of `units`.
#' * `folds` (`integer(1)`)\cr
#'   Number of splits.
#' * `steps_ahead` (`integer(1)`)\cr
#'   Number of steps between the last observation in the training set and the first
#'   observation in the test set.
#' * `unit` (`character(1)`)\cr
#'   Either `"rows"` (default) to split based on the row number, or, if there is a single order column
#'   of type [POSIXct], one of `"secs"`, `"mins"`, `"hours"`, `"days"`, `"months"`, or `"years"`.
#'
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("airpassengers")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rolling_forecast = rsmp("rolling_forecast", test_size = 3, folds = 2)
#' rolling_forecast$instantiate(task)
#' rolling_forecast$iters
#'
#' # Individual sets:
#' rolling_forecast$train_set(1)
#' rolling_forecast$test_set(1)
#' rolling_forecast$train_set(2)
#' rolling_forecast$test_set(2)
#'
#' # Disjunct sets:
#' intersect(rolling_forecast$train_set(1), rolling_forecast$test_set(1))
#'
#' # Internal storage:
#' rolling_forecast$instance # table
ResamplingRollingForecast = R6Class("ResamplingRollingForecast", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        # train_size ?
        test_size = p_int(lower = 0L, default = 1L, tags = "required"),
        folds = p_int(lower = 0L, default = 1L, tags = "required"),
        steps_ahead = p_int(lower = 1L, default = 1L, tags = "required"),
        unit = p_fct(levels = c("rows", "secs", "mins", "hours", "days", "months", "years"),
          default = "rows", tags = "required")
      )
      ps$values = list(test_size = 1L, folds = 1L, steps_ahead = 1L, unit = "rows")

      super$initialize(id = "rolling_forecast", param_set = ps, man = "mlr3::mlr_resamplings_rolling_forecast")
    },

    #' @description
    #' Instantiate this [Resampling].
    #'
    #' @param task [Task].
    instantiate = function(task) {
      # TODO:
      # Check if we can use the standard `.sample()` approach.
      # This should be straight forward, but we need to raise an exception if stratification or grouping
      # is also active.
      # Same for ordered holdout.
      #
      # # TODO:
      # This resampling supersedes ordered holdout if we introduce a ratio parameter
      pv = self$param_set$get_values()

      order_col = task$col_roles$order
      if (length(order_col) != 1L || fget(task$col_info, order_col, "type", "id") %nin% c("POSIXct", "Date")) {
        stopf("For 'unit' != \"rows\", the task must be ordered by a single POSIXct or Date column")
      }
      ids = setnames(task$data(cols = c(task$backend$primary_key, order_col), ordered = TRUE), c("row_id", "group"))

      if (pv$unit == "rows") {
        n_groups = nrow(ids)
        set(ids, j = "group", value = seq_row(ids))
      } else {
        group = cut(ids$group, breaks = pv$unit) # FIXME: include.lowest?
        n_groups = nlevels(group)
        set(ids, j = "group", value = as.integer(group))
      }

      tmp = rep(seq_len(pv$folds), each = pv$test_size)
      blocks = data.table(
        group = seq_len(n_groups),
        block = c(rep(0L, n_groups - length(tmp)), tmp)
      )

      self$instance = setkeyv(merge(ids, blocks, by = "group", all.x = TRUE, all.y = FALSE)[, c("row_id", "block")], "block")[]
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      as.integer(self$param_set$values$folds)
    }
  ),

  private = list(
    .get_train = function(i) {
      row_id = block = NULL
      ids = self$instance[block < i, row_id]
      steps = self$param_set$values$steps_ahead
      if (steps == 1L) {
        ids
      } else {
        head(ids, - (steps - 1L))
      }
    },

    .get_test = function(i) {
      row_id = block = NULL
      self$instance[block == i, row_id]
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("rolling_forecast", ResamplingRollingForecast)
