#' @title Rolling Cross-Validation Resampling
#'
#' @name mlr_resamplings_cv
#' @include Resampling.R
#'
#' @description
#' Splits data into `folds` folds (default: 10 folds) w.r.t. the order of observations in task.
#' In the $i$-th iteration, the first $i$ folds serve as training set and the $i+1$-th fold as test set.
#'
#' @templateVar id cv
#' @template resampling
#'
#' @template seealso_resampling
#' @export
ResamplingRollingCV = R6Class("ResamplingRollingCV", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        folds = p_int(2L, tags = "required"),
        initial = p_int(lower = 1L, tags = "required"), # size of initial training set, in terms of unit
        cumulative = p_lgl(default = TRUE),
        unit = p_fct(levels = c("rows", "secs", "mins", "hours", "days", "months", "years"), default = "rows", tags = "required")
      )
      ps$values = list(folds = 10L, unit = "rows")

      super$initialize(id = "rolling_cv", param_set = ps,
        label = "Rolling Cross-Validation", man = "mlr3::mlr_resamplings_rolling_cv")
    },

    instantiate = function(task) {
      if (FALSE) {
        task = tsk("airpassengers")
        pv = list(folds = 3, unit = "years")
        order_col = task$col_roles$order
        tab = setnames(task$data(cols = c(task$backend$primary_key, order_col), ordered = TRUE), c("row_id", "group"))
        unit = "years"

        encode_by_unit = function(tab, unit) {
          switch(unit,
            "years" = set(tab, j = "group", value = year(tab[["group"]]))
          )
          grouped = as.data.table(unclass(rle(tab$group)))
          grouped$group = seq_row(grouped)
          grouped
        }

        encoded = encode_by_unit(tab, "year")

      }

      order_col = task$col_roles$order
      if (length(order_col) != 1L || fget(task$col_info, order_col, "type", "id") %nin% c("POSIXct", "Date")) {
        stopf("For 'unit' != \"rows\", the task must be ordered by a single POSIXct or Date column")
      }

      ids <- setnames(task$data(cols = c(task$backend$primary_key, order_col), ordered = TRUE), c("row_id", "group"))
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
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },

    deep_clone = function(name, value) {
      switch(name,
        "instance" = copy(value),
        "param_set" = value$clone(deep = TRUE),
        value
      )
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("rolling_cv", function() ResamplingRollingCV$new())
