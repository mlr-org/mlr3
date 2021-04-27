ResamplingRollingOrigin = R6Class("ResamplingRollingOrigin", inherit = Resampling,
  public = list(
    initialize = function() {
      ps = ps(
        # train_size ?
        initial = p_int(lower = 1L, tags = "required"),              # size of (first) training set
        assess = p_int(lower = 1L, default = 1L, tags = "required"), # size of predict set
        cumulative = p_lgl(default = TRUE),                          # training set grows with each iteration?
        skip = p_int(lower = 0L, default = 0L),                      # how many units to move right in each iteration?
        lag = p_int(lower = 0L, default = 0L),                       # number of units to leave out between train and predict
        unit = p_fct(levels = c("rows", "secs", "mins", "hours", "days", "months", "years"),
          default = "rows", tags = "required")
      )
      ps$values = list(initial = 1L, assess = 1L, unit = "rows")

      super$initialize(id = "rolling_origin", param_set = ps, man = "mlr3::mlr_resamplings_rolling_origin")
    },

    instantiate = function(task) {
      if (FALSE) {
        task = tsk("airpassengers")
      }
      pv = self$param_set$get_values()
      cumulative = pv$cumulative %??% TRUE
      skip = pv$skip %??% 0L
      lag = pv$lag %??% 0L

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
  )
)
