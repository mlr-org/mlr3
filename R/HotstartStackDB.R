#' @title Database for Hot Start Learners
#'
#' @description
#' This class stores learners for hot starting. When fitting a learner
#' repeatedly on the same task but with a different fidelity, hot starting
#' accelerates model fitting. The learner reuses a previously fitted model while
#' training e.g. adding trees to a random forest.
#'
#' The `HotstartStack` stores trained learners which can be potentially used to
#' hot start a learner. Learner automatically hot start while training if a
#' stack is attached to the `$hotstart_stack` field and the stack contains a
#' suitable learner (see examples).
#'
#' Hot starting is only supported by learners which have the property
#' `"hotstart_forward"` or `"hotstart_backward"`. For example, an xgboost model
#' can hot start forward by adding more boosting iterations and a random forest
#' can go backwards by removing trees. The fidelity parameters are tagged with
#' `"hotstart"` in learner's parameter set.
#'
#' The `HotstartStackDB` stores the learners in an \CRANpkg{RSQLite} database
#' and is generally slower than [HotstartStack], which stores the learners
#' locally in a [`data.table::data.table()`]. However, this class allows to
#' access the data in \CRANpkg{future} workers without copying the stack.
#'
#' @export
#' @examples
#' # train learner on pima task
#' task = tsk("pima")
#' learner = lrn("classif.debug", iter = 1)
#' learner$train(task)
#'
#' # initialize stack with previously fitted learner
#' hot = HotstartStack$new(list(learner))
#'
#' # retrieve learner with increased fidelity parameter
#' learner = lrn("classif.debug", iter = 2)
#'
#' # calculate cost of hot starting
#' hot$start_cost(learner, task$hash)
#'
#' # add stack with hot start learner
#' learner$hotstart_stack = hot
#'
#' # train automatically uses hot start learner while fitting the model
#' learner$train(task)
HotstartStackDB = R6Class("HotstartStackDB",
  public = list(

    #' @field stack [data.table::data.table()]\cr
    #' Stores hot start learners.
    stack = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (List of [Learner]s)\cr
    #'  If `NULL` (default), empty database is created.
    initialize = function(learners = NULL) {
      requireNamespace("RSQLite")

      # create table on disk in temp
      self$stack = tempfile(fileext = ".db")
      con = self$connection
      DBI::dbCreateTable(con, "stack",
        c(task_hash = "VARCHAR", learner_hash = "VARCHAR", hotstart_value = "DECIMAL", state = "BLOB"))

      if (!is.null(learners)) {
        # add learners to database
        self$add(learners)

        # set index for fast subsetting
        DBI::dbExecute(con, "CREATE INDEX task_learner_index ON stack (task_hash, learner_hash)")
      }
    },

    #' @description
    #' Add learners to hot start stack.
    #'
    #' @param learners (List of [Learner]s).
    #'
    #' @return self (invisibly).
    add = function(learners) {
      learners = assert_learners(as_learners(learners))
      con = self$connection

      # record value of hotstart parameter
      hotstart_value = map_dbl(learners, function(learner) {
        if (all(c("hotstart_forward", "hotstart_backward") %nin% learner$properties)) return(NA_real_)
        learner$param_set$values[[learner$param_set$ids(tags = "hotstart")]] %??% NA_real_
      })

      # serialize state of learner
      state = map(learners, function(learner) serialize(learner$state, NULL))

      # hashes
      task_hash = map_chr(learners, function(learner) learner$state$task_hash)
      learner_hash = map_chr(learners, learner_hotstart_hash)

      stack = data.table(task_hash, learner_hash, hotstart_value, state)
      DBI::dbAppendTable(con, "stack", stack)

      invisible(self)
    },

    #' @description
    #' Calculates the cost for each learner of the stack to hot start `learner`.
    #'
    #' The following cost values can be returned:
    #'
    #' * `NA_real_`: Learner is unsuitable to hot start `learner`.
    #' * `-1`: Learner in the stack and `learner` are identical.
    #' * `0` Cost for hot starting backwards are always 0.
    #' * `> 0` Cost for hot starting forward.
    #'
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #'
    # @return `numeric()`.
    start_cost = function(learner, task_hash) {
      learner_hash = learner_hotstart_hash(assert_learner(learner))
      task_hash = assert_string(task_hash)
      hotstart_id = learner$param_set$ids(tags = "hotstart")
      con = self$connection

      # target learner is not able to hotstart
      if (!length(hotstart_id)) {
        return(rep(NA_real_, DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM stack")$count))
      }
      hotstart_value = learner$param_set$values[[hotstart_id]]

      cost = DBI::dbGetQuery(con,
        sprintf("SELECT s2.cost FROM stack AS s1 LEFT JOIN (SELECT rowid, %f - hotstart_value as cost FROM stack WHERE task_hash = '%s' AND learner_hash = '%s') AS s2 ON s1.rowid = s2.rowid",
          hotstart_value, task_hash, learner_hash))$cost

      map_dbl(cost, function(x) calculate_cost_db(x, learner))
    },

    #' @description
    #' Finalizer which disconnects from the database.
    #' This is called during garbage collection of the instance.
    #' @return `logical(1)`, the return value of [DBI::dbDisconnect()].
    finalize = function() {
      DBI::dbDisconnect(self$connection, shutdown = TRUE)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #'
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      print(DBI::dbGetQuery(self$connection, "SELECT * FROM stack LIMIT 10"),  digits = 2)
      cat("(First 10 rows only.)")
    }
  ),

  private = list(

    # Queries the stack for the start learner with the lowest cost of hot
    # starting `learner`. The start learner's state is copied to `learner` and
    # `learner` is returned. This method is internally used by `Learner$train()`
    # `resample()` and `benchmark()` which call `learner_train(learner, task,
    # row_ids, mode = 'retrain')` with the returned learner.
    .start_learner = function(learner, task_hash) {
      learner_hash = learner_hotstart_hash(assert_learner(learner))
      assert_string(task_hash)
      hotstart_id = learner$param_set$ids(tags = "hotstart")
      if (!length(hotstart_id)) return(NULL)
      hotstart_value = learner$param_set$values[[hotstart_id]]
      con = self$connection

      # filtered stack contains multiple versions of the same learner with different fidelity levels
      stack = DBI::dbGetQuery(con,
        sprintf("SELECT state, %f - hotstart_value as cost FROM stack WHERE task_hash = '%s' AND learner_hash = '%s'",
          hotstart_value, task_hash, learner_hash))
      if (!nrow(stack)) return(NULL)

      # select learner with minimum cost and in case of ties, use random sampling
      idx = which_min(map_dbl(stack$cost, function(x) calculate_cost_db(x, learner)), na_rm = TRUE)
      if (!length(idx)) return(NULL)
      state = unserialize(stack$state[[idx]])

      learner$state = state
      learner
    }
  ),

  active = list(
    connection = function() {
      DBI::dbConnect(RSQLite::SQLite(), self$stack)
    }
  )
)

calculate_cost_db = function(cost, learner) {
  if (is.na(cost)) return(NA_real_)
  if (cost == 0) return(-1)

  if ("hotstart_backward" %in% learner$properties && "hotstart_forward" %in% learner$properties) {
    if (cost < 0) 0 else cost
  } else if ("hotstart_backward" %in% learner$properties) {
    if (cost < 0) 0 else NA_real_
  } else {
    if (cost > 0) cost else NA_real_
  }
}
