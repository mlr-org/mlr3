#' @title Stack for Hot Start Learners
#'
#' @description
#' This class stores learners for hot starting training, i.e. resuming or
#' continuing from an already fitted model.
#' We assume that hot starting is only possible if a single hyperparameter
#' (also called the fidelity parameter, usually controlling the complexity or
#' expensiveness) is altered and all other hyperparameters are identical.
#'
#' The `HotstartStack` stores trained learners which can be potentially used to
#' hot start a learner. Learner automatically hot start while training if a
#' stack is attached to the `$hotstart_stack` field and the stack contains a
#' suitable learner.
#'
#' For example, if you want to train a random forest learner with 1000 trees but
#' already have a random forest learner with 500 trees (hot start learner),
#' you can add the hot start learner to the `HotstartStack` of the expensive learner
#' with 1000 trees. If you now call the `train()` method (or [resample()] or
#' [benchmark()]), a random forest with 500 trees will be fitted and combined
#' with the 500 trees of the hotstart learner, effectively saving you to
#' fit 500 trees.
#'
#' Hot starting is only supported by learners which have the property
#' `"hotstart_forward"` or `"hotstart_backward"`. For example, an `xgboost` model
#' (in \CRANpkg{mlr3learners}) can hot start forward by adding more boosting
#' iterations, and a random forest can go backwards by removing trees.
#' The fidelity parameters are tagged with `"hotstart"` in learner's parameter set.
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
HotstartStack = R6Class("HotstartStack",
  public = list(

    #' @field stack [data.table::data.table()]\cr
    #' Stores hot start learners.
    stack = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (List of [Learner]s)\cr
    #'   Learners are added to the hotstart stack. If `NULL` (default), empty
    #'   stack is created.
    initialize = function(learners = NULL) {
      self$stack = data.table()

      # add learners to stack
      if (!is.null(learners)) self$add(learners)
    },

    #' @description
    #' Add learners to hot start stack.
    #'
    #' @param learners (List of [Learner]s).
    #'   Learners are added to the hotstart stack.
    #'
    #' @return self (invisibly).
    add = function(learners) {
      learners = assert_learners(as_learners(learners))

      # hashes
      task_hash = map_chr(learners, function(learner) learner$state$task_hash)
      learner_hash = map_chr(learners, learner_hotstart_hash)

      self$stack = rbindlist(list(self$stack, data.table(start_learner = learners, task_hash, learner_hash)))
      setkeyv(self$stack, c("task_hash", "learner_hash"))

      invisible(self)
    },

    #' @description
    #' Calculates the cost for each learner of the stack to hot start the target
    #' `learner`.
    #'
    #' The following cost values can be returned:
    #'
    #' * `NA_real_`: Learner is unsuitable to hot start target `learner`.
    #' * `-1`: Hotstart learner in the stack and target `learner` are identical.
    #' * `0` Cost for hot starting backwards is always 0.
    #' * `> 0` Cost for hot starting forward.
    #'
    #' @param learner [Learner]\cr
    #'   Target learner.
    #' @param task_hash [Task]\cr
    #'   Hash of the task on which the target learner is trained.
    #'
    # @return `numeric()`.
    start_cost = function(learner, task_hash) {
      if(!nrow(self$stack)) return(numeric(0))
      .learner_hash = learner_hotstart_hash(assert_learner(learner))
      .task_hash = assert_string(task_hash)
      hotstart_id = learner$param_set$ids(tags = "hotstart")

      set(self$stack, j = "cost", value = NA_real_)
      cost = self$stack[list(.task_hash, .learner_hash), "cost" := map_dbl(get("start_learner"), function(l) calculate_cost(l, learner, hotstart_id)) , on = c("task_hash", "learner_hash")
        ][, get("cost")]
      self$stack[, "cost" := NULL]
      cost
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
      print(self$stack, digits = 2)
    }
  ),

  private = list(

    # Queries the stack for the start learner with the lowest cost of hot
    # starting the target `learner`. The start learner's state is copied to
    # `learner` and `learner` is returned. This method is internally used by
    # `Learner$train()` `resample()` and `benchmark()` which call
    # `learner_train(learner, task, row_ids, mode = 'retrain')` with the
    # returned learner.
    .start_learner = function(learner, task_hash) {
      if(!nrow(self$stack)) return(NULL)
      .learner_hash = learner_hotstart_hash(assert_learner(learner))
      .task_hash = assert_character(task_hash, len = 1)
      hotstart_id = learner$param_set$ids(tags = "hotstart")

      start_learner = self$stack[list(.task_hash, .learner_hash), on = c("task_hash", "learner_hash"), nomatch = NULL
        ][, "cost" := map_dbl(start_learner, function(l) calculate_cost(l, learner, hotstart_id))
        ][which_min(get("cost"), na_rm = TRUE), start_learner]

      if (!length(start_learner)) return(NULL)
      learner$state = start_learner[[1]]$state
      learner
    }
  )
)

#' @description
#' Calculates cost to hotstart learner.
#' @param start_learner ([Learner]).
#' @param learner ([Learner]).
#' @param hotstart_id (`character(1)`).
#'
#' @return `numeric(1)`.
#' @noRd
calculate_cost = function(start_learner, learner, hotstart_id) {
  if (is.null(start_learner)) return(NA_real_)

  cost = learner$param_set$values[[hotstart_id]] - start_learner$param_set$values[[hotstart_id]]
  if (cost == 0) return(-1)

  if ("hotstart_backward" %in% learner$properties && "hotstart_forward" %in% learner$properties) {
    if (cost < 0) 0 else cost
  } else if ("hotstart_backward" %in% learner$properties) {
    if (cost < 0) 0 else NA_real_
  } else {
    if (cost > 0) cost else NA_real_
  }
}

#' @description
#' Hash (unique identifier) for learner object, excluding parameter values
#' tagged with `hotstart`.
#'
#' @param learner [Learner].
#'
#' @return `character(1)`.
#' @noRd
learner_hotstart_hash = function(learner) {
  param_vals = learner$param_set$values
  hotstart_id = learner$param_set$ids(tags = "hotstart")
  train_ids = setdiff(learner$param_set$ids(tags = "train"), hotstart_id)
  train_vals = param_vals[names(param_vals) %in% train_ids]

  calculate_hash(class(learner), learner$id, learner$predict_type, learner$fallback$hash, train_vals)
}
