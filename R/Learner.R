#' @title Class for Learners
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct learners.
#'
#' @return [[Learner()]].
#' @family Learner
#' @export
Learner = R6Class("Learner",
  public = list(
    id = NULL,
    name = NULL,
    task_type = NULL,
    packages = NULL,
    par_set = NULL,
    properties = NULL,
    train = NULL,
    predict = NULL,

    initialize = function(task_type, name, par_set, par_vals, packages, properties, train, predict, predict_type) {
      self$task_type = assert_string(task_type)
      self$name = assert_string(name)
      self$id = stri_paste(task_type, ".", name)
      self$par_set = assert_class(par_set, "ParamSet")
      self$packages = assert_character(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assert_character(properties, any.missing = FALSE, unique = TRUE)
      self$train = assert_function(train, args = c("task", "row_ids"), ordered = TRUE)
      self$predict = assert_function(predict, args = c("model", "task", "row_ids"), ordered = TRUE)
      private$.par_vals = assert_list(par_vals, names = "unique")
      private$.predict_type = assert_choice(predict_type, capabilities$predict_types[[self$task_type]], fmatch = TRUE)

      # set environments for functions
      environment(self$train) = environment(self$predict) = environment(self$initialize)
    },

    print = function(...) {
     catf("Learner '%s' for %s", self$id, self$task_type)
     catf("predict_type: %s", self$predict_type)
     catf(stri_list("Properties: ", self$properties))
    }
  ),
  active = list(
    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      assert_list(rhs, names = "unique")
      assert_subset(names(rhs), self$par_set$ids)
      private$.par_vals[names(rhs)] = rhs
    },

    predict_type = function(rhs) {
      if (missing(rhs))
        return(private$.predict_type)
      assert_choice(rhs, capabilities$predict_types[[self$task_type]], fmatch = TRUE)
      private$.predict_type = rhs
    }
  ),
  private = list(
    .par_vals = NULL,
    .predict_type = NULL
  )
)

assert_learner = function(learner, task = NULL) {
  assert_r6(learner, "Learner")
  if (!is.null(task)) {
    if (!identical(task$task_type, learner$task_type)) {
      stopf("Learner '%s' (type: %s) is not compatible with task '%s' (type: %s)",
        learner$id, learner$task_type, task$id, task$task_type)
    }
  }
  invisible(learner)
}
