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
    id = NA_character_,
    name = NULL,
    task_type = NA_character_,
    packages = NULL,
    par_set = NULL,
    properties = NULL,

    print = function(...) {
     catf("Learner '%s' for %s", self$id, self$task_type)
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
    if (!identical(class(task)[1L], learner$task_type)) {
      stopf("Learner '%s' (type: %s) is not compatible with task '%s' (type: %s)",
        learner$id, learner$task_type, task$id, class(task)[1L])
    }
  }
  invisible(learner)
}
