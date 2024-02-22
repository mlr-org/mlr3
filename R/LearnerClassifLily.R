#' @title Lily and marshal
#'
#' @name mlr_learners_classif.lily
#' @include LearnerClassifDebug.R
#'
#' @description
#' This learner is just like [`LearnerClassifDebug`], but can be marshaled.
#' When the `count_marshaling` parameter is `TRUE`, the model contains a `marshal_count` that will be increased
#' by 1, each time `marshal_model` is called.
#'
#' @templateVar id classif.lily
#' @template learner
#'
#' @export
LearnerClassifLily = R6Class("LearnerClassifLily",
  inherit = LearnerClassifDebug,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      self$param_set$add(ps(count_marshaling = p_lgl(tags = c("train", "required"))))
      self$param_set$values$count_marshaling = FALSE
      self$properties = sort(c("marshal", self$properties))
      self$man = "mlr3::mlr_learners_classif.lily"
      self$label = "Lily Learner"
      self$id = "classif.lily"
    },
    #' @description
    #' Marshals the learner.
    marshal = function() {
      learner_marshal(self)
    },
    #' @description
    #' Unmarshal the learner.
    unmarshal = function() {
      learner_unmarshal(self)
    }
  ),
  active = list(
    #' @field marshaled (logical(1))\cr
    #' Whether the learner has been marshaled.
    marshaled = function() {
      learner_marshaled(self)
    }
  ),
  private = list(
    .train = function(task) {
      model = super$.train(task)
      if (self$param_set$values$count_marshaling) {
        model$marshal_count = 0L
      }
      class(model) = "classif_lily_model"
      return(model)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.lily", function() LearnerClassifLily$new())

#' @export
marshal_model.classif_lily_model = function(model, ...) {
  if (!is.null(model$marshal_count)) {
    model$marshal_count = model$marshal_count + 1
  }
  newclass = c("classif_lily_model_marshaled", "marshaled")
  structure(list(marshaled = model, packages = "mlr3"), class = newclass)
}

#' @export
unmarshal_model.classif_lily_model_marshaled = function(model, ...) {
  model$marshaled
}
