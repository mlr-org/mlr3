#' @title Classification Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for classification problems:
#'
#' * `task_type` is set to `"classif"`.
#' * Creates [Prediction]s of class [PredictionClassif].
#' * Possible values for `predict_types` are:
#'   - `"response"`: Predicts a class label for each observation in the test set.
#'   - `"prob"`: Predicts the posterior probability for each class for each observation in the test set.
#' * Additional learner properties include:
#'   - `"twoclass"`: The learner works on binary classification problems.
#'   - `"multiclass"`: The learner works on multiclass classification problems.
#'
#' Predefined learners can be found in the [dictionary][mlr3misc::Dictionary] [mlr_learners].
#' Essential classification learners can be found in this dictionary after loading \CRANpkg{mlr3learners}.
#' Additional learners are implement in the Github package \url{https://github.com/mlr-org/mlr3extralearners}.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @template seealso_learner
#' @export
#' @examples
#' # get all classification learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^classif"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = lrn("classif.rpart")
#' print(lrn)
#'
#' # train the learner:
#' task = tsk("penguins")
#' lrn$train(task, 1:200)
#'
#' # predict on new observations:
#' lrn$predict(task, 201:344)$confusion
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), predict_types = "response", feature_types = character(), properties = character(), packages = character(), label = NA_character_, man = NA_character_) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties, packages = packages,
        label = label, man = man)

      if (getOption("mlr3.prob_as_default", FALSE) && "prob" %in% self$predict_types) {
        self$predict_type = "prob"
      }
    },

    #' @description
    #' Predicts outcomes for new data in `newdata` using the model fitted during `$train()`.
    #' This method is faster than `$predict_newdata()` as it skips assertions, type conversions, encapsulation, and logging.
    #'
    #' Unlike `$predict_newdata()`, this method does not return a [Prediction] object.
    #' Instead, it returns a list with either a `"response"` or `"prob"` element, depending on the prediction type.
    #'
    #' Note that `state$predict_time` and `state$log` will remain empty after using this method.
    #' Some learners may not support this method and may fail when it is called.
    #' Prefer `$predict_newdata()` unless performance is critical.
    #'
    #' If the model was trained via [resample()] or [benchmark()], you must pass the associated task object stored in the corresponding [ResampleResult] or [BenchmarkResult].
    #'
    #' @param newdata [`data.table::data.table()`]\cr
    #'   New data to predict on.
    #' @param task ([Task]).
    #'
    #' @return `list()` with elements `"response"` or `"prob"` depending on the predict type.
    predict_newdata_fast = function(newdata, task = NULL) {
      if (is.null(task) && is.null(self$state$train_task)) stopf("No task stored, and no task provided")
      feature_names = self$state$train_task$feature_names %??% task$feature_names
      class_names = self$state$train_task$class_names %??% task$class_names

      # add data and most common used meta data
      fake_task = list(
        # some learners require that newdata contains the features names in the same order as the training data
        data = function(...) newdata[, feature_names, with = FALSE],
        class_names = class_names,
        feature_names = feature_names,
        nrow = nrow(newdata)
      )

      # train failed, use fallback
      if (is.null(self$model) && !is.null(self$state$fallback_state$model)) {
        return(self$fallback$predict_newdata_fast(newdata))
      }
      pred = get_private(self)$.predict(fake_task)


      # predict missing predictions with fallback
      miss = logical(fake_task$nrow)
      if (!is.null(pred$response)) {
        miss = is.na(pred$response)
      }

      if (!is.null(pred$prob)) {
        miss = miss | apply(pred$prob, 1L, anyMissing)
      }

      miss_ids = which(miss)
      if (length(miss_ids) && !is.null(self$state$fallback_state$model)) {
        pred_miss = self$fallback$predict_newdata_fast(newdata[miss_ids, ])

        if (!is.null(pred$response)) {
          pred$response[miss_ids] = pred_miss$response
        }

        if (!is.null(pred$prob)) {
          pred$prob[miss_ids, ] = pred_miss$prob
        }
      }

      return(pred)
    }
  )
)
