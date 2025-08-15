test_that("autotest catches error in train", {
  learner = lrn("classif.debug", error_train = 1)
  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_integer(result$seed)
  expect_task(result$task)
  expect_learner(result$learner)
  expect_null(result$prediction)
  expect_null(result$score)
  expect_string(result$error)
})

test_that("autotest catches error in predict", {
  learner = lrn("classif.debug", error_predict = 1)
  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_integer(result$seed)
  expect_task(result$task)
  expect_learner(result$learner)
  expect_null(result$prediction)
  expect_null(result$score)
  expect_string(result$error)
})

test_that("autotest recognizes learner that does not use .get_weights()", {
  learner = R6Class("learner", inherit = LearnerClassifDebug, private = list(
    .train = function(task) {
      structure(
        list(response = as.character(sample(task$truth(), 1L))),
        class = "classif.debug_model"
      )
    }
  ))$new()

  task = tsk("spam")

  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_string(result$error, pattern = "get_weights was not called")
})

test_that("autotest configure_learner works", {
  task = tsk("spam")
  learner = lrn("classif.debug", error_train = 1)
  # keep only feat_all_* tasks to save time
  exclude = "^feat_single|^missings|^weights|^utf8|^sanity"
  result = run_autotest(learner, 20, exclude)
  expect_false(result$ok)

  # example changes the learner's parameter during the autotest
  cfg_lrn = function(learner, task) {
    if (task$n_features > 1) {
      learner$param_set$set_values(error_train = 0)
    }
  }
  result = run_autotest(learner, 10, exclude = exclude, configure_learner = cfg_lrn)
  expect_true(result)
  expect_equal(learner$param_set$values$error_train, 1)
})

test_that("autotest on marshal / unmarshal", {
  learner = R6Class(
    "learner_broken_marshal",
    inherit = LearnerClassifDebug,
    public = list(
      marshal = function(...) {
        # do nothing
        invisible(self)
      }
    )
  )$new()

  task = tsk("spam")$clone(deep = TRUE)
  task$id = "feat_all_spam"
  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_string(result$error, pattern = "marshal")

  learner = R6Class(
    "learner_broken_marshal_class",
    inherit = LearnerClassifDebug,
    public = list(
      marshal = function(...) {
        # miss to set class "marshaled" in the model
        self$model = structure(list(
          marshaled = self$model, packages = "mlr3"),
          class = c("classif.debug_model_marshaled")
        )
      }
    )
  )$new()

  task = tsk("spam")$clone(deep = TRUE)
  task$id = "feat_all_spam"
  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_string(result$error, pattern = "marshal")


  learner = R6Class(
    "learner_broken_marshal",
    inherit = LearnerClassifDebug,
    public = list(
      unmarshal = function(...) {
        # do nothing
        invisible(self)
      }
    )
  )$new()

  task = tsk("spam")$clone(deep = TRUE)
  task$id = "feat_all_spam"
  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_string(result$error, pattern = "unmarshal")

  learner = R6Class(
    "learner_broken_marshal",
    inherit = LearnerClassifDebug,
    private = list(
      .predict = function(task) {
        # simulate a broken unmarshaled model
        if (!is.null(self$model$marshal_count) && self$model$marshal_count) {
          stop("unmarshaled model is broken")
        }
        super$.predict(task)
      }
    )
  )$new()
  learner$param_set$set_values(count_marshaling = TRUE)

  task = tsk("spam")$clone(deep = TRUE)
  task$id = "feat_all_spam"
  result = run_experiment(task, learner)
  expect_false(result$ok)
  expect_string(result$error, pattern = "unmarshaled model is broken")
})

test_that("autotest on encapsulation", {
  # error in train
  learner1 = R6Class(
      "learner_broken_marshal",
      inherit = LearnerClassifDebug,
      private = list(
        .train = function(task) {
          if (Sys.getenv("in_mirai") == "TRUE") {
            stop("Error in mirai process in train")
          }
          super$.train(task)
        }
      )
    )$new()
    task = tsk("spam")
    task$id = "feat_all_spam"

    with_mirai({
      mirai::everywhere({Sys.setenv(in_mirai = "TRUE")}, .compute = "mlr3_encapsulation")
      result = run_experiment(task, learner1)
    }, compute = "mlr3_encapsulation")

    expect_false(result$ok)
    expect_string(result$error, pattern = "Error in mirai process in train")

  # error in predict
  learner2 = R6Class(
    "learner_broken_marshal",
    inherit = LearnerClassifDebug,
    private = list(
      .predict = function(task) {
        if (Sys.getenv("in_mirai") == "TRUE") {
          stop("Error in mirai process in predict")
        }
        super$.predict(task)
      }
    )
  )$new()

  task = tsk("spam")
  task$id = "feat_all_spam"

  with_mirai({
    mirai::everywhere({Sys.setenv(in_mirai = "TRUE")}, .compute = "mlr3_encapsulation")
    result = run_experiment(task, learner2)
  }, compute = "mlr3_encapsulation")

  expect_false(result$ok)
  expect_string(result$error, pattern = "Error in mirai process in predict")

  # access on state
  learner3 = R6Class(
    "learner_broken_marshal",
    inherit = LearnerClassifDebug,
    private = list(
      .predict = function(task) {
        if (Sys.getenv("in_mirai") == "TRUE") {
          self$state$train_task$levels()
          super$.predict(task)
        }
        super$.predict(task)
      }
    )
  )$new()

  task = tsk("spam")
  task$id = "feat_all_spam"

  with_mirai({
    mirai::everywhere({Sys.setenv(in_mirai = "TRUE")}, .compute = "mlr3_encapsulation")
    result = run_experiment(task, learner3)
  }, compute = "mlr3_encapsulation")

  expect_false(result$ok)
  expect_string(result$error, pattern = "attempt to apply non-function")
})
