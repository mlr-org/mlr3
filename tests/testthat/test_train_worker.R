context("train_worker")

LearnerTest = R6Class("LearnerTest", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "test") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        properties = "missings"
      )
    },

    train = function(task) {
      self$model = list()
      self
    },

    predict = function(task) {
      PredictionClassif$new(response = task$truth(1), task$nrow)
    }
  )
)

test_that("Handling of training errors", {
  learner = LearnerTest$new()
  e = Experiment$new(task = mlr_tasks$get("sonar"), learner = learner)
  r = ResamplingCustom$new()$instantiate(e$task, train_sets = list(1:150))
  e$data$resampling = r
  e$data$iteration = 1L

  res = train_worker(e$clone(), mlr_control())
  expect_list(res, len = 3)
  expect_learner(res$learner)
  expect_data_table(res$train_log, null.ok = TRUE)
  expect_number(res$train_time, lower = 0)

  LearnerTest$set("public", "train", function(task) self, overwrite = TRUE)
  learner = LearnerTest$new()
  e = Experiment$new(task = mlr_tasks$get("sonar"), learner = learner)
  e$data$resampling = r
  e$data$iteration = 1L

  expect_error(train_worker(e, mlr_control()), "store a model", class = "trainError")

  res = train_worker(e, mlr_control(encapsulate_train = "evaluate"))
  expect_string(Log$new(res$train_log)$errors, fixed = "store a model")
})
