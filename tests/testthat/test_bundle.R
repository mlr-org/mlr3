test_that("bundleable learner behaves as expected", {
  task = tsk("mtcars")
  LearnerRegrTest = R6Class("LearnerRegrTest",
    inherit = LearnerRegrFeatureless,
    public = list(
      initialize = function() {
        super$initialize()
        self$id = "regr.test"
        self$properties = c("bundle", self$properties)
      },
      bundle = function() {
        learner_bundle(self)
      },
      unbundle = function() {
        learner_unbundle(self)
      }
    ),
    private = list(
      .bundle = function(model) {
        private$.tmp_model = model
        "bundle"
      },
      .unbundle = function(model) {
        model = private$.tmp_model
        private$.tmp_model = NULL
        private$.counter = private$.counter + 1
        model
      },
      .tmp_model = NULL,
      .counter = 0
    ),
    active = list(
      bundled = function() {
        learner_bundled(self)
      }
    )
  )
  learner = LearnerRegrTest$new()
  # bundleable learner
  expect_true("bundle" %in% learner$properties)
  expect_bundleable(learner, task)

  # callr encapsulation causes bundling
  learner2 = LearnerRegrTest$new()
  learner2$encapsulate = c(train = "callr")
  learner2$train(task)

  learner3 = LearnerRegrTest$new()
  learner3$encapsulate = c(train = "try")
  learner3$train(task)
  expect_false(learner3$bundled)

  # for callr, we had to unbundle
  expect_equal(get_private(learner2)$.counter, 1)
  # for other encapsulation, no need to unbundle becausse it was not bundled
  expect_equal(get_private(learner3)$.counter, 0)

  learner4 = LearnerRegrTest$new()
  learner4$train(task)
  # invisible
  expect_invisible(learner$bundle())
  expect_invisible(learner$unbundle())

  # bundling ResampleResult without stored models does not break state
  rr = resample(task, learner4, rsmp("cv"), store_models = FALSE)
  s1 = map(rr$learners, function(x) x$state)
  rr$bundle()
  s2 = map(rr$learners, function(x) x$state)
  expect_equal(s1, s2)

  # bundles is FALSE when model was nulled
  learner5 = LearnerRegrTest$new()
  learner5$train(task)
  learner5$bundle()
  learner5$model = NULL
  expect_false(learner5$bundled)
})

test_that("unbundleable learner's bundle is NULL", {
  learner = lrn("regr.featureless")
  learner$train(tsk("mtcars"))
  expect_true(is.null(learner$state$bundled))
})
