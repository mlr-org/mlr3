test_that("best valid score", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.2)$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(
    rr$score(msr("best_valid_score", select = "acc"))$acc,
    rr$learners[[1]]$best_valid_scores$acc
  )
  expect_equal(
    rr$score(msr("best_valid_score", select = "wrong_name"))$wrong_name,
    NA_real_
  )
  rr = resample(task, lrn("classif.rpart"), rsmp("holdout"))
  expect_equal(
    rr$score(msr("best_valid_score", select = "acc"))$acc,
    NA_real_
  )
  expect_measure(msr("best_valid_score"))

  # learner that does not have it
  m = msr("best_valid_score", select = "a")
  task = tsk("mtcars")
  learner = lrn("regr.debug")
  learner$train(task)
  pred = learner$predict(task)
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(rr$score(m)$a, NA_real_)

  task = tsk("iris")
  # the first validation score is taken by default
  rr = resample(task, lrn("classif.debug", predict_type = "prob", validate = 0.2), rsmp("holdout"))

  m = msr("best_valid_score")
  expect_equal(
    rr$score(m)$best_valid_score,
    rr$learners[[1]]$best_valid_scores[[1L]]
  )

  measure = msr("best_valid_score")
  expect_equal(measure$id, "best_valid_score")
  expect_equal(measure$minimize, NA)
  measure = msr("best_valid_score", select = "acc", minimize = TRUE)
  expect_equal(measure$id, "acc")
  expect_true(measure$minimize)
})

test_that("best valid score differs from final valid score with early stopping", {
  task = tsk("iris")
  learner = lrn(
    "classif.debug",
    validate = 0.2,
    predict_type = "prob",
    early_stopping = TRUE,
    iter = 10
  )
  learner$train(task)

  # without a validation curve, best and final coincide
  learner_no_es = lrn("classif.debug", validate = 0.2)$train(task)
  expect_equal(learner_no_es$best_valid_scores, learner_no_es$internal_valid_scores)

  # with early stopping, the best score is at least as good as the final one
  expect_names(names(learner$best_valid_scores), permutation.of = c("acc", "mbrier"))
  expect_true(learner$best_valid_scores$acc >= learner$internal_valid_scores$acc)
  expect_true(learner$best_valid_scores$mbrier <= learner$internal_valid_scores$mbrier)

  # both measures are available side by side during resampling.
  # their ids clash when `select` is given, so we check them one at a time
  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(
    rr$score(msr("best_valid_score", select = "acc"))$acc,
    rr$learners[[1]]$best_valid_scores$acc
  )
  expect_equal(
    rr$score(msr("internal_valid_score", select = "acc"))$acc,
    rr$learners[[1]]$internal_valid_scores$acc
  )
})

test_that("best valid scores are stored in the state without storing models", {
  task = tsk("iris")
  learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, iter = 5)
  rr = resample(task, learner, rsmp("holdout"), store_models = FALSE)
  expect_equal(
    rr$score(msr("best_valid_score", select = "acc"))$acc,
    rr$learners[[1]]$state$best_valid_scores$acc
  )
  expect_number(rr$learners[[1]]$state$best_valid_scores$acc)
})

test_that("no best valid scores without validation", {
  task = tsk("iris")
  learner = lrn("classif.debug")$train(task)
  expect_null(learner$best_valid_scores)
  expect_null(learner$internal_valid_scores)
})

test_that("extractors without the `which` argument are still supported", {
  # learners implemented before `which` was introduced must keep working and report no best scores
  LearnerLegacyValid = R6Class(
    "LearnerLegacyValid",
    inherit = LearnerClassifDebug,
    private = list(
      .extract_internal_valid_scores = function() {
        list(acc = 0.5)
      }
    )
  )

  task = tsk("iris")
  learner = LearnerLegacyValid$new()
  learner$id = "classif.legacy_valid"
  learner$validate = 0.2

  rr = resample(task, learner, rsmp("holdout"))
  expect_equal(rr$learners[[1]]$state$internal_valid_scores, list(acc = 0.5))
  expect_null(rr$learners[[1]]$state$best_valid_scores)
  expect_equal(rr$score(msr("internal_valid_score", select = "acc"))$acc, 0.5)
  expect_equal(rr$score(msr("best_valid_score", select = "acc"))$acc, NA_real_)
})
