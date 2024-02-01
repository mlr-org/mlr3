test_that("marshal works as expectected", {
  learner = lrn("classif.lily")
  task = tsk("iris")
  expect_error(learner_marshal(learner), "not been trained")
  expect_error(learner_unmarshal(learner), "not been trained")
  expect_error(learner_marshalled(learner), "not been trained")

  learner$train(task)
  expect_false(learner_marshalled(learner))
  learner$marshal()
  expect_true(learner_marshalled(learner))
  learner$unmarshal()
  expect_false(learner_marshalled(learner))

  # default does nothing
  expect_equal(marshal_model(1), 1)
  expect_equal(unmarshal_model(1), 1)
})
