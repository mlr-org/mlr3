test_that("learner methods", {
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
})

test_that("NULL", {
  expect_equal(marshal_model(NULL), NULL)
  expect_equal(unmarshal_model(NULL), NULL)
})

test_that("default method just changes class", {
  x = 1
  xm = marshal_model(x)
  expect_equal(class(xm), c("numeric_marshalled", "marshalled"))
  expect_equal(x, unmarshal_model(xm))
})

test_that("marshalling a marshalled object does nothing", {
  x = 1
  xm = marshal_model(x)
  expect_equal(marshal_model(xm), xm)
})

test_that("unmarshalling a unmarshalled object does nothing", {
  x = 1
  xm = marshal_model(x)
  expect_equal(unmarshal_model(xm), x)
  expect_equal(unmarshal_model(unmarshal_model(xm)), x)
})
