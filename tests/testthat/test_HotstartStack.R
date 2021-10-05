test_that("HotStartStack adapt forward works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotStartStack$new(list(learner_1, learner_2))

  expect_equal(hot$adaption_cost(learner, task$hash), c(2, 1))
  expect_equal(hot$adaption_learner(learner, task$hash), learner_2)

   # no adaptable learner
  learner_1 = lrn("classif.rpart")
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotStartStack$new(list(learner_1))

  expect_equal(hot$adaption_cost(learner, task$hash), NA_real_)
  expect_null(hot$adaption_learner(learner, task$hash))

  # equal cost
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotStartStack$new(list(learner_1, learner_2))

  expect_equal(hot$adaption_cost(learner, task$hash), c(1, 1))
  expect_equal(hot$adaption_learner(learner, task$hash), learner_1)

  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  hot = HotStartStack$new(list(learner_1, learner_2))

  expect_equal(hot$adaption_cost(learner, task$hash), c(1, NA_real_))
  expect_equal(hot$adaption_learner(learner, task$hash), learner_1)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotStartStack$new(list(learner_1))  

  expect_equal(hot$adaption_cost(learner, task$hash), NA_real_)
  expect_null(hot$adaption_learner(learner, task$hash))

  # only higher
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotStartStack$new(list(learner_1, learner_2))

  expect_equal(hot$adaption_cost(learner, task$hash), c(NA_real_, NA_real_))
  expect_null(hot$adaption_learner(learner, task$hash))
})

test_that("HotStartStack adapt backwards works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotStartStack$new(list(learner_1, learner_2))

  expect_equal(hot$adaption_cost(learner, task$hash), c(0, 0))
  expect_equal(hot$adaption_learner(learner, task$hash), learner_1)

  # only lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotStartStack$new(list(learner_1))

  expect_equal(hot$adaption_cost(learner, task$hash), NA_real_)
  expect_null(hot$adaption_learner(learner, task$hash))
})

test_that("HotStartStack adapt forward and backwards works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotStartStack$new(list(learner_1, learner_2))

  expect_equal(hot$adaption_cost(learner, task$hash), c(1, 0))
  expect_equal(hot$adaption_learner(learner, task$hash), learner_2)
})

test_that("HotStartStack add method works", {
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  hot = HotStartStack$new(list(learner_1))
  
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)
  hot$add(list(learner_2))

  expect_data_table(hot$stack, nrows = 2, ncols = 3)
  expect_equal(hot$stack$start_learner[[1]], learner_1)
  expect_equal(hot$stack$start_learner[[2]], learner_2)
})

test_that("HotStartStack with many learners works", {
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)
  learner_3 = lrn("classif.rpart")
  learner_3$train(task)

  learners = c(
    replicate(3000, learner_1$clone()),
    replicate(3000, learner_2$clone()),
    replicate(3000, learner_3$clone())
  )

  learner = lrn("classif.debug", iter = 2)
  hot = HotStartStack$new(learners)

  expect_numeric(hot$adaption_cost(learner, task$hash), len = 9000)
  expect_equal(hot$adaption_learner(learner, task$hash), learner_1))
})
