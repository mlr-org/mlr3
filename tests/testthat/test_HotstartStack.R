test_that("HotstartStack hotstart forward works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(2, 1))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_2$state)

   # no adaptable learner
  learner_1 = lrn("classif.rpart")
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), NA_real_)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))

  # equal cost
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 1))
  
  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, NA_real_))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStack$new(list(learner_1))  

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # only higher
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, NA_real_))
  expect_null(get_private(hot)$.start_learner(learner, task$hash))

  # mixed
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner 
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(1, -1, NA_real_, NA_real_))
})

test_that("HotstartStack hotstart backwards works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), 0)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # equal cost
  learner_1 = lrn("classif.debug", iter = 4)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 4)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, 0))

  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 3)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 1)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, NA_real_))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotstartStack$new(list(learner_1))  

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # only lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), NA_real_)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))

  # mixed
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner 
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "adapt_forward"] = "adapt_backward"
  hot = HotstartStack$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, -1, 0, NA_real_))
})

test_that("HotstartStack hotstart forward and backwards works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 0))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_2$state)

  # equal cost
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 1))

  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 0))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_2$state)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1))  

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # only higher
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, 0))

  # only lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), 1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)

  # mixed
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner 
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "adapt_backward")
  hot = HotstartStack$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(1, -1, 0, NA_real_))
})

test_that("HotstartStack add method works", {
  task = tsk("pima")

  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  hot = HotstartStack$new(list(learner_1))
  
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)
  hot$add(list(learner_2))

  expect_data_table(hot$stack, nrows = 2, ncols = 3)
  expect_equal(hot$stack$start_learner[[1]], learner_1)
  expect_equal(hot$stack$start_learner[[2]], learner_2)
})

test_that("copy state works", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner_2 = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(learner_1)
  learner_2$hotstart_stack = hot

  start_learner = get_private(learner_2$hotstart_stack)$.start_learner(learner_2, task$hash)
  start_learner$param_set$values$iter = 2

  expect_equal(learner_1$param_set$values$iter, 1)
  expect_equal(start_learner$param_set$values$iter, 2)
})
