test_that("HotstartStack works with forward target learner and increased hotstart parameter", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(2, 1))
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_2$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward target learner when hotstart learner is different", {
  task = tsk("pima")
  learner_1 = lrn("classif.rpart")
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), NA_real_)
  expect_data_table(hot$stack, ncols = 3)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))
})

test_that("HotstartStack works with forward target learner when cost of hotstarting is equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 1))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when hotstart values of hotstart learners are lower and higher than value of forward target learner", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, NA_real_))
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when forward hotstart and target learner are equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward target learner when hotstart values are only higher", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, NA_real_))
  expect_data_table(hot$stack, ncols = 3)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward target learner when hotstart learners are different and values are lower, equal and higher", {
  task = tsk("pima")
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
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when forward target learner is unable to hotstart", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.rpart")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, NA_real_))
  expect_null(get_private(hot)$.start_learner(learner, task$hash))
})

test_that("HotstartStack works when stack is empty", {
  task = tsk("pima")
  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStack$new()

  expect_equal(hot$start_cost(learner, task$hash), numeric(0))
  expect_null(get_private(hot)$.start_learner(learner, task$hash))
})

test_that("HotstartStack works with backward target learner and decreased hotstart parameter", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), 0)
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with backward target learner when cost of hotstarting is equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 4)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 4)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, 0))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when hotstart values of hotstart learners are lower and higher than value of backward target learner", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 3)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 1)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, NA_real_))
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when backward hotstart and target learner are equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})


test_that("HotstartStack works with backward target learner when hotstart values are only lower", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), NA_real_)
  expect_data_table(hot$stack, ncols = 3)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with backward target learner when hotstart learners are different and values are lower, equal and higher", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStack$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, -1, 0, NA_real_))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward/backward target learner and increased hotstart parameter", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 0))
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_2$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward/backward target learner when cost of hotstarting is equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 1))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when hotstart values of hotstart learners are lower and higher than value of forward/backward target learner", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 0))
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_2$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works when forward/backward hotstart and target learner are equal", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward/backward target learner when hotstart values are only higher", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, 0))
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward/backward target learner when hotstart values are only lower", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), 1)
  expect_data_table(hot$stack, ncols = 3)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state, learner_1$state)
  expect_data_table(hot$stack, ncols = 3)
})

test_that("HotstartStack works with forward/backward target learner when hotstart learners are different and values are lower, equal and higher", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStack$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(1, -1, 0, NA_real_))
  expect_data_table(hot$stack, ncols = 3)
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

test_that("copying state of learner works", {
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

test_that("HotstartStack works without hotstart parameter", {
  task = tsk("pima")
  start_learner = lrn("classif.rpart")
  start_learner$train(task)

  target_learner = lrn("classif.rpart")
  hot = HotstartStack$new(start_learner)
  target_learner$hotstart_stack = hot

  expect_equal(hot$start_cost(target_learner, task$hash), NA_real_)
  expect_null(get_private(target_learner$hotstart_stack)$.start_learner(target_learner, task$hash))
})
