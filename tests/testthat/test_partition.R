test_that("partition two way split", {
  task = tsk("pima")
  li = partition(task, ratio = 0.66)
  expect_list(li, len = 3L)
  expect_names(names(li), identical.to = c("train", "test", "validation"))
  expect_equal(length(li$train), 507)
  expect_equal(length(li$test), 261)
  expect_equal(length(li$validation), 0)

  expect_disjunct(li$train, li$test)
  expect_disjunct(li$train, li$validation)
})

test_that("partition three way split", {
  task = tsk("pima")
  li = partition(task, ratio = c(0.66, 0.14))
  expect_list(li, len = 3L)
  expect_names(names(li), identical.to = c("train", "test", "validation"))
  expect_equal(length(li$train), 507)
  expect_equal(length(li$test), 107)
  expect_equal(length(li$validation), 154)

  expect_disjunct(li$train, li$test)
  expect_disjunct(li$train, li$validation)
})
