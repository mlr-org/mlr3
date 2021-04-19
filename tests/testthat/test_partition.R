test_that("partition TaskClassif", {
  task = tsk("pima")
  li = partition(task)
  expect_list(li, len = 2L)
  expect_names(names(li), identical.to = c("train", "test"))

  ratio = function(x) {
    tab = table(x)
    tab[1] / tab[2]
  }

  r = ratio(task$truth())
  expect_true(abs(ratio(task$truth(li$train)) - r) < 0.05)
  expect_true(abs(ratio(task$truth(li$test)) - r) < 0.05)
})

test_that("partition TaskRegr", {
  task = tsk("boston_housing")
  li = partition(task)
  expect_list(li, len = 2L)
  expect_names(names(li), identical.to = c("train", "test"))
})
