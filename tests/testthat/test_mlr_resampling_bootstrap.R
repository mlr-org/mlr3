test_that("bootstrap has duplicated ids", {
  r = rsmp("bootstrap")
  expect_identical(r$duplicated_ids, TRUE)
})

test_that("stratification", {
  data = data.table(y = factor(rep(letters[1:2], times = c(90, 10))), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("bootstrap", ratio = 1, repeats = 3)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 90)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 10)
  }
})

test_that("grouping", {
  r = rsmp("bootstrap", ratio = 1, repeats = 3)
  expect_grouping_works(r)
})

test_that("#518", {
  with_seed(36, {
    test_df = data.frame(feat1 = runif(3000),
      feat2 = runif(3000),
      target = runif(3000),
      group = sample(paste0("g", 1:100), 3000, replace = TRUE)
    )
    task = TaskRegr$new(id = "test", backend = test_df, target = "target")
    task$col_roles$group = "group"
    task$col_roles$feature = c("feat1", "feat2")
    rb = rsmp("bootstrap", repeats = 10, ratio = 1)
    rb$instantiate(task)
  })

  min.len = if (getRversion() >= "3.6.0") 3001L else NULL
  expect_integer(rb$train_set(2), min.len = min.len)
})
