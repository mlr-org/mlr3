context("Task")

test_that("Task duplicates rows", {
  task = mlr_tasks$get("iris")
  data = task$data(c(1L, 1L))
  expect_data_table(data, nrow = 2L, any.missing = FALSE)
})

test_that("Task rbind", {
  task = mlr_tasks$get("iris")
  data = iris[1:10, ]
  task$rbind(iris[1:10, ])
  expect_equal(task$nrow, 160)
})

test_that("Task cbind", {
  task = mlr_tasks$get("iris")
  data = data.frame(..row_id = task$row_ids(), foo = 150:1)
  task$cbind(data)
  expect_names(task$feature_names, must.include = "foo")
})

test_that("Rows return ordered", {
  x = load_dataset("nhtemp", "datasets", TRUE)
  data = as.data.frame(x)
  data$x = as.numeric(data$x)
  data$t = as.integer(time(x))
  data = data[rev(seq_row(data)), ]
  rownames(data) = NULL
  b = BackendDataTable$new(data)
  task = TaskRegr$new(id = "nhtemp", b, target = "x")

  x = task$data()
  expect_true(is.unsorted(x$t))

  task$order = "t"
  x = task$data()
  expect_integer(x$t, sorted = TRUE, any.missing = FALSE)

  x = task$data(rows = sample(nrow(data), 50))
  expect_integer(x$t, sorted = TRUE, any.missing = FALSE)
})

test_that("Rows return ordered with multiple order cols", {
  task = mlr_tasks$get("iris")

  x = task$data()
  expect_true(is.unsorted(x$Petal.Length))

  task$order = c("Petal.Length", "Petal.Width")

  x = task$data()
  expect_numeric(x$Petal.Length, sorted = TRUE, any.missing = FALSE)

  expect_true(x[, is.unsorted(Petal.Width)])
  expect_true(all(x[, is.unsorted(Petal.Width), by = Petal.Width]$V1 == FALSE))
})


test_that("cbind/rbind works", {
  task = mlr_tasks$get("iris")
  data = data.table(..row_id = 1:150, foo = 150:1)

  task$cbind(data)
  expect_task(task)
  expect_set_equal(c(task$feature_names, task$target_names), c(names(iris), "foo"))
  expect_data_table(task$data(), ncol = 6, any.missing = FALSE)

  task$rbind(cbind(data.table(..row_id = 201:210, foo = 99L), iris[1:10, ]))
  expect_task(task)
  expect_set_equal(task$row_ids(), c(1:150, 201:210))
  expect_data_table(task$data(), ncol = 6, nrow = 160, any.missing = FALSE)
})
