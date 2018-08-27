context("Task")

test_that("Task Construction with multiple backends", {
  task = TaskClassif$new(id = "foo", iris, target = "Species")
  newdata = iris[1:10, ]
  newdata$..row_id = 151:160
  b = BackendDataTable$new(data = newdata, primary = "..row_id")
  task$add_backend(b)

  expect_equal(task$nrow, 150)
  expect_data_table(task$data(), nrow = 150, ncol = 5)

  task = TaskClassif$new(id = "foo", iris, target = "Species")
  newdata = iris[1:10, ]
  newdata$..row_id = 151:160
  b = BackendDataTable$new(data = newdata, primary = "..row_id")
  task$add_backend(b, row.role = "training")

  expect_equal(task$nrow, 160)
  expect_data_table(task$data(), nrow = 160, ncol = 5)
})

test_that("Rows return ordered", {
  ee = new.env()
  data("nhtemp", package = "datasets", envir = ee)
  data = as.data.frame(ee$nhtemp)
  data$x = as.numeric(data$x)
  data$t = as.integer(time(ee$nhtemp))
  data = data[rev(seq_row(data)), ]
  rownames(data) = NULL
  task = TaskRegr$new(id = "nhtemp", data, target = "x")

  x = task$data()
  expect_true(is.unsorted(x$t))

  task$order = "t"
  x = task$data()
  expect_integer(x$t, sorted = TRUE, any.missing = FALSE)

  x = task$data(rows = sample(nrow(data), 50))
  expect_integer(x$t, sorted = TRUE, any.missing = FALSE)
})

test_that("Rows return ordered with multiple order cols", {
  data = iris
  task = TaskClassif$new(id = "iris_sorted", data, target = "Species")

  x = task$data()
  expect_true(is.unsorted(x$Petal.Length))

  task$order = c("Petal.Length", "Petal.Width")

  x = task$data()
  expect_numeric(x$Petal.Length, sorted = TRUE, any.missing = FALSE)

  expect_true(x[, is.unsorted(Petal.Width)])
  expect_true(all(x[, is.unsorted(Petal.Width), by = Petal.Width]$V1 == FALSE))
})
