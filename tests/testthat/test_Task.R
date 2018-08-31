context("Task")

test_that("Rows return ordered", {
  data = as.data.frame(load_dataset("nhtemp", "datasets", TRUE))
  data$x = as.numeric(data$x)
  data$t = as.integer(time(ee$nhtemp))
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

# test_that("Rows return ordered with multiple order cols", {
#   data = iris
#   task = TaskClassif$new(id = "iris_sorted", data, target = "Species")

#   x = task$data()
#   expect_true(is.unsorted(x$Petal.Length))

#   task$order = c("Petal.Length", "Petal.Width")

#   x = task$data()
#   expect_numeric(x$Petal.Length, sorted = TRUE, any.missing = FALSE)

#   expect_true(x[, is.unsorted(Petal.Width)])
#   expect_true(all(x[, is.unsorted(Petal.Width), by = Petal.Width]$V1 == FALSE))
# })
