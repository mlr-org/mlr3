context("Task")

test_that("Task duplicates rows", {
  task = mlr_tasks$get("iris")
  data = task$data(c(1L, 1L))
  expect_data_table(data, nrow = 2L, any.missing = FALSE)
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
