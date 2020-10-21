test_that("Feature columns can be reordered", {
  bh = load_dataset("BostonHousing", "mlbench")
  task = tsk("boston_housing")
  task$col_roles$feature = setdiff(names(bh), "medv")

  expect_equal(task$feature_names, setdiff(names(bh), "medv"))
  expect_equal(names(task$data(rows = 1)), c("medv", setdiff(names(bh), "medv")))

  task$col_roles$feature = shuffle(task$col_roles$feature)
  expect_equal(names(task$data(rows = 1)), c("medv", task$col_roles$feature))
})

test_that("Task duplicates rows", {
  task = tsk("iris")
  data = task$data(c(1L, 1L))
  expect_data_table(data, nrows = 2L, any.missing = FALSE)
})

test_that("Rows return ordered", {
  x = load_dataset("nhtemp", "datasets", TRUE)
  data = as.data.frame(x)
  data$x = as.numeric(data$x)
  data$t = as.integer(time(x))
  data = data[rev(seq_row(data)), ]
  rownames(data) = NULL
  b = as_data_backend(data)
  task = TaskRegr$new(id = "nhtemp", b, target = "x")

  x = task$data()
  expect_true(is.unsorted(x$t))

  task$col_roles$order = "t"
  x = task$data()
  expect_integer(x$t, sorted = TRUE, any.missing = FALSE)

  x = task$data(ordered = FALSE)
  expect_true(is.unsorted(x$t))

  x = task$data(rows = sample(nrow(data), 50))
  expect_integer(x$t, sorted = TRUE, any.missing = FALSE)
})

test_that("Rows return ordered with multiple order cols", {
  task = tsk("iris")

  x = task$data()
  expect_true(is.unsorted(x$Petal.Length))

  task$col_roles$order = c("Petal.Length", "Petal.Width")
  expect_equal(task$col_roles$order, c("Petal.Length", "Petal.Width"))

  x = task$data()
  expect_numeric(x$Petal.Length, sorted = TRUE, any.missing = FALSE)

  expect_true(x[, is.unsorted(Petal.Width)])
  expect_true(all(x[, is.unsorted(Petal.Width), by = Petal.Width]$V1 == FALSE))
})


test_that("Task rbind", {
  task = tsk("iris")
  # expect_error(task$rbind(task), "data.frame")
  data = iris[1:10, ]
  task$rbind(iris[1:10, ])
  expect_task(task)
  expect_equal(task$nrow, 160)

  task$rbind(iris[integer(), ])
  expect_equal(task$nrow, 160)

  # #185
  task = tsk("iris")
  task$select("Petal.Length")
  task$rbind(task$data())
  expect_set_equal(task$row_ids, 1:300)

  task$rbind(data.table())
  expect_equal(task$nrow, 300L)

  # #437
  task = tsk("zoo")
  data = task$data()
  data$foo = 101:1
  nt = task$clone()$rbind(data)
  expect_task(nt)
  expect_set_equal(nt$row_ids, 1:202)
  expect_equal(nt$row_names$row_name, c(task$row_names$row_name, rep(NA, 101)))
  expect_equal(nt$col_info[list("foo"), .N, nomatch = NULL], 0L)

  # #423
  task = tsk("iris")
  task$row_roles$use = 1:10
  task$row_roles$holdout = 11:150

  task$rbind(iris[sample(nrow(iris), 5), ])
  expect_set_equal(task$row_ids, c(1:10, 151:155))

  # 496
  data = iris
  data$blocks = sample(letters[1:2], nrow(iris), replace = TRUE)
  task = TaskClassif$new("iris", data, target = "Species")
  task$col_roles$feature = setdiff(task$col_roles$feature, "blocks")
  task$col_roles$group = "blocks"
  learner = lrn("classif.rpart")
  learner$train(task)
  expect_prediction(predict(learner, iris, predict_type = "<Prediction>"))
})

test_that("Task cbind", {
  task = tsk("iris")
  # expect_error(task$cbind(task), "data.frame")
  data = cbind(data.frame(foo = 150:1), data.frame(..row_id = task$row_ids))
  task$cbind(data)
  expect_task(task)
  expect_equal(task$ncol, 6L)
  expect_names(task$feature_names, must.include = "foo")

  data = data.frame(bar = runif(150))
  task$cbind(data)
  expect_task(task)
  expect_equal(task$ncol, 7L)
  expect_names(task$feature_names, must.include = "bar")

  data = data.frame(..row_id = task$row_ids)
  task$cbind(data)
  expect_equal(task$ncol, 7L)

  task$cbind(iris[, character()])
  expect_equal(task$ncol, 7L)

  task$cbind(data.table())
  expect_equal(task$ncol, 7L)

  y = task$data(cols = task$target_names, rows = shuffle(task$row_ids))
  task$cbind(y)
  expect_equal(task$ncol, 7L)
  expect_disjunct(task$feature_names, task$target_names)

  # cbind to subsetted task
  task = tsk("iris")$filter(1:120)
  backend = data.table(x = runif(120))
  task$cbind(backend)

  # cbind 0-row data (#461)
  task = tsk("iris")$filter(integer())
  task$cbind(data.frame(x = integer()))
  expect_set_equal(c(task$target_names, task$feature_names), c(names(iris), "x"))
})

test_that("cbind/rbind works", {
  task = tsk("iris")
  data = data.table(..row_id = 1:150, foo = 150:1)

  task$cbind(data)
  expect_task(task)
  expect_set_equal(c(task$feature_names, task$target_names), c(names(iris), "foo"))
  expect_data_table(task$data(), ncols = 6, any.missing = FALSE)

  task$rbind(cbind(data.table(..row_id = 201:210, foo = 99L), iris[1:10, ]))
  expect_task(task)
  expect_set_equal(task$row_ids, c(1:150, 201:210))
  expect_data_table(task$data(), ncols = 6, nrows = 160, any.missing = FALSE)

  # auto generated ids
  task = tsk("zoo")
  newdata = task$data(1)
  newdata$animal = "boy"
  task$rbind(newdata)
  expect_set_equal(task$row_ids, 1:102)
})

test_that("filter works", {
  task = tsk("iris")
  task$filter(1:100)
  expect_equal(task$nrow, 100L)

  task$filter(91:150)
  expect_equal(task$nrow, 10L)

  expect_equal(task$row_ids, 91:100)

  task$filter(91)
  expect_equal(task$nrow, 1L)
  expect_data_table(task$data(), nrows = 1L, any.missing = FALSE)
})

test_that("select works", {
  task = tsk("iris")
  task$select(setdiff(task$feature_names, "Sepal.Length"))
  expect_equal(task$ncol, 4L)

  expect_error(task$select(c("Sepal.Width", "foobar")))

  task$select("Sepal.Width")
  expect_equal(task$feature_names, "Sepal.Width")

  expect_error(task$select(1:4), "subset")
})

test_that("rename works", {
  task = tsk("iris")
  old = names(iris)
  new = paste0("xx_", old)
  task$rename(old, new)

  expect_set_equal(task$feature_names, setdiff(new, "xx_Species"))
  expect_equal(task$target_names, "xx_Species")
  expect_task_classif(task)
})

test_that("stratify works", {
  task = tsk("iris")
  expect_false("strata" %in% task$properties)
  expect_null(task$strata)

  task$col_roles$stratum = task$target_names
  expect_true("strata" %in% task$properties)
  tab = task$strata
  expect_data_table(tab, ncols = 2, nrows = 3)
  expect_list(tab$row_id, "integer")
})

test_that("groups/weights work", {
  b = as_data_backend(data.table(x = runif(20), y = runif(20), w = runif(20), g = sample(letters[1:2], 20, replace = TRUE)))
  task = TaskRegr$new("test", b, target = "y")
  task$set_row_role(16:20, character())

  expect_false("groups" %in% task$properties)
  expect_false("weights" %in% task$properties)
  expect_null(task$groups)
  expect_null(task$weights)

  task$col_roles$weight = "w"
  expect_subset("weights", task$properties)
  expect_data_table(task$weights, ncols = 2, nrows = 15)
  expect_numeric(task$weights$weight, any.missing = FALSE)

  task$col_roles$weight = character()
  expect_true("weights" %nin% task$properties)

  task$col_roles$group = "g"
  expect_subset("groups", task$properties)
  expect_data_table(task$groups, ncols = 2, nrows = 15)
  expect_subset(task$groups$group, c("a", "b"))

  task$col_roles$group = character()
  expect_true("groups" %nin% task$properties)

  expect_error({task$col_roles$weight = c("w", "g")}, "up to one")
})

test_that("ordered factors (#95)", {
  df = data.frame(
    x = c(1, 2, 3),
    y = factor(letters[1:3], levels = letters[1:3], ordered = TRUE),
    z = factor(c("M", "R", "R"), levels = c("M", "R"))
  )
  b = as_data_backend(df)
  task = TaskClassif$new(id = "id", backend = b, target = "z")
  expect_subset(c("numeric", "ordered", "factor"), task$col_info$type)
  expect_set_equal(task$col_info[id == "z", levels][[1L]], c("M", "R"))
  expect_set_equal(task$col_info[id == "y", levels][[1L]], letters[1:3])
})

test_that("as.data.table", {
  task = tsk("iris")
  expect_data_table(as.data.table(task), nrows = 150, ncols = 5)
})

test_that("extra factor levels are stored (#179)", {
  dt = data.table(
    x1 = factor(letters[1:5], levels = letters[5:1]),
    x2 = factor(letters[1:5], levels = letters),
    x3 = letters[1:5],
    target = 1:5)
  task = TaskRegr$new("extra_factor_levels", as_data_backend(dt), "target")
  expect_equal(task$levels("x2")$x2, letters)
})

test_that("task$droplevels works", {
  dt = data.table(
    x1 = factor(letters[1:3]),
    target = 1:3
  )
  task = TaskRegr$new("droplevels", as_data_backend(dt), "target")

  task$filter(1:2)
  expect_equal(task$nrow, 2L)
  expect_equal(task$levels("x1")$x1, letters[1:3])
  task$droplevels()
  expect_equal(task$levels("x1")$x1, letters[1:2])
})

test_that("task$missings() works", {
  task = tsk("pima")
  x = task$missings()
  y = map_int(task$data(), function(x) sum(is.na(x)))
  expect_equal(x, y[match(names(x), names(y))])
})

test_that("task$feature_types preserves key (#193)", {
  task = tsk("iris")$select(character())$cbind(iris[1:4])
  expect_data_table(task$feature_types, ncols = 2L, nrows = 4L, key = "id")
})

test_that("switch columns on and off (#301)", {
  task = tsk("iris")
  task$col_roles$feature = setdiff(task$col_roles$feature, "Sepal.Length")
  task$cbind(data.table(x = 1:150))
  task$col_roles$feature = union(task$col_roles$feature, "Sepal.Length")
  expect_data_table(task$data(), ncols = 6, nrows = 150, any.missing = FALSE)
})

test_that("row roles setters", {
  task = tsk("iris")

  expect_error({ task$row_roles$use = "foo" })
  expect_error({ task$row_roles$foo = 1L })

  task$row_roles$use = 1:20
  expect_equal(task$nrow, 20L)
})

test_that("col roles getters/setters", {
  task = tsk("iris")

  expect_error({ task$col_roles$feature = "foo" })

  # additional roles allowed (#558)
  task$col_roles$foo = "Species"

  task$col_roles$feature = setdiff(task$col_roles$feature, "Sepal.Length")
  expect_false("Sepal.Length" %in% task$feature_names)
})

test_that("Task$row_names", {
  task = tsk("mtcars")
  tab = task$row_names
  expect_data_table(tab, any.missing = FALSE, ncols = 2, nrows = task$nrow)
  expect_integer(tab$row_id, unique = TRUE)
  expect_character(tab$row_name)

  tab = task$filter(1:10)$row_names
  expect_data_table(tab, any.missing = FALSE, ncols = 2, nrows = task$nrow)
  expect_integer(tab$row_id, unique = TRUE)
  expect_character(tab$row_name)
})
