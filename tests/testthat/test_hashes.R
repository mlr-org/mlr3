context("hashes")

expect_hash_changes = function(x) {
  id_before = x$id
  hash_before = x$hash

  x$id = "foo"
  expect_false(identical(x$id, id_before))
  expect_false(identical(x$hash, hash_before))
  x$id = id_before
  expect_true(identical(x$id, id_before))
  expect_true(identical(x$hash, hash_before))
}

test_that("task$hash", {
  x = mlr_tasks$get("iris")
  expect_hash_changes(x)
}
)

test_that("learner$hash", {
  x = mlr_learners$get("classif.rpart")
  expect_hash_changes(x)
}
)

test_that("measure$hash", {
  x = mlr_measures$get("classif.ce")
  expect_hash_changes(x)
}
)

test_that("resampling$hash", {
  x = mlr_resamplings$get("cv")
  expect_hash_changes(x)
}
)
