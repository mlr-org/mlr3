expect_hash_changes = function(x) {
  id_before = x$id
  hash_before = x$hash

  x$id = "foo"
  expect_false(identical(x$id, id_before))
  if (!is.na(hash_before)) {
    expect_false(identical(x$hash, hash_before))
  }
  x$id = id_before
  expect_identical(x$id, id_before)
  expect_identical(x$hash, hash_before)
  if ("use_weights" %in% names(x)) {
    use_weights_before = x$use_weights
    x$use_weights = "ignore"
    expect_false(identical(x$use_weights, use_weights_before))
    if (!is.na(hash_before)) {
      expect_false(identical(x$hash, hash_before))
    }
    x$use_weights = use_weights_before
    expect_identical(x$use_weights, use_weights_before)
    expect_identical(x$hash, hash_before)
  }
}

test_that("task$hash", {
  x = tsk("iris")
  expect_hash_changes(x)
})

test_that("learner$hash", {
  x = lrn("classif.rpart")
  expect_hash_changes(x)
})

test_that("measure$hash", {
  x = msr("classif.ce")
  expect_hash_changes(x)
})

test_that("resampling$hash", {
  x = rsmp("cv")
  expect_hash_changes(x)
})
