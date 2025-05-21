test_that("assert_empty_ellipsis works", {
  expect_error(assert_empty_ellipsis(1), "Received 1 unnamed argument")
  expect_error(assert_empty_ellipsis(1, 2), "Received 2 unnamed argument")
  expect_error(assert_empty_ellipsis(a = 1), "that were unused: a")
  expect_error(assert_empty_ellipsis(a = 1, b = 2), "that were unused: a, b")
  expect_error(assert_empty_ellipsis(a = 1, b = 1, 2), "1 unnamed, as well as named arguments a, b")
  expect_null(assert_empty_ellipsis())
})
