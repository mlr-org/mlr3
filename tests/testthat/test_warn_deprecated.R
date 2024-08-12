

test_that("warn_deprecated works as expected", {

  oldopts = options(mlr3.warn_deprecated = TRUE)
  expect_warning(warn_deprecated("test"), "^test is deprecated and will be removed in the future\\.$")
  expect_warning(warn_deprecated("test"), NA) # no second warning

  oldopts = options(mlr3.warn_deprecated = FALSE)
  expect_warning(warn_deprecated("test2"), NA) # no warning when options disallow it

  options(oldopts)
})

test_that("deprecated_binding works as expected", {

  oldopts = options(mlr3.warn_deprecated = TRUE)
  MyClass = R6::R6Class("MyClass", public = list(val = 1),
    active = list(
      foo = deprecated_binding("MyClass$foo", "bar"),
      foo2 = deprecated_binding("MyClass$foo2", self$val)
    ),
  )
  mco = MyClass$new()
  expect_warning({fooval = mco$foo}, "^MyClass\\$foo is deprecated and will be removed in the future\\.$")
  expect_equal(fooval, "bar")
  expect_warning({fooval = mco$foo}, NA) # no second warning
  expect_equal(fooval, "bar")

  oldopts = options(mlr3.warn_deprecated = FALSE)
  expect_warning({foo2val = mco$foo2}, NA) # no warning when options disallow it
  expect_equal(foo2val, 1)
  mco$val = 2
  expect_equal(mco$foo2, 2)

  options(oldopts)
})
