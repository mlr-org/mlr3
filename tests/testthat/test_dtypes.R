context("Additional data types")

test_that("data.type: imagepath", {
  x = c("foo/bar/1.jpg", "/foo/bar/1.jpg", "~/bar/1.jpg", "1.jpg",
    "//foo.png", "a-b_c.png")
  imp = as.imagepath(x)
  expect_is(imp, "imagepath")
  expect_identical(as.character(x), x)

  expect_error(as.imagepath("foo?bar"))
  expect_error(as.imagepath("foo/bar"))
  expect_error(as.imagepath("foobar"))
})

test_that("data.type: imagepath in task", {
  imps = as.imagepath(c("foo.jpg", "bar.jpg"))
  dt = data.table(ims = imps, cl = factor(0:1))
  t = TaskClassif$new("imtsk", dt, target = "cl")
  expect_task_classif(t)
  expect_true(t$feature_types$type == "imagepath")
  dt2 = t$data()
  expect_equal(dt2$ims, imps)
  expect_output(print(t), "- imp \\(1\\): ims")
})
