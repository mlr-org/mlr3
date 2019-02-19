expect_grouping_works = function(r) {
  data = insert_named(as.data.table(iris), list(grp = rep_len(letters[1:10], 150)))
  task = TaskClassif$new("iris-grp", as_data_backend(data), target = "Species")
  task$set_col_role("grp", "groups")

  r$instantiate(task)
  for (i in seq_len(r$iters)) {
    expect_integer(r$train_set(i), lower = 1L, upper = 150L, any.missing = FALSE)
    expect_integer(r$test_set(i), lower = 1L, upper = 150L, any.missing = FALSE)
    expect_true(length(intersect(data[r$train_set(i), grp], data[r$test_set(i), grp])) == 0L)
  }
}
