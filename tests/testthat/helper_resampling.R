expect_grouping_works = function(r) {
  data = insert_named(as.data.table(iris), list(grp = rep_len(letters[1:10], 150)))
  task = TaskClassif$new("iris-grp", as_data_backend(data), target = "Species")
  task$col_roles$group = "grp"

  r$instantiate(task)
  for (i in seq_len(r$iters)) {
    expect_integer(r$train_set(i), lower = 1L, upper = 150L, any.missing = FALSE)
    expect_integer(r$test_set(i), lower = 1L, upper = 150L, any.missing = FALSE)
    if (!inherits(r, "ResamplingInsample")) {
      expect_length(intersect(data[r$train_set(i), get("grp")], data[r$test_set(i), get("grp")]), 0L)
    }
  }
}
