    add_backend = function(backend, row.role = "validation") {
      b = self$backend[[1L]]
      assert_names(backend$colnames, subset.of = b$colnames, must.include = b$primary_key)

      rn = backend$rownames
      for (b in self$backend) {
        clashes = b$data(rn, b$primary_key)
        if (nrow(clashes))
          stopf("Cannot add new backend: name clashes with primary_key id: %s", stri_peek(clashes[[1L]]))
      }

      self$backend[[length(self$backend) + 1L]] = backend
      self$row_info = rbind(self$row_info, data.table(id = rn, role = row.role))
      setkeyv(self$row_info, "id")

      invisible(self)
    }

# test_that("Task Construction with multiple backends", {
#   b = BackendDataTable$new(iris)
#   task = TaskClassif$new(id = "foo", b, target = "Species")
#   newdata = iris[1:10, ]
#   newdata$..row_id = 151:160
#   b = BackendDataTable$new(data = newdata, primary = "..row_id")
#   task$add_backend(b)

#   expect_equal(task$nrow, 150)
#   expect_data_table(task$data(), nrow = 150, ncol = 5)

#   task = TaskClassif$new(id = "foo", b, target = "Species")
#   newdata = iris[1:10, ]
#   newdata$..row_id = 151:160
#   b = BackendDataTable$new(data = newdata, primary = "..row_id")
#   task$add_backend(b, row.role = "training")

#   expect_equal(task$nrow, 160)
#   expect_data_table(task$data(), nrow = 160, ncol = 5)
# })
