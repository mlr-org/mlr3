#' @include Resampling.R
ResamplingRepeatedCV = R6Class("ResamplingRepeatedCV", inherit = Resampling,
  public = list(
    id = "repeated.cv",
    repeats = 10L,
    folds = 10L,
    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      rcv = function(ids, folds, repeats) {
        n = length(ids)
        data = rbindlist(lapply(seq_len(self$repeats), function(i) {
          data.table(
            row_id = ids,
            rep = i,
            fold = shuffle(seq_len0(n) %% folds + 1L)
          )
        }))

        setkeyv(data, c("rep", "fold"))
      }

      assert_task(task)
      private$instance = rcv(task$row_ids(), asInt(self$folds, lower = 1L), asInt(self$repeats, lower = 1L))
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i) - 1L
      folds = as.integer(self$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = setdiff(seq_len(folds), fold))
      private$instance[ii, "row_id"][[1L]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i) - 1L
      folds = as.integer(self$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = fold)
      private$instance[ii, "row_id"][[1L]]
    }
  ),
  active = list(
    iters = function() {
      self$repeats * self$folds
    }
  )
)

mlr_resamplings$add(
  ResamplingRepeatedCV$new()
)
