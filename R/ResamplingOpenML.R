#' @title OpenML Task Resampling
#'
#' @name mlr_resamplings_openml
#' @include Resampling.R
#'
#' @description
#' Splits data according to OpenML Task.
#'
#' @template section_dictionary_resampling
#'
#' @template seealso_resampling
#' @export
ResamplingOpenML = R6Class("ResamplingOpenML", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task_id) {
      require_namespaces("jsonlite")
      super$initialize(id = "openml", man = "mlr3::mlr_resamplings_openml")
      assert_int(task_id)
      j = withCallingHandlers(jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/task/%i", task_id), simplifyVector = FALSE),
        error = function(e) {
          if (test_string(e$message) && grepl("error 500", e$message)) {
            stopf("Task %s not found", task_id)
          }
        })
      names(j$task$input) = map(j$task$input, "name")
      splitsurl = j$task$input$estimation_procedure$estimation_procedure$data_splits_url
      on.exit(try(file.remove(fn), silent = TRUE))
      utils::download.file(splitsurl, fn)
      splits = as.data.table(farff::readARFF(fn))
      splits$rowid = splits$rowid + 1
      colnames(splits) = c("type", "rowid", "rep", "fold")  # 'repeat' is reserved
      folds = splits[, 1, by = c("rep", "fold")]
      getfold = function(fold, which) {
        repinfo = splits[, 1, by = c("rep", "fold")][fold, c("rep", "fold")]
        splits[get("type") == which &
                    get("rep") == repinfo$rep &
                    get("fold") == repinfo$fold, ]$rowid
      }
      train_sets = lapply(seq_row(folds), getfold, which = "TRAIN")
      test_sets = lapply(seq_row(folds), getfold, which = "TEST")
      self$task_hash = "xxx"
      self$task_nrow = max(splits$rowid)
      self$instance = list(train = train_sets, test = test_sets)
    }
  ),
  active = list(
    iters = function(rhs) {
      assert_ro_binding(rhs)
      length(self$instance$train)
    }
  ),

  private = list(
    .get_train = function(i) self$instance$train[[i]],
    .get_test = function(i) self$instance$test[[i]]
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("openml", ResamplingOpenML)
