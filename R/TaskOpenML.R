#' @title OpenML Task
#'
#' @name mlr_tasks_openml
#' @format [R6::R6Class] inheriting from [TaskClassif] or [TaskRegr].
#' @include mlr_tasks.R
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("openml", 13)
#' tsk("openml", 13)
#' ```
#'
#' @description
#' A Task loaded from OpenML.
#'
#' The data can either be identified with `data_id` (first default parameter), or with `task_id`,
#' which gets the data of the referenced (OpenML) task.
#'
#' @template seealso_task
NULL

load_task_openml = function(task_id, data_id, id) {
  require_namespaces(c("jsonlite", "farff"))
  if (missing(data_id) == missing(task_id)) {
    stop("Exactly one of data_id and task_id must be given.")
  }
  target = NULL
  tasktype = NULL
  if (missing(data_id)) {
    assert_int(task_id)
    j = withCallingHandlers(jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/task/%i", task_id), simplifyVector = FALSE),
      error = function(e) {
        if (test_string(e$message) && grepl("error 500", e$message)) {
          stopf("Task %s not found", task_id)
        }
      })
    names(j$task$input) = map(j$task$input, "name")
    data_id = as.numeric(j$task$input$source_data$data_set$data_set_id)
    if (!test_int(data_id)) {
      stopf("Got malformed reply for OpenML Task %s", task_id)
    }
    target = j$task$input$source_data$data_set$target_feature
    tasktype = switch(j$task$task_type_id,
      `1` = "classification",
      `2` = "regression",
      {
        warningf("Cannot map to Task type %s yet, going by type of first target variable.", j$tasktype$name)
        NULL
      })
  }
  j = withCallingHandlers(jsonlite::fromJSON(sprintf("https://www.openml.org/api/v1/json/data/%i", data_id)),
    error = function(e) {
      if (test_string(e$message) && grepl("error 500", e$message)) {
        stopf("Data %s not found", data_id)
      }
    })$data_set_description
  fn = tempfile()
  on.exit(try(file.remove(fn), silent = TRUE))
  utils::download.file(j$url, fn)
  content = farff::readARFF(fn)
  target = target %??% j$default_target_attribute
  if (length(target) > 1) {
    warningf("Can only work with 1 target right now. Dropping other targets (%s)", str_collapse(target[-1]))
    target = target[1]
  }
  tasktype = tasktype %??% if (is.numeric(content[[target]])) "regression" else "classification"

  target = match(target, colnames(content))
  colnames(content) = make.names(colnames(content), unique = TRUE)
  target = colnames(content)[target]

  b = as_data_backend(content)
  if (missing(id)) {
    id = paste0("openml_", j$name)
  }
  task = switch(tasktype, classification = TaskClassif, regression = TaskRegr)$new(id, b, target = target)
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("openml", load_task_openml)
