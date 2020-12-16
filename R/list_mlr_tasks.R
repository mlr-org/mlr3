#' @title List Tasks in mlr3
#' @description Lists all the predefined tasks in mlr3. Can be filtered by task types.
#' @param task (`character(1)`) \cr Task type to filter by.
#' @return (`data.table()`)
#' @examples
#' # list all tasks
#' list_mlr_tasks()
#'
#' # list regression only tasks
#' list_mlr_tasks("regr")
#'
#' # list classif tasks with twoclass property
#' list_mlr_tasks("classif")[twoclass == TRUE]
list_mlr_tasks = function(task = NULL) {
  tasks = mlr_tasks$mget(mlr_tasks$keys())
  properties = unique(unlist(mlr_reflections$task_properties))
  dt = mlr3misc::map_dtr(tasks, function(.x) {
    p = .x$properties

    if (!length(p)) p = "-"
    props = as.logical(match(properties, p, 0L))
    props[properties %nin% mlr_reflections$task_properties[[.x$task_type]]] = ""
    data.frame(matrix(c(.x$id, .x$task_type, props), nrow = 1))
  })
  colnames(dt) =  c("id", "task_type", properties)

  if (!is.null(task)) {
    dt[task_type == task,
       c("id", "task_type", mlr_reflections$task_properties[[task]]), with = FALSE]
  } else {
    dt
  }
}
