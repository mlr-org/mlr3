#' @title Get 'private' part of an R6 Instance
#'
#' @param x ([R6::R6Class]).
#'
#' @return (`environment()`).
#' @noRd
get_private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}

#' @title Sets the State in a Learner
#'
#' @param learner ([Learner]).
#' @param state (named `list()`).
#'
#' @return ([Learner]) with updated state.
#' @noRd
reassemble_learner = function(learner, state) {
  Map(function(l, s) {
        l = l$clone(deep = TRUE)
        l$state = s
        l
  }, l = learner, s = state)
}

#' @title Normalize List Column of R6 Objects
#'
#' @description
#' Given a table `tab` with list column `col` of R6 objects, walks over values of
#' `col` and replaces objects with their hash.
#' The objects are returned as as environment.
#'
#' @param tab (`data.table()`).
#' @param col (`character(1)`)\cr
#'   Column of `tab`.
#'
#' @return (`environment()`).
#' Environment of distinct extracted R6 objects alongside their hash and id.
#'
#' @noRd
normalize_tab = function(tab, col) {
  values = tab[[col]]
  hashes = hashes(values)
  idx = which(!duplicated(hashes))
  ee = list2env(set_names(values[idx], hashes[idx]), parent = emptyenv(), hash = TRUE)
  set(tab, j = col, value = hashes)
  ee
}

#' @title Revert Normalization of a Table
#'
#' @param bmr ([BenchmarkResult]).
#' @param data ([data.table()]).
#' @param reassemble_learner (logical(1)).
#'
#' @return (`data.table()`) with hashes replaced by their referenced objects.
#'
#' @noRd
denormalize_tab = function(bmr, data = bmr$data, reassemble_learners = FALSE, convert_predictions = FALSE, predict_sets = "test") {
  tab = copy(data)
  p = get_private(bmr)

  set(tab, j = "task", value = mget(tab$task, envir = p$.tasks, inherits = FALSE))
  set(tab, j = "learner", value = mget(tab$learner, envir = p$.learners, inherits = FALSE))
  set(tab, j = "resampling", value = mget(tab$resampling, envir = p$.resamplings, inherits = FALSE))

  if (reassemble_learners) {
    set(tab, j = "learner", value = reassemble_learner(tab$learner, tab$state))
  }

  if (convert_predictions) {
    set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets))
  }

  set(tab, j = "state", value = NULL)[]
}


#' @title Convert Environment to Table
#'
#' @description
#' Given an environment of R6 objects (as returned by [normalize_tab()]),
#' returns a `data.table` with three columns:
#' * `[obj_type]_hash`: Hash of object.
#' * `[obj_type]_id`: Id of object.
#' * `[obj_type]`: Object itself.
#'
#' @param ee (`environment()`).
#' @param obj_type (`character(1)`).
#'
#' @return (`data.table()`).
#' @noRd
env2tab = function(ee, obj_type) {
  setnames(data.table(
    names(ee),
    ids(ee),
    as.list(ee)
  ), sprintf(c("%s_hash", "%s_id", "%s"), obj_type))[]
}

#' @title Copies Environment of R6 Objects
#'
#' @description
#' Given an environment of R6 objects (as returned by [normalize_tab()]),
#' returns a new environment with cloned objects.
#'
#' @param ee (`environment()`).
#' @param deep (`logical(1)`).
#'
#' @return (`environment()`).
#' @noRd
copy_r6_dict = function(ee, clone = TRUE, deep = FALSE) {
  if (isTRUE(clone)) {
    new_env = list2env(eapply(ee, function(x) x$clone(deep = deep)), parent = emptyenv(), hash = TRUE)
  } else {
    new_env = new.env(parent = emptyenv(), hash = TRUE)
    for (name in ls(ee, all.names = TRUE)) {
      assign(name, value = get(name, envir = ee, inherits = FALSE), envir = new_env)
    }
  }
  new_env
}
