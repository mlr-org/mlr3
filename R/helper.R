#' @title Get 'private' part of an R6 Instance
#'
#' @param x ([R6::R6Class]).
#'
#' @return (`environment()`).
#' @noRd
get_private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

phashes = function(x) {
  map_chr(unname(x), "phash")
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key][]
}

translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}

allow_partial_matching = list(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)


# extract values from a single column of a data table
# tries to avoid the overhead of data.table for small tables
fget = function(tab, i, j, key) {
  if (nrow(tab) > 1000L) {
    tab[list(i), j, on = key, with = FALSE, nomatch = NULL][[1L]]
  } else {
    x = tab[[key]]
    if (is.character(x) && is.character(i)) {
      tab[[j]][x %chin% i]
    } else {
      tab[[j]][x %in% i]
    }
  }
}

allow_utf8_names = function() {
  isTRUE(getOption("mlr3.allow_utf8_names"))
}

get_featureless_learner = function(task_type) {
  if (!is.na(task_type)) {
    id = paste0(task_type, ".featureless")
    if (mlr_learners$has(id)) {
      return(mlr_learners$get(id))
    }
  }

  return(NULL)
}

set_encapsulation = function(learners, encapsulate) {
  assert_choice(encapsulate, c(NA_character_, "none", "evaluate", "callr"))

  if (!is.na(encapsulate)) {
    lapply(learners, function(learner) learner$encapsulate = c(train = encapsulate, predict = encapsulate))
    if (encapsulate %in% c("evaluate", "callr")) {
      task_type = unique(map_chr(learners, "task_type"))
      stopifnot(length(task_type) == 1L) # this should not be possible for benchmarks
      fb = get_featureless_learner(task_type)
      if (!is.null(fb)) {
        lapply(learners, function(learner) if (is.null(learner$fallback)) learner$fallback = fb$clone(TRUE))
      }
    }
  }
  learners
}
