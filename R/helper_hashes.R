hashes = function(x) {
  map_chr(unname(x), "hash")
}

phashes = function(x) {
  map_chr(unname(x), "phash")
}

#' @description
#' Calculate task hashes of resampling iterations.
#'
#' @param task ([Task]).
#' @param resampling ([Resampling]).
#'
#' @return (`character()`).
#' @noRd
task_hashes = function(task, resampling) {
  row_roles = get_private(task)$.row_roles
  map_chr(seq_len(resampling$iters), function(i) {
    train_set = resampling$train_set(i)
    row_roles$use = train_set
    calculate_hash(class(task), task$id, task$backend$hash, task$col_info, row_roles, task$col_roles,
      task$properties)
  })
}
