#' @title Calculate the Hash for a ResampleResult
#'
#' @description
#' Exported for add-on packages.
#' Calculates the hash of the [ResampleResult] given a [Task], a [Learner] and a [Resampling].
#'
#' @param task :: [Task].
#' @param learner :: [Learner].
#' @param resampling :: [Resampling].
#'
#' @return (`character(1)`) hash value.
#' @keywords internal
#' @export
hash_resample_result = function(task, learner, resampling) {
  hash(task$hash, learner$hash, resampling$hash)
}
