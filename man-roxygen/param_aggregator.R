#' @param aggregator (`function()`)\cr
#'   Function to aggregate over multiple iterations. The role of this function depends on
#'   the value of field `"average"`:
#'
#'   * `"macro"`: A numeric vector of scores (one per iteration) is passed.
#'     The aggregate function defaults to [mean()] in this case.
#'   * `"micro"`: The `aggregator` function is not used.
#'     Instead, predictions from multiple iterations are first combined and then
#'     scored in one go.
#'   * `"custom"`: A [ResampleResult] is passed to the aggregate function.
