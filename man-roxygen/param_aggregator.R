#' @param aggregator (`function()` | `NULL`)\cr
#'   Function to aggregate over multiple iterations. The role of this function depends on
#'   the value of field `"average"`:
#'
#'   * `"macro"`: A numeric vector of scores (one per iteration) is passed.
#'     The aggregate function defaults to [mean()] in this case.
#'   * `"macro_weighted"`: A numeric vector of scores and a numeric vector of weights (one per iteration each) are
#'     passed as first and second argument.
#'     The aggregate function defaults to [weighted.mean()] in this case.
#'   * `"micro"`: The `aggregator` function is not used.
#'     Instead, predictions from multiple iterations are first combined and then
#'     scored in one go.
#'   * `"custom"`: A [ResampleResult] is passed to the aggregate function.
