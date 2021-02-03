#' @title Convert to BenchmarkResult
#'
#' @description
#' Convert object to a [BenchmarkResult].
#'
#' @param x (`any`)\cr
#'  Object to convert on, e.g. a [ResampleResult].
#' @param ... (`any`)\cr
#'  Currently not used.
#'
#' @return ([BenchmarkResult]).
#' @export
as_benchmark_result = function(x, ...) {
  UseMethod("as_benchmark_result")
}


#' @rdname as_benchmark_result
#' @export
as_benchmark_result.BenchmarkResult = function(x, ...) { # nolint
  x
}

#' @rdname as_benchmark_result
#' @export
as_benchmark_result.ResampleResult = function(x, ...) { # nolint
  rdata = x$data$clone(deep = TRUE)
  if (!is.null(x$view)) {
    rdata$data$fact = rdata$data$fact[list(x$view), on = "uhash", nomatch = NULL]
    rdata$sweep()
  }
  BenchmarkResult$new(rdata)
}
