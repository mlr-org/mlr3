#' @title Convert to BenchmarkResult
#'
#' @description
#' Convert object to a [BenchmarkResult].
#'
#' @inheritParams as_task
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
  private = get_private(x)
  rdata = private$.data$clone(deep = TRUE)
  if (!is.null(private$.view)) {
    rdata$data$fact = rdata$data$fact[list(private$.view), on = "uhash", nomatch = NULL]
    rdata$sweep()
  }
  BenchmarkResult$new(rdata)
}
