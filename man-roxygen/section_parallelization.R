#' @section Parallelization:
#'
#' This function can be parallelized with the \CRANpkg{future} or \CRANpkg{mirai} package.
#' One job is one resampling iteration.
#' All jobs are send to an apply function from \CRANpkg{future.apply} or `mirai::mirai_map()` in a single batch.
#' To select a parallel backend, use [future::plan()].
#' To use \CRANpkg{mirai}, call `mirai::daemons()` before calling this function.
#' More on parallelization can be found in the book:
#' \url{https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html}
