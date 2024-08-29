#' @section Parallelization:
#'
#' This function can be parallelized with the \CRANpkg{future} package.
#' One job is one resampling iteration, and all jobs are send to an apply function
#' from \CRANpkg{future.apply} in a single batch.
#' To select a parallel backend, use [future::plan()].
#' More on parallelization can be found in the book:
#' \url{https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html}
