#' @section Progress Bars:
#' This function supports progress bars via the package \CRANpkg{progressr}.
#' Simply wrap the function call in [progressr::with_progress()] to enable them.
#' Alternatively, call [progressr::handlers()] with `global = TRUE` to enable progress bars
#' globally.
#' We recommend the \CRANpkg{progress} package as backend which can be enabled with
#' `progressr::handlers("progress")`.
