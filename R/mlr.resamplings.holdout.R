#' @include Resampling.R
#' @include mlr_resamplings.subsampling.R
local({
  rr = ResamplingSubsampling$new()
  rr$id = "holdout"
  rr$repeats = 1L
  mlr_resamplings$add(rr)
})
