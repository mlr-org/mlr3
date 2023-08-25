`[[.R6` = function(x, i, ...) {
  if (exists(i, envir = x, inherits = FALSE))
    return(get(i, envir = x))
  stop("R6 class ", paste0(class(x), collapse = "/") ," does not have slot '", i, "'!")
}

`$.R6` = function(x, name) {
  if (exists(name, envir = x, inherits = FALSE))
    return(get(name, envir = x))
  stop("R6 class ", paste0(class(x), collapse = "/") ," does not have slot '", name, "'!")
}
