str_indent = function(str, initial, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}

str_r6_interface = function(x) {
  categorize = function(name) {
    if (bindingIsActive(name, x))
      return("active_binding")
    if (is.function(x[[name]]))
      return("method")
    return("slot")
  }

  public = setdiff(ls(x), c("initialize", "print", "format"))
  cats = split(public, map_chr(public, categorize))
  cats$method = paste0(cats$method, "()")
  sort(unlist(cats, use.names = FALSE))
}
