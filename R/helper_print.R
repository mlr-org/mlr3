str_r6_interface = function(x) {
  categorize = function(name) {
    if (bindingIsActive(name, x))
      return("active_binding")
    if (is.function(x[[name]]))
      return("method")
    return("field")
  }

  public = setdiff(ls(x), c("initialize", "print", "format", "finalize"))
  cats = split(public, map_chr(public, categorize))
  cats$method = paste0(cats$method, "()")
  sort(unlist(cats, use.names = FALSE))
}
