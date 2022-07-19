# format_list_item = function(x, ...) {
#   UseMethod("format_list_item")
# }

catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}
