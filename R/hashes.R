# same as ids() but for hashes
hashes = function(x) {
  vcapply(x, "[[", "hash", use.names = FALSE)
}
