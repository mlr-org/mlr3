# same as ids() but for hashes
hashes = function(x) {
  map_chr(unname(x), "hash")
}
