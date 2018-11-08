# same as ids() but for hashes
hashes = function(x) {
  pluck_chr(unname(x), "hash")
}
