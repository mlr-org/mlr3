seq_row = function(x) {
  seq_len(nrow(x))
}

seq_col = function(x) {
  seq_len(ncol(x))
}

seq_len0 = function(n) {
  if (n >= 1L)
    seq(from = 0L, to = n - 1L)
  else
    integer(0L)
}

seq_along0 = function(x) {
  seq_len0(length(x))
}
