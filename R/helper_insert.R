insert = function(x, y) {
  UseMethod("insert")
}

insert.list = function(x, y) {
  x[names(y)] = y
  x
}

insert.environment = function(x, y) {
  for (nn in names(y))
    assign(nn, y[[nn]], envir = x)
  x
}

insert.data.table = function(x, y) {
  if (ncol(x) > 0L) {
    ..y = y
    x[, names(..y) := ..y][]
  } else { # null data.table, we cannot assign with `:=`
    as.data.table(y)
  }
}

remove = function(x, nn) {
  UseMethod("remove")
}

remove.list = function(x, nn) {
  x[intersect(nn, names(x))] = NULL
  x
}

remove.environment = function(x, nn) {
  rm(list = intersect(nn, names(x)), envir = x)
  x
}

remove.data.table = function(x, nn) {
  nn = intersect(nn, names(x))
  if (length(nn))
    x[, (nn) := NULL]
  x
}
