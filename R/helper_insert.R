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
  x[, names(y) := y][]
}

remove = function(x, nn) {
  UseMethod("remove")
}

remove.list = function(x, nn) {
  nn = nn[hasName(x, nn)]
  x[nn] = NULL
  x
}

remove.enviroment = function(x, nn) {
  nn = nn[hasName(x, nn)]
  rm(x, list = nn)
  x
}

remove.data.table = function(x, nn) {
  nn = intersect(nn, names(x))
  if (length(nn))
    x[, (nn) := NULL]
  x
}
