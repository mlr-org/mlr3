root = rprojroot::find_package_root_file()
data = data.table::fread("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", data.table = FALSE)
names(data) = c("type", "alcohol", "malic", "ash", "alcalinity", "magnesium", "phenols",
  "flavanoids", "nonflavanoids", "proanthocyanins", "color", "hue", "dilution", "proline")
data$type = factor(data$type, levels = 1:3)
saveRDS(data, file = file.path(root, "inst", "extdata", "wine.rds"), version = 2L)
