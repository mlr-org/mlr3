# download data from https://www.kaggle.com/datasets/camnugent/california-housing-prices
root = rprojroot::find_package_root_file()
data = data.table::fread("housing.csv")
data[, ocean_proximity := as.factor(ocean_proximity)]
saveRDS(data, file = file.path(root, "inst", "extdata", "california_housing.rds"), version = 2L)
