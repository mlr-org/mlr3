root = rprojroot::find_package_root_file()
data = setDT(AmesHousing::make_ames())
saveRDS(data, file = file.path(root, "inst", "extdata", "ames_housing.rds"), version = 2L)
