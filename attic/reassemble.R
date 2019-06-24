library(R6)

obj = mlr3::LearnerClassifRpart$new()

cloned = obj$clone()
tmp = tempfile()
saveRDS(obj, file = tmp)
deserialized = readRDS(tmp)
reassembled = deserialized$reassemble()

pryr::object_size(obj)
pryr::object_size(cloned)
pryr::object_size(deserialized)
pryr::object_size(reassembled)
