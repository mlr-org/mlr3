data = iris
data$uri = 1:150

task = TaskClassif$new("iris", data, target = "Species")
task$set_col_roles("uri", "uri")

task$data(cols = "uri")
