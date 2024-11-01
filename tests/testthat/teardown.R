options(old_opts)
lg$set_threshold(old_threshold)
future::plan(old_plan)
file.remove("tests/testthat/Rplots.pdf")
