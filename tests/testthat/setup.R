old_threshold = log$threshold
old_plan = future::plan()
log$set_threshold("warn")
future::plan("sequential")
