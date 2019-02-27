old_threshold = lg$threshold
old_plan = future::plan()
lg$set_threshold("warn")
future::plan("sequential")
