The 'use' column in a task defines which observations are available when working with the task.
Here, working with the task can either mean training a model, or evaluating it via resampling schemes.
When resampling, 'use' rows are also just available observations and can be used for either the 'test' or 'train' set.


TODO:
* task$data() should check that row_ids is subset of task$row_ids and not backend$rownames
