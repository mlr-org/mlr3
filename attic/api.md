# Base Objects

## Dictionary
A simple Hashmap/Dictionary/Register/you name it with lazy loading of objects.

##### Members
- `items` [`environment`]: Storage object.
- `contains` [`string`]: Class of stored objects (used for assertions, but can be dropped?)

##### Methods
- `ids`:
  Get all keys. This is sadly *not* an active binding because of a bug.
- `add(value)`:
  Stores object `value` in `items` using key `value$id`.
- `get(key)`:
  Retrieves object with key `key`.
- `mget(keys)`:
  Retrieves objects with keys `keys` as a named list.
- `remove(keys)`:
  Removes objects with keys `keys` from `items` (can be used in `unloadPackage` hook).

##### Bindings


## Backend
Abstraction for tabular data. Can transparently access data.frames and data bases.

##### Initialize
- `function(data, primaray.key)`

##### Clonable
- Nope.

##### Members
- `primary_key` [`string`]:
  Column name of the unique primary key
- `data`: [depending on backend]

##### Methods
- `data(rows, cols) -> data.table`:
  Returns data subsetd to specified rows (via row_id) and cols.
  Typically called internally by class Task.
- `head(n = 6) -> data.table`:
  Peek into the data, convenient to retrieve column class information.
- `levels(col)`:
  Get all distinct values of column `col`.

##### Bindings
- `colnames -> character()`
- `rownames -> character()`
- `nrow -> integer(1)`
- `ncol -> integer(1)`

##### Comments
- a BE is mainly considered immutable
- we implement the DT version first, everything else comes later
- dplyr backend as proof-of-concept available in git history.
- a MultipleBackends cbinding and rbinding multiple BEs seems very useful




## TaskAbstract
Abstract base class for Task

##### Initialize
- `function(id, backend)`

##### Clonable
- Yes.
- Backend is shallow ref copy, rest is deep copy

##### Members
- `id` [`string`]
- `backend` [`Backend`]
- `row_info` [`data.table`]:
  Table of row information: [id] | [role].
  Valid row roles: "use", "validation", "ignore", ...?
  - ignore: not touched / considered in any operation
  - validation: (possibly unlabeled) rows which are held back from all operations (just like ignore)
  - use: use for train, resample, tune, ....
- `col_info` [`data.table`]:
  Table of column infomation: [id] | [role] | [type].
  Valid col roles: "primary_key", "target", "feature", "ignore".
  Possibly more in the future (candidates: order, blocking, weights, observation_name for plotting).
- `measures` [`list`]
  Performance measures for this task. Can be "overruled" by user provided measures later.
- `active_rows` [`character`]: Multiset of row ids, can only contain "use" row ids.
  ALL DATA OPERATIONS ONLY REFER TO ACTIVE ROWS

##### Methods
- `data(rows = NULL, cols = NULL): char x char -> data.table`:
  Returns data filtered on row roles and col roles.
  Returns only "use" rows and "not ignore" cols (see col_info and row_info)
- `head(n = 6) -> data.table`:
  peek into the data, calls Task$data()

##### Bindings
- `features_names -> character`
  Names of all feature columns (see col_info)
- `target_names -> character`:
  Names of the target columns (see col_info)
- `formula -> formula`:
  target_names[1] ~ feature_names[1] + ... + feature_names[p]
  Apparently in R we cannot combine multiple targets with "+" on LHS
- `nrow -> integer(1)`:
  Number of observations
- `ncol -> integer(1)`:
  Number of targets + number of features

##### Comments
- we probably want a summary method, that descibes the Task by peeking a bit into the data
- we can maybe not use col_info="use", just validation / ignore, that would save quite some mem?
- inheritings tasks can specialize task meta and view informration by adding member varsnd, using the current
  row / col info differently and overwriting logic operation
- Task (their meta info like row and colinfo and active_rows) are mutable, but this is a dangerous
  operation ion the reference. The convention is to clone the task first - e.g. in a complex
  function which would rewrite meta-view-info
- BENUTZEN WIR CHAR ODER FACT (bei cols, target und features und allgemein?)
- We might want to differentiate between binary and non-binary classification tasks

## TaskSupervised :: TaskAbstract
## TaskUnSupervised :: TaskAbstract
##### Comments
- currently both pretty empty but probably good idea to ahave them


## TaskClassif :: TaskSupervised

##### Initialize
- `function(id, backend, target, positive)`

##### Methods
- `truth(row_ids = active_rows) --> factor :
  Get true labels for provided rows

##### Bindings
- `class_names -> character()`.
- `class_n -> integer(1)`

## TaskRegr :: TaskSupervised

##### Initialize
- `function(id, backend, target)`

##### Methods
- `truth(row_ids = active_rows) --> numeric` :
  Get true response for provided rows


## Learner


##### Members
- `id` [`string`]
- `packages` [`character()`]
- `par_set` [`ParamSet`]
- `properties` [`character()`]
  Defines what the learner can do. See capabilities.R
- `model`
  Model from underlying R package. Has to be set for model ops like predict to work. We do this in Experiment

##### Methods
- `trainInternal` [`function(task)`] --> model
- `predictInternal` [`function(task)`]  --> data.table.
  Cols = response | prob or response | se. 1st col is mandatory, 2nd is optional depending on predict_type


##### Bindings
- `<-.par_vals` -->`list`:
  Gets and sets hyperpars. For the setter you have to provide the complete list of new settings.
- `<-.predict_type` -> character(1):
  Retrieve / Set prediction type.


##### Comments
- ParamSet could be loaded lazily. Objects are kind of large and you typically don't work with dozens of learners. Suggestion ML: Store params as JSON in `inst/parsets`?
- We probably want to enricht the idea here later. Candidates:
  plot, getOOBPredictions, trainContinue, getFeatureImportance, getSelectedFeature
  If we add stuff here, should add a capability.
- trainInternal and predictInternal are never called by the user / client code. Consider making them private!
- Do we still describe defaults in par.set


## LearnerClassif :: Learner
## LearnerRegr :: Learner



TODO MOVE RUNTER EXPERIMENT - Learners train and predict in separate R sessions ()


### Measure
Tranforms an experiment into a scalar performance number (and defines an aggregation over resampling)

##### Members
- `id` [`string`]
- `task_types` [`character()`]
- `packages` [`character()`]
- `minimize` [`logical(1)`]
- `range` [`numeric(2)`]

##### Methods
- `calculate` [`function(experiment)`] --> `numeric(n)`
- `aggregate` [`function(ResampleResult)`] --> `numeric(1)`
   Default: function(rr) mean(r$performances)


##### Comments
- If you want to change the aggregate function you need to inherit and overwrite
- We will wrap many functions from the Metrics package
- We should talk to PB why he created a new measures package?




### Experiment
Execution control and container object for train-predict-score experiments.

##### Initialize
- `initialize(task, learner, train_inds, test_inds)`

##### Members
- `data` [`list`]: Storage object for all objects.



```
> capabilities$experiment_slots
           name       type atomic
 1:        task       Task  FALSE
 2:     learner    Learner  FALSE
 3:  resampling Resampling  FALSE
 4:   iteration    integer   TRUE
 5:   train_log data.table  FALSE
 6:  train_time    numeric   TRUE
 8:    test_log data.table  FALSE
 9:   test_time    numeric   TRUE
10:   predictions data.table FALSE
11:   scores named-numeric  FALSE
```

Split into train and test are stored in a "Resample" object for experiments.
Iteration is always == 1 for experiments.
This gurarantees that experiments have the same interface, regardless if constructed manually via a single experiment or via resample/benchmark.

Objects are stored in list data. This is a bit non-standard, as for a single Exp, normal member-vars would
be natural. But we later want to build up tables of repeated experiments (in resample and benchmark). In these
cases we build up a table, where each above list becomes a row in a data.table, resulting in structure we can operate and aggregate on (split-apply-combine paradigm).
These later classes have some "sugar" to convert single rows back to proper experiment objects.



##### Methods
- `train` [`function(row_ids = NULL)`]:
   Trains learner on task with train_ids (or passed argument ids) and stores model in learner.
   Predictions/scores are NULLed. Argument ids become train_set.
- `predict` [`function(row_ids)`]
   Predicts learner on task with test_ids (or passed argument ids) and stores
   predictions in`self$data$predicted`. Scores are NULLed. Argument ids become test_set.
- `score` [`function(measures = NULL)`]:
  Evaluates `self$data$predicted` on `y[test_set]` and stores performance values in `self$data$performance` using `task$measures`.
  `measures` defaults to measures stored in `task`.

##### Bindings
- `logs` [`list`]: Slots `$train` and `$predict`.
- `train_set`: [`integer` | `character`] with row ids
- `test_set`: [`integer` | `character`] with row ids
- `predictions` [`data.table`]: `row_id | predicted[vector]`
  Example classif with response:
  ```
  row_id | response (char)
  ------------------------
  1      | "a"
  2      | "b"
  ```
  Example classif with probability:
  ```
  row_id | response | probabilites (list-col of numvecs)
  ------------------------------------------------------
  1      | "a"      | c(0.7, 0.3)
  2      | "b"      | c(0.5, 0.5)
  ```
- `performance`: [`data.table`] `measure_id | performance`.
  Retrieve performances from score-method call. nrows = nr of measures
- `has_error` [logical(1)]:
  Did error occur during execution
- `state` [`ordered`]:
  How much of the experiment is done (as ordered factor):
  "initialized", "trained", "predicted", "scored"


##### Comments
- train, predict and score call internal worker functions using the future package.
  The user can choose how to execute this worker, e.g. with `plan(callr)` where functions are exectured in an external R process.
  The future backend also determines how much of the output can be stored in the log.
  To capture all output, `future.callr` is required.
- We have to define how to muffle output, would be good, if this can be done by future / callr
- WAS PASSIERT WENN EIN LEARNER KAPUTT GEHT? WO SIND MESSAGES, GIBT ES FALLBACK LEARNER DER DOCH GEHT


## Resampling

Description object for resampling, can be instantiated with concrete splits


##### Subclasses
ReasamplingHoldout, ResamplingCV, ResamplingRepeatedCV, ResamplingSubsampling, ResamplingCustom, ResamplingBootstrap


##### Initialize
- `function()`

##### Members
- id [`character(1)`]
- [additional parameters depending on specialization, e.g. folds]

##### Methods
- `instantiate(task)`: updates Resampling and stores fixed splits into train/test
- `train_set(i)`: returns indices of i-th training set
- `test_set(i)`: returns indices of i-th test set

##### Bindings
- `iters` [`integer(1)`]: Returns number of iterations.
- `is_instantiated` [`logical(1)`].
- `checksum` [`character(1)`] hash of stored resampling indices. Unsure if this is required?

##### Comments
- ResamplingCustom allows to create your own multiple splits
- Stratification is an obvious extension
- Can we maybe export the helper function which generate the split datatables?


## ResampleResult

```
rr = resample(task, lrn, resampling)
```

Creates as a datatable of multiple Experiments

##### Initialize
- `function(data)`: data is `data.table` of experiment data, e.g.
  ```
  data = rbind(e1$data, e2$data)
  ```
  Assumes that learner, task and resamlpling refs are all equal in the experiments

##### Members
- data [`data.table`]: internal data structure, each row = data of 1 experiments
  See reflections$experiment_slots (or experiment above) for list of columns


##### Methods
- `experiment(i) -> Experiment`: Constructs the i-th experiment from internally stored data
- `experiments(i) -> list of Experiments`: Constructs slice of all i experiments from internally stored data

##### Bindings
- learner: [Learner]
- task: [Task]
- resampling: [Resampling]
- measures: [list of Measure]
- `performance_iter`: `data.table` with columns `measure_id ` | `performance` | `iter`
- `performance_aggr` -> `data.table` with columns `measure_id ` | `performance`

##### Comments
WERDEN MODELLE GESPEICHERT? IMMER? STEUERUNG?
--> resample, benchmark etc brauchen wohl ein control -object-



## BenchmarkResult
Creates as a datatable of multiple Experiments
```
benchmark(design)
```

design = data.table(task, learner, resampling)



##### Initialize
- `function(data)`: data is `data.table` of experiment data, e.g.
  ```
  data = rbind(e1$data, e2$data)
  ```

##### Members
- data [`data.table`]: internal data structure, each row = data of 1 experiments
  See reflections$experiment_slots (or experiment above) for list of columns

> capabilities$experiment_slots
           name       type atomic
 1:        task       Task  FALSE
 2:     learner    Learner  FALSE
 3:  resampling Resampling  FALSE
 4:   iteration    integer   TRUE
 ......



##### Methods
- `experiment(i) -> Experiment`: Constructs the i-th experiment from internally stored data
- `experiments(i) -> list of Experiments`: Constructs slice of all i experiments from internally stored data
- `resample_result(task_id, lrn_id, resa_id)` -> ResampleResult
  ```
  ResamplingResult$new(self$data[ids(task) == task_id & ids(learner) == lrn_id & ids(resampling) = resa_id])
  ```
-

##### Bindings
- `performance_iter`: `data.table` with columns `learner` | `task` | `iter` | `measure_id ` | `performance`
- `performance_aggr` -> `data.table` with columns `learner` | `task` | `measure_id` | `performance`

##### Comments

- You can work on the data dt nicely, by using data[,, by = ids(attask)]
- We might want to create a helper to create a crossprod design table
- bei gettern wie resample_esult könnte man sich überlegen ob man "eindeutige" argumente weg
  lassen kannlassen kann (es gibt nur einen task oder so)

ACHTUNG WIR SOLLTEN SO NETTE HELPERS wie "ids" MAL AUFLISTEN


#####



Was fehlt noch alles?


-- errorhandling und budget constraints
---- mit plan(callr) läuft der fit in einer anderen session
---- wir machen einen fallbackmechanismus beim Experiment, so dass predict random-dummy-preds auspuckt.
     es gibt keine NA preds mehr, man hat immer (technisch valide) preds
---- das callr backend hat hoffentlich einen timeout... wir wollen das nicht selbst schreiben
---- es würde auch mit batchtools wohl gehen, wenn man slurm oder docker nachinstalliert (oder sowiieso hat)

-- parallel
---- library(future); plan(multcicore) oder plan(batchtools)
ACHTUNG NOCH PARALLELISIERUNG VON LEARNER beachten, wie xgboost

-- option handling
---- logging von mlr --> wir benutzen ein paket (oder schreiben ein selbst, aber lieber nicht)
---- logging learner ---> option vom learner: <off, store_in_learner, console>
---- params ohne desc  ---> das wird eine option vom learner


## FitnessFunction
Creates a fitness function object from experiment data, and allows automatic logging of results.

##### Members
  exp_store WIR BRAUCHEN EINE KLASSE HIER?
  task, learner, resampling, par_set, measures

##### Initialize
- `function(task, learner, resampling, par_set, measures)`
  (We allow multiple measures, for multicrit later)

##### Methods
-  eval(x) --> -> numeric(length(measures))
  Basically calls resample, returns performance scores, and logs results

## TunerTermination
Creates a fitness function object from experiment data, and allows automatic logging of results.
##### Initialize + Members
- simple settings to check for termination
##### Methods
is_terminated(expstore) --> logical(1)



## TunerAbstract

##### Subclasses
`TunerRandom`, `TunerGrid` etc. pp.

##### Initialize
- `function(id, tuner_settings, terminations)`

##### Members
- `id` [character(1)]
- `packages` [character]
-  learner, task, resampling, parset
- `tuner_settings` [list] - arguments of call function
- `terminations` [list of Terminations] of termination criterions, if any is fullfilled we stop
- x [list] <- is set by tune()
- y [numeric] - <- is set by tune()


##### Methods
- `tune(task, learner, resampling, par_set) -> void
- (Optional) `continue()` continues optimization, `terminatations` has to be updated before.


tt = TunerRandom$new()
tt = tt$tune(task, learner, resampling, par_set)
tuneCMAES {
  tune(task, learner, resampling, par_set) {
    lower, upper <- par_set
    fn = Fitness(task, learner, resampling, par_set) {)
    cmaes(fitn, lower, upper)
    fitnessfunction "self logging"
  }
}


ctrl = TuneControl$new()
tune.result = tune(task, learner, ctrl)



# Conventions

## Naializedng

- Functions: `do_something(row_ids = selected_ids)`
- all variables: `some_vector`
- R6:
 - Methods: `SomeClass$do_something(...)` (always verb!)
 - Active Bindings: `SomeClass$thing`

## Training

- Data that goes into Learner$train() is already subsetted
- All active bindings on Data are calculated for active training observations





# Renamings

- Backend --> DataBackend

# Questions

- How to control
  - resampling (e.g. do not store model etc.)
  - benchmarking
  - wir brauchen vermutlich TaskClassifBinary -




