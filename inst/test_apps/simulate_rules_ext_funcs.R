library(formods)
library(ggplot2)

# For more information see the Clinical Trial Simulation vignette:
# https://ruminate.ubiquity.tools/articles/clinical_trial_simulation.html

# None of this will work if rxode2 isn't installed:
if(is_installed("rxode2")){
library(rxode2)
set.seed(8675309)
rxSetSeed(8675309)
# This creates the object my_model which contains the 
# function definition for the rxode2 model shown just below
source(system.file(package="ruminate", 
                   "test_apps", 
                   "test_rxode2_system.R"))

message(paste0(readLines(
  system.file(package="ruminate", 
              "test_apps", 
              "test_rxode2_system.R")), 
  collapse="\n"))

# This creates an rxode2 object
object  = rxode(my_model)

# If you want details about the parameters, states, etc
# in the model you can use this:
rxdetails = fetch_rxinfo(object)

rxdetails$elements

# Next we will create subjects. To do that we need to 
# specify information about covariates:
nsub = 24
covs = list(
  SEX_ID     = list(type     = "discrete", 
                    values   = c(0,1)),
  SUBTYPE_ID = list(type     = "fixed",
                    values   = c(0)),
  WT         = list(type     = "continuous",
                    sampling = "log-normal",
                    values   = c(70, .15))
)

subs = mk_subjects(object = object,
                   nsub   = nsub,
                   covs   = covs)

head(subs$subjects)

rules = list(
  low_dose = list(
    condition = "time != 45",
    true_flag  = 3e6,
    false_flag = 0,
    action    = list(
      type  = "dose",
      state     = "Ac", 
      values    = "c(3,  3,  3,  3)*1e6/MW",
      times     = "c(0, 14, 28, 42)",
      durations = "c(0,  0,  0,  0)")
    ),
  reset_Ac    = list(
    condition = "time == 45",
    true_flag  = "Ac/2",
    false_flag = "",
    action    = list(
      type     = "set state",
      state    = "Ac",
      value    = "Ac/2")
    ),
  reset_Cp    = list(
    condition = "time == 45",
    true_flag  = "Cp/2",
    false_flag = "",
    action    = list(
      type     = "set state",
      state    = "Cp",
      value    = "Cp/2")
  )
)

# We evaulate the rules for dosing at time 0
eval_times =  0

# We also do a state reset at 45 days.
eval_times = sort(unique(c(45, eval_times)))

# Stop 2 months after the last dose
output_times = seq(0, 56, 1)

rx_options = list(
   covsInterpolation = "locf"
)

simres = 
simulate_rules(object   = object,
               subjects      = subs[["subjects"]],
               eval_times    = eval_times,
               output_times  = output_times, 
               rules         = rules,
               rx_options    = rx_options)

# First subject data:
sub_1 = simres$simall[simres$simall$id == 1, ]

# First subjects events
evall = as.data.frame(simres$evall)
ev_sub_1 = evall[evall$id ==1, ]

simall = simres$simall
simall$id = as.factor(simall$id)

# Timecourse
psim = 
  plot_sr_tc(
    sro    = simres,
    dvcols = "Cc")
psim$fig

# Events
pev = 
  plot_sr_ev(
    evplot = c(1,4),
    sro    = simres,
    ylog   = FALSE)
pev$fig

}
