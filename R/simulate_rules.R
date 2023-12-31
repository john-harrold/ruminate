#'@import dplyr
#'@import rxode2
#'@importFrom stats rnorm  runif


#'@export
#'@title Rule-Based simulates
#'@description Simulate an rxode2 model based on rules evaluated at specified
#'time-points. For example if you want to titrate dosing based on individual
#'plasma levels you could create a rule that changes dosing at specified time
#'points based on the last observation of the user.
#'@param object rxode2 model object  An ID string that corresponds with the ID
#'used to call the modules UI elements
#'@param subjects  Dataframe of subject level information.
#'@param eval_times   Vector of simulation times to evaulate the rules (units are
#'time units of the system).
#'@param output_times  Specific output times to include. Other times will be
#'included as well to ensure proper evaluation of the rules.
#'@param rules  List of rules, see below for a description of the format.
#'@param rx_options List of options to pass through to `rxSolve()`.
#'@param preamble Character string of user-defined code to execute in
#'rule-evaluation environment (e.g. you can put user-defined functions here).
#'@param smooth_sampling Boolean when TRUE will insert sampling just before
#'dosing to make sampling smooth.
#'@return List with the following elements:
#' \itemize{
#'  \item{isgood:}     Return status of the function.
#'  \item{msgs:}       Error or warning messages if any issues were encountered.
#'  \item{simall:}     Simulation results.
#'  \item{ev_history:} The event table for the entire simulation history.
#'}
#'@details
#' For a detailed examples see \code{vignette("clinical_trial_simulation", package = "ruminate")}
#'@includeRmd  vignettes/rmdhunks/simulate_rules.Rmd
#'@example inst/test_apps/CTS_funcs.R
simulate_rules <- function(object,
                           subjects,
                           eval_times,
                           output_times,
                           rules, rx_options=list(),
                           preamble = "",
                           smooth_sampling=TRUE){


  eval_times = sort(eval_times)

  msgs        = c()
  isgood      = TRUE
  simall      = NULL
  sub_ids     = as.numeric(subjects[["id"]])
  ev_history  = NULL
  rx_details  = fetch_rxinfo(object)
  nsub        = length(unique(subjects[["id"]]))

  if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
    #---------------------------------------------------
    # Extracting covariates and parameters from subjects:
    iCov = list()
    params = subjects
    if(length(rx_details[["elements"]][["covariates"]])>0){
      iCov_cols = rx_details[["elements"]][["covariates"]]
      # Removing the coviariates from the parameters table
      for(cname in c("time", iCov_cols)){
        iCov[[cname]] = params[[cname]]
        params[[cname]] = NULL
      }
      iCov = as.data.frame(iCov)
      if(!("time" %in% names(iCov))){
        iCov[["time"]] = 0
      }
    }

    params = dplyr::group_by(params, .data[["id"]]) |>
      dplyr::filter(row_number()==1)                |>
      dplyr::ungroup()

    # If there is a time column we remove that
    if("time" %in% names(params)){
      params[["time"]] = NULL
    }
    #---------------------------------------------------
    # Converting rx_options into a string to use below
    rx_options_str = ""

    if(!is.null(names(rx_options))){
      for(optname in names(rx_options)){
        rx_options_str = paste0(rx_options_str, ", ", optname, "=", deparse(rx_options[[optname]]))
      }
    }

    # Creating standard functions
    std_fcns = '
    SI_fpd = function(id=NULL, state=NULL){
      fpd = -1

      if(is.null(id) | is.null(state) | is.null(SI_ev_history)){
        fpd = -1
      }else{

        # All of the events as a data frame:
        evall = as.data.frame(SI_ev_history)

        # Just the dosing records for the current subject/state
        drecs =  evall[evall[["id"]] == id & evall[["cmt"]] == state & evall[["evid"]] == 1, ]
        if(nrow(drecs) > 0){
          # Making sure everything is sorted correctly:
          drecs = drecs[with(drecs, order(time)),]

          # Pulling out the last dose
          fpd = drecs[nrow(drecs), ][["amt"]]
        }
      }


    fpd}
    '  

    preamble = paste0(c(std_fcns, preamble), collapse="\n")

    #------------------------------------
    # Checking rules:
    for(rule_id in names(rules)){
      # This makes sure there are flags for the true and false evaluations
      if(!("true_flag" %in% names(rules[[rule_id]]))){
        rules[[rule_id]][["true_flag"]] = "true"
        msgs = c(msgs, paste0('The true_flag is defaulting to "true" for rule: ', rule_id))
      }
      if(!("false_flag" %in% names(rules[[rule_id]]))){
        rules[[rule_id]][["false_flag"]] = "false"
        msgs = c(msgs, paste0('The false_flag is defaulting to "false" for rule: ', rule_id))
      }


      # Next we walk through all the required fields and sub fields to make sure
      # they exist
      if(!("condition" %in% names(rules[[rule_id]]))){
        isgood = FALSE
        msgs = c(msgs, paste0("No condition has been defined for rule: ", rule_id,))
      }

      if(("action" %in% names(rules[[rule_id]]))){
        if("type" %in% names(rules[[rule_id]][["action"]])){
          allowed_types = c("dose", "set state", "manual")
          if(rules[[rule_id]][["action"]][["type"]] %in% allowed_types){
            # The required fields depend on the action type:
            if(rules[[rule_id]][["action"]][["type"]] == "dose"){
              required_fields = c("values", "times", "durations") }
            if(rules[[rule_id]][["action"]][["type"]] == "set state"){
              required_fields = c("state", "value") }
            if(rules[[rule_id]][["action"]][["type"]] == "manual"){
              required_fields = c("code") }

            if(!all(required_fields %in% names(rules[[rule_id]][["action"]]))){
              isgood = FALSE
              msgs = c(msgs, paste0("The following required fields are missing for rule: ", rule_id))
              msgs = c(msgs, paste0("  - missing fields: ",paste0( required_fields[!(required_fields %in% names(rules[[rule_id]][["action"]]))], collapse=", ")))
            }
          } else {
            isgood = FALSE
            msgs = c(msgs, paste0("Unrecognized action type: ", rules[[rule_id]][["action"]][["type"]], " for rule: ", rule_id))
            msgs = c(msgs, paste0("  - allowed types: ", paste0(allowed_types, collapse=", ")))
          }
        } else {
          isgood = FALSE
          msgs = c(msgs, paste0("No action type has been defined for rule: ", rule_id,))
        }
      } else {
        isgood = FALSE
        msgs = c(msgs, paste0("No action has been defined for rule: ", rule_id,))
      }

    }

    #------------------------------------
    # Making sure there is a time column
    # If we add time it breaks everything.
   #if(!("time" %in% names(subjects))){
   #  subjects[["time"]] = 0
   #}

    #------------------------------------
    # Checking covariates:
    if(length(rx_details[["elements"]][["covariates"]])>0){
      if(!all(rx_details[["elements"]][["covariates"]] %in% names(subjects))){
        missing_covs =
        rx_details[["elements"]][["covariates"]] [
          !(rx_details[["elements"]][["covariates"]] %in% names(subjects))
                                                 ]

        missing_covs = paste0(missing_covs, collapse=", ")

        isgood = FALSE
        msgs = c(msgs, paste0("The following covariates are not defined in subjects: "))
        msgs = c(msgs, paste0("  > ", missing_covs ))
      }
    }
  } else {
    isgod = FALSE
    msgs = c(msgs, "One or more packages from the rxode2 family are missing")
  }

  # Tracking errors found to prevent repeated reporting. As errors are
  # encountered a key is created in this list. If that key exists then
  # subsequent errors are not reported.
  errors_found = list()

  if(isgood){
    # This will strip out any evaluation times beyond the simulation time
    # interval
    if(max(eval_times) > max(output_times)){
      msgs = c(msgs, "eval_times => output_times were skipped")
      eval_times = eval_times[eval_times < max(output_times)]
    }

    # It is necessary to insert sample times in order to ensure continuity
    # of the simulated output. This defines the small delta that will be used
    # below.
    frac_sample = .0001
    frac_step   = frac_sample*(max(output_times)-min(output_times))

    # The acutal output times used are ot_sim
    ot_sim = output_times

    # Adding output times at the eval times and just before the eval times:
    ot_sim = c(ot_sim, eval_times, (eval_times - frac_step))

    # If our evaluation times occur before or at the start
    # value of our output times we need to add an output time right before the
    # first eval time so we can get a snapshot of the simulation at the first
    # evaluation time.
    if(min(eval_times) <= min(output_times)){
      tstart = min(output_times) - frac_step
      ot_sim  = c(ot_sim, tstart)
    }

    # In case we get duplicates this will eliminate them
    ot_sim = sort(unique(ot_sim))

    # This will force the simulation to initialize at the
    # first observed time. This will be important as wel
    # step through the different eval_time intervals:
    rxSetIni0(FALSE)

    # Simulating before the fist eval_time to get
    # a snapshot of the simulation:
    tmp_ot  = sort(unique(c(ot_sim[ot_sim < min(eval_times)], min(eval_times))))
    init_ev  = rxode2et::et(time=tmp_ot, id=sub_ids)
    sim_cmd = paste0(c(preamble,
                     paste0("sim = rxode2::rxSolve(object, params=subjects, events=ev", rx_options_str, ")")),
                     collapse="\n")
    tcres = FM_tc(
      cmd     = sim_cmd,
      capture = c("sim"),
      tc_env  = list(object   = object,
                     subjects = subjects,
                     ev       = init_ev))

    if(tcres[["isgood"]]){
      #sim_pre = as.data.frame(tcres[["capture"]][["sim"]])
      # JMH
      sim_pre =
      fetch_rxtc(rx_details = rx_details,
                 sim        = tcres[["capture"]][["sim"]])

      # Catching the case where there is 1 subject
      if(nsub == 1){
        sim_pre[["id"]] = 1
      }
      # Setting the rule flag for the presimulation
      for(rule_id in names(rules)){
        sim_pre[[rule_id]] = rules[[rule_id]][["false_flag"]]
      }

      simall = rbind(simall, sim_pre)
    } else {
      isgood = FALSE
      msgs = c(msgs, "System initlaization failed")
      msgs = c(msgs, tcres$msgs)
    }

    cmd_init = c(
      "if(!exists('SI_interval_ev')){  ",
      "  SI_interval_ev = NULL         ",
      "}                                   ",
      "if(!exists('SI_ud_history')){   ",
      "  SI_ud_history  = NULL         ",
      "}                                   ",
      "if(!exists('SI_ev_history')){   ",
      "  SI_ev_history  = NULL         ",
      "}                                   ")

    if(isgood){
      for(et_idx in 1:length(eval_times)){
        if(isgood){
          # Building the event table for the current evaluation time interval.
          # The following determines the bounds of the simulation time interval:
          if(et_idx == 1 & length(eval_times) > 1){
            t_min = eval_times[1]
            t_max = eval_times[1+1]
          } else if(et_idx == 1){
            t_min = eval_times[1]
            t_max = max(ot_sim)
          } else if(et_idx > 1 & et_idx < length(eval_times)){
            t_min = eval_times[et_idx]
            t_max = eval_times[et_idx+1]
          } else if(et_idx == length(eval_times)){
            t_min = eval_times[et_idx]
            t_max = max(ot_sim)
          }

          # We start with an event table that has only the output times
          tmp_ot      =  ot_sim[ t_min <= ot_sim & ot_sim <= t_max]
          interval_ev = NULL

          # This is an object the user can create within the action
          # environment of the "manual" action type.
          ud_history  = NULL

          # Now for each subject we apply the rules that are triggered
          for(sub_id in sub_ids){
            # Subject output times during this interval:
            sub_ot          = c()
            # Values of the rules after condition is evaluated during this
            # interval:
            sub_rule_flags  = list()

            # This will hold any rule actions overwriting the states during
            # this interval. This will overwrite the previous steady-state
            sub_state_reset = list()

            if(isgood){
              # This pulls out the state of the current subject at the evaluation
              # point
              sub_state       = simall[simall[["time"]] == eval_times[et_idx] & simall[["id"]] == sub_id, ]
              sub_sim_history = simall[                                         simall[["id"]] == sub_id, ]

              # This should aways be one row, but just in case we check and throw
              # a flag if something went wrong.
              if(nrow(sub_state) == 1){
                for(rule_id in names(rules)){
                  # THis is the snapshot of the current users simulation state at
                  # the evaluation point
                  tc_env = as.list(sub_state)

                  # adding the evaluation times
                  tc_env[["SI_eval_times"]]  = eval_times

                  # adding the subjects
                  tc_env[["SI_subjects"]]    = subjects

                  # This is the subject history up to this point:
                  tc_env[["SI_SUB_HISTORY"]] = sub_sim_history

                  # Evaulating the condtion for the current rule.
                  tc_env[["SI_interval_ev"]] = interval_ev

                  # This is the subject history up to this point:
                  tc_env[["SI_ud_history"]]  = ud_history

                  # This is the ev history leading up to titration
                  tc_env[["SI_ev_history"]] = ev_history

                  cmd     = paste0(c(cmd_init,
                                     preamble,
                                     paste0("condition = ", rules[[rule_id]][["condition"]]), collapse="\n"))
                  tcres =
                    FM_tc(tc_env  = tc_env,
                          capture = c("condition"),
                          cmd     = cmd)

                  if(tcres[["isgood"]]){
                    if(tcres[["capture"]][["condition"]]){
                      # Capturing the rule flag value for true
                      sub_rule_flags[[rule_id]] =  rules[[rule_id]][["true_flag"]]

                      # Dosing action
                      if(rules[[rule_id]][["action"]][["type"]] == "dose"){
                        # evaluting the times, values, and dosing in a
                        # try/catch. This enables dosing beyond just values
                        # and allows the incorporation of other simulation
                        # aspects (e.g. body weight).
                        cmd = paste0(
                          c(cmd_init,
                            preamble,
                            paste0("SI_values    = ", rules[[rule_id]][["action"]][["values"]]),
                            paste0("SI_times     = ", rules[[rule_id]][["action"]][["times"]]),
                            paste0("SI_durations = ", rules[[rule_id]][["action"]][["durations"]])),
                          collapse="\n")

                        capture = c(
                          "SI_values",
                          "SI_times",
                          "SI_durations")

                        tc_env[["SI_interval_ev"]] = interval_ev
                        tc_env[["SI_ud_history"]]  = ud_history


                        tcres =
                           FM_tc(tc_env  = tc_env,
                                 capture = capture,
                                 cmd     = cmd)

                        if(tcres[["isgood"]]){
                          # Now we're going to process the dosing
                          dvals  = as.numeric(tcres[["capture"]][["SI_values"]])
                          dtimes = as.numeric(tcres[["capture"]][["SI_times"]])
                          ddurs  = as.numeric(tcres[["capture"]][["SI_durations"]])

                          # We need to convert the dosing from the relative values
                          # to the absolute by adding in the current evaluation
                          # time point:
                          dtimes = dtimes + eval_times[et_idx]

                          # It's possible the user specified dosing times beyond
                          # the current evaluation period. If that's the case we
                          # trim off those values and throw a message:
                          if(any(dtimes >= t_max)){
                            idx_keep = which(dtimes < t_max)
                            dtimes = dtimes[idx_keep]
                            dvals  =  dvals[idx_keep]
                            ddurs  =  ddurs[idx_keep]

                            error_flag = "Warning: dosing beyond time interval"
                            if(is.null(errors_found[[error_flag]])){
                              msgs = c(msgs, error_flag)
                              msgs = c(msgs, paste0( paste0("Subject: ", sub_id, ", Rule: ", rule_id, ", Evaluation time: ", eval_times[et_idx], " doses beyond interval (", t_min, ",", t_max, ") ignored.")))
                              errors_found[[error_flag]] = "found"
                            }
                          }

                          tmp_cmt =  rules[[rule_id]][["action"]][["state"]]

                          # Here we're adding sampling at the dosing times and
                          # just before if smooth sampling has been selected
                          sub_ot = c()
                          if(smooth_sampling){
                            sub_ot = c(dtimes, (dtimes-frac_step), (dtimes+frac_step))
                            sub_ot = sub_ot[sub_ot >= t_min & sub_ot <=t_max]
                          }

                          interval_ev = rxode2et::etRbind(interval_ev,
                                           rxode2et::et(
                                              cmt  = tmp_cmt,
                                              id   = sub_id,
                                              amt  = dvals,
                                              time = dtimes,
                                              dur  = ddurs))
                        } else {
                          error_flag = "Error: Unable to evaluate dose."
                          if(is.null(errors_found[[error_flag]])){
                            isgood = FALSE
                            msgs = c(msgs, error_flag)
                            msgs = c(msgs, paste0("Subject: ", sub_id,", Rule id: ", rule_id, ", Evaluation time: ", eval_times[et_idx], " failed to evaluate dose:  ", ))
                            msgs = c(msgs, tcres[["msgs"]])
                            errors_found[[error_flag]] = "found"
                          }
                        }
                      }
                      # State reset action
                      if(rules[[rule_id]][["action"]][["type"]] == "set state"){
                        # evaluting the value component. This will contain the
                        # new value of the state.
                        cmd = paste0(
                          c(cmd_init,
                            preamble,
                            paste0("SI_value   = ", rules[[rule_id]][["action"]][["value"]])),
                          collapse="\n")

                        capture = c("SI_value")

                        tc_env[["SI_interval_ev"]] = interval_ev
                        tc_env[["SI_ud_history"]]  = ud_history

                        tcres =
                           FM_tc(tc_env  = tc_env,
                                 capture = capture,
                                 cmd     = cmd)

                        if(tcres[["isgood"]]){
                          # Resetting the state
                          sub_state_reset[[ rules[[rule_id]][["action"]][["state"]] ]] = tcres[["capture"]][["SI_value"]]
                        } else {
                          error_flag = "Error: Unable to evaluate new state value."
                          if(is.null(errors_found[[error_flag]])){
                            msgs = c(msgs, error_flag)
                            msgs = c(msgs, paste0( paste0("Subject: ", sub_id, ", Rule: ", rule_id, ", Evaluation time: ", eval_times[et_idx], " New state value not set:", rules[[rule_id]][["action"]][["value"]],".")))
                            errors_found[[error_flag]] = "found"
                          }
                        }
                      }
                      # Run manual code action
                      if(rules[[rule_id]][["action"]][["type"]] == "manual"){
                        # Evaulating the manual code
                        tc_env[["SI_interval_ev"]] = interval_ev

                        # This is the subject history up to this point:
                        tc_env[["SI_ud_history"]]  = ud_history

                        # This makes sure the values that have only been
                        # initialized in this fuction actually exist in
                        # the rule evaluation environment

                        # Appending the code
                        cmd  = paste0(c(cmd_init, preamble, rules[[rule_id]][["action"]][["code"]]), collapse="\n")

                        tcres =
                          FM_tc(tc_env  = tc_env,
                                capture = c("SI_interval_ev", "SI_ud_history"),
                                cmd     = cmd)
                        if(tcres[["isgood"]]){
                          # Extracting the interval event table from the
                          # results:
                          interval_ev = tcres[["capture"]][["SI_interval_ev"]]

                          # Also extracting the user-defined history as well:
                          ud_history  = tcres[["capture"]][["SI_ud_history"]]
                        } else {
                          error_flag = "Error: Unable to evaluate manual code."
                          if(is.null(errors_found[[error_flag]])){
                            msgs = c(msgs, error_flag)
                            msgs = c(msgs, paste0( paste0("Subject: ", sub_id, ", Rule: ", rule_id, ", Evaluation time: ", eval_times[et_idx], " Manual code not evaulated:", rules[[rule_id]][["action"]][["code"]])))
                            msgs = c(msgs, tcres[["msgs"]])
                            errors_found[[error_flag]] = "found"
                          }
                        }
                      }
                    } else {
                      # Capturing the rule flag value for false
                      sub_rule_flags[[rule_id]] =  rules[[rule_id]][["false_flag"]]
                    }
                  } else {
                    error_flag = paste0("Error: rule condition evaluation.")
                    if(is.null(errors_found[[error_flag]])){
                      isgood = FALSE
                      msgs = c(msgs, error_flag)
                      msgs = c(msgs, paste0("Subject: ", sub_id,", Rule id: ", rule_id, ", Evaluation time: ", eval_times[et_idx], ", failed to evaluate condition:  ", rules[[rule_id]][["condition"]]))
                      msgs = c(msgs, tcres[["msgs"]])
                      errors_found[[error_flag]] = "found"
                    }
                  }
                }
              }else{
                error_flag = "Error: unable to determine subjects state at evaluation point"
                if(is.null(errors_found[[error_flag]])){
                  isgood = FALSE
                  msgs = c(msgs, error_flag)
                  msgs = c(msgs, paste0("Subject: ", sub_id, ", Evaluation time: ", eval_times[et_idx], " nrows should be 1 but ", nrow(sub_state), " were found."))
                  errors_found[[error_flag]] = "found"
                }
              }
            }


            # Setting the initial conditions
            for(state in rx_details[["elements"]][["states"]]){
              # If the state is has been selected for reset we use that value:
              if(state %in% names(sub_state_reset)){
               new_state_amt  = sub_state_reset[[state]]
              } else {
               new_state_amt  = sub_state[[state]]
              }
              interval_ev = rxode2et::etRbind(interval_ev,
                               rxode2et::et(
                                  cmt  = state,
                                  id   = sub_id,
                                  amt  = new_state_amt,
                                  evid = 4,
                                  time = eval_times[et_idx]))
            }

            # Combining the subject specific sampling with
            # the interval samples:
            sub_ot       = sort(unique(c(sub_ot, tmp_ot)))
            interval_ev  = rxode2et::etRbind(interval_ev,
                           rxode2et::et(time=sub_ot, id=sub_id))


          }

          # Running the simulation for the chunk of time between the current
          # eval_time and either the next or the end of the simulation
          sim_cmd = paste0(c(
                    cmd_init,
                    preamble,
                    paste0("sim = rxode2::rxSolve(object, params=subjects, events=ev", rx_options_str, ")")),
                    collapse = "\n")
          tcres = FM_tc(
            cmd     = sim_cmd,
            capture = c("sim"),
            tc_env  = list(object   = object,
                           subjects = subjects,
                           ev       = interval_ev))

          # Storing all of the events in a single table to return to the user
          ev_history  = rxode2et::etRbind(ev_history , interval_ev)

          if(tcres[["isgood"]]){

            # This contains the current chunk of the simulation:
            #tmp_sim = as.data.frame(tcres[["capture"]][["sim"]])
            tmp_sim =
            fetch_rxtc(rx_details = rx_details,
                       sim        = tcres[["capture"]][["sim"]])

            # Setting the rule flag for the presimulation
            for(rule_id in names(rules)){
              tmp_sim[[rule_id]] = sub_rule_flags[[rule_id]]
            }

            # We need to glue the simulations together. So first we remove the
            # last time point off of the simall data frame. The last time
            # point of that data frame should be the first of this new
            # simulation:
            simall = simall[simall[["time"]] != eval_times[et_idx], ]

            # Now we stack the old simulations on top of the new one:
            simall = rbind(simall, tmp_sim)

          } else {
            error_flag = "Warning: dosing beyond time interval"
            if(is.null(errors_found[[error_flag]])){
              isgood = FALSE
              msgs = c(msgs, error_flag)
              msgs = c(msgs, paste0("Subject: ", sub_id,", Rule id: ", rule_id, ", Evaluation time: ", eval_times[et_idx], " interval simulation failed. ", ))
              msgs = c(msgs, tcres[["msgs"]])
              errors_found[[error_flag]] = "found"
            }
          }



        }
      }
    }
  }

  if(isgood){
    # sorting by id then time:
    simall = simall[ with(simall, order(id, time)), ]

    if(smooth_sampling){
      # trimming things down to the desired time interval:
      simall = simall[simall[["time"]] >= min(output_times) &
                      simall[["time"]] <= max(output_times), ]

    } else {
      # trimming things down to just the desired time outputs:
      simall = simall[simall[["time"]] %in% output_times, ]
    }

  } else {
    msgs = c("simulate_rules()", msgs)
  }

  res = list(
    simall      = simall,
    ev_history  = ev_history,
    msgs        = msgs,
    isgood      = isgood
  )
res}



#'@export
#'@title Fetches Information from an rxode2 Object
#'@description This will provide information like parameter names, covriates,
#'etc from an rxode2 object.
#'@param object rxode2 model object  An ID string that corresponds with the ID used to call the modules UI elements
#'@return  List with the following elements.
#' \itemize{
#' \item{elements:} List with names of simulation elements:
#'   \itemize{
#'   \item{covariates:} Names of the covariates in the system.
#'   \item{parameters:} Names of the parameters (subject level) in the system.
#'   \item{iiv:} Names of the iiv parameters in the system.
#'   \item{states:} Names of the states/compartments in the system.
#'   }
#' }
#'@example inst/test_apps/CTS_funcs.R
fetch_rxinfo <- function(object){

  isgood    = TRUE
  msgs      = c()
  txt_info  = c()
  ht_info   = tagList()

  if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
    # use str(object) to get the names of the list elements
    covariates = object$allCovs
    parameters = names(object$theta)
    states     = object$state
    iiv        = unique(names(object$etaLhs))
    elements = list(
      covariates = covariates,
      parameters = parameters,
      iiv        = iiv,
      states     = states)

    # State details
    txt_info = c(txt_info, "States/Compartments\n")
    ht_info  = tagList(ht_info, tags$b("States/Compartments"), tags$br())
    if(length(states) > 0){
      txt_info = c(txt_info, paste0(states, collapse=", "), "\n\n")
      ht_info  = tagList(ht_info, paste0(states, collapse=", "), tags$br(), tags$br())
    } else{
      txt_info = c(txt_info, "None found\n")
      ht_info  = tagList(ht_info, tags$em("None found"), tags$br(), tags$br())
      isgood   = FALSE
      msgs = c(msgs, "No state/compartment information found.")
    }
    # Covariates details
    txt_info = c(txt_info, "Covariates\n")
    ht_info  = tagList(ht_info, tags$b("Covariates"), tags$br())
    if(length(covariates) > 0){
      txt_info = c(txt_info, paste0(covariates, collapse=", "), "\n\n")
      ht_info  = tagList(ht_info, paste0(covariates, collapse=", "), tags$br(), tags$br())
    } else{
      txt_info = c(txt_info, "None found\n")
      ht_info  = tagList(ht_info, tags$em("None found"), tags$br(), tags$br())
    }
    # Parameters details
    txt_info = c(txt_info, "Parameters\n")
    ht_info  = tagList(ht_info, tags$b("Parameters"), tags$br())
    if(length(parameters) > 0){
      txt_info = c(txt_info, paste0(parameters, collapse=", "), "\n\n")
      ht_info  = tagList(ht_info, paste0(parameters, collapse=", "), tags$br(), tags$br())
    } else{
      txt_info = c(txt_info, "None found\n")
      ht_info  = tagList(ht_info, tags$em("None found"), tags$br(), tags$br())
    }
    # IIV details
    txt_info = c(txt_info, "Between-subject variability\n")
    ht_info  = tagList(ht_info, tags$b("Between-subject variability"), tags$br(),)
    if(length(iiv) > 0){
      txt_info = c(txt_info, paste0(iiv, collapse=", "), "\n\n")
      ht_info  = tagList(ht_info, paste0(iiv, collapse=", "), tags$br(), tags$br())
    } else{
      txt_info = c(txt_info, "None found\n")
      ht_info  = tagList(ht_info, tags$em("None found"), tags$br(), tags$br())
    }

  } else {
    isgood   = FALSE
    elements = NULL
    msgs = c(msgs, "rxode2 not installed")
  }

  if(is.null(txt_info)){
    txt_info = ""
  }



  res = list(
    isgood   = isgood,
    msgs     = msgs,
    elements = elements,
    txt_info = paste0(txt_info, collapse=""),
    ht_info  = ht_info)
res}


#'@export
#'@title Fetches Information from an rxode2 Object
#'@description This will provide information like parameter names, covriates,
#'etc from an rxode2 object.
#'@param object rxode2 model object  An ID string that corresponds with the ID used to call the modules UI elements.
#'@param nsub Number of subjects to generate.
#'@param covs List describing how covariates should be generated.
#'@return  List with the following elements.
#' \itemize{
#'  \item{isgood:}     Return status of the function.
#'  \item{msgs:}       Error or warning messages if any issues were encountered.
#'  \item{subjects:}   Data frame of parameters and covariates for the subjects generated.
#'  \item{iCov:}       Data frame of the covariates.
#'  \item{params:}     Data frame of the parameters.
#' }
#'@details See below.
#'@includeRmd  vignettes/rmdhunks/simulate_rules.Rmd
#'@example inst/test_apps/CTS_funcs.R
#'@seealso \code{vignette("clinical_trial_simulation", package = "ruminate")}
mk_subjects <- function(object, nsub = 10, covs=NULL){

  isgood       = TRUE
  msgs         = c()
  subjects     = NULL
  missing_covs = c()


  if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
    rx_details = fetch_rxinfo(object)

    if(is.null(covs) & length(rx_details[["elements"]][["covariates"]])>0){
      isgood = FALSE
      msgs = c(msgs, "Covariates were found in the model but not specified.")
      missing_covs =
        rx_details[["elements"]][["covariates"]]
    } else if(!is.null(covs) & !is.null(rx_details[["elements"]][["covariates"]])){
      if(!all(rx_details[["elements"]][["covariates"]] %in% names(covs))){
        missing_covs =
          rx_details[["elements"]][["covariates"]][
            !(rx_details[["elements"]][["covariates"]] %in% names(covs))
          ]
        isgood = FALSE
      }
    }

    if(!is.null(missing_covs)){
      msgs = c(msgs,
               "Covariates were found in the model but not specified.",
               paste0("  > ", paste0(missing_covs, collapse=", ")))
    }

    # If we made it here then we've checked everything successfully
    if(isgood){
      iCov = data.frame(
                id         = as.factor(c(1:nsub)))

      for(covname in names(covs)){
        # Discrete and continuous distributions are treated differently
        if(covs[[covname]][["type"]] == "discrete"){
          iCov[[covname]] =  sample(covs[[covname]][["values"]], nsub, replace=TRUE)
        }
        if(covs[[covname]][["type"]] == "fixed"){
          iCov[[covname]] =  covs[[covname]][["values"]]
        }
        # JMH run these sampling methods by someone
        if(covs[[covname]][["type"]] == "continuous"){
          if(covs[[covname]][["sampling"]] %in% c("normal", "log-normal")){
            if(covs[[covname]][["sampling"]] == "normal"){
              iCov[[covname]] = covs[[covname]][["values"]][1] +
                stats::rnorm(
                mean = 0,
                sd   = covs[[covname]][["values"]][2],
                n    = nsub
              )
            }
            if(covs[[covname]][["sampling"]] == "log-normal"){
              iCov[[covname]] = covs[[covname]][["values"]][1] *
                exp(stats::rnorm(
                    mean = 0,
                    sd   = covs[[covname]][["values"]][2],
                    n    = nsub
                  ))
            }
          }
          if(covs[[covname]][["sampling"]] == "random"){
            iCov[[covname]] =  stats::runif(min = covs[[covname]][["values"]][1],
                                            max = covs[[covname]][["values"]][2],
                                            n   = nsub)
          }
        }
      }

      #-----------------------------------------------------------------
      # JMH figure out a better way to do this using low level functions
      ev <-rxode2et::et(amt=0, cmt=1, id=c(1:nsub)) |>
           add.sampling(c(0,1))
      sim  <- rxode2::rxSolve(object, ev, nSub=nsub, iCov = iCov)
      params = as.data.frame(sim$params)

      # Just extracting the parameters we need for running simulations:
      col_keep = c("id",
                   rx_details[["elements"]][["parameters"]],
                   rx_details[["elements"]][["iiv"]])

      # If only one subject is selected no id column is created. This creates
      # one to make everything else work below.
      if(nsub == 1){
        params[["id"]] = 1
      }

      params   = dplyr::select(params, dplyr::all_of(col_keep)) |>
        dplyr::mutate("id" := as.factor(.data[["id"]]))

      subjects = dplyr::left_join(params, iCov, by="id")
      #-----------------------------------------------------------------
    }


  } else {
    isgood   = FALSE
    msgs = c(msgs, "rxode2 not installed")
  }

  res = list(subjects   = subjects,
             iCov       = iCov,
             params     = params,
             msgs       = msgs,
             isgood     = isgood)
res }

#'@title Extracts Timecourse and Merges Covariates
#'@description Takes the output of `rxSolve()` and merges in any missing
#'covariates that are present in params but not in sim
#'@param rx_details Output of `fetch_rxinfo()`
#'@param sim output of `rxSolve()`
#'@return  Dataframe of the simulated time course.
fetch_rxtc <- function(rx_details, sim){

  rxtc = NULL
  if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
    # This is a temporary (hopefully) fix until this feature is added:
    # https://github.com/nlmixr2/rxode2/issues/638
    rxtc   = as.data.frame(sim)
   
    # Catching the case where there is 1 subject and no "id" column
    if(!("id" %in% names(rxtc))){
      rxtc[["id"]] = 1
    }
   
   ## Merging any covariates from params
   #if(length(rx_details[["elements"]][["covariates"]])>0){
   #  params = as.data.frame(sim$params)
   #  # Catching the case where there is 1 subject and no "id" column
   #  if(!("id" %in% names(params))){
   #    params[["id"]] = 1
   #  }
   #
   #  iCov_cols = c("id", rx_details[["elements"]][["covariates"]])
   #  iCov   = dplyr::select(params, dplyr::all_of(iCov_cols)) |>
   #           dplyr::mutate("id" := as.numeric(.data[["id"]]))
   #  rxtc = dplyr::left_join(rxtc, iCov, by="id")
   #}
  }


rxtc}
