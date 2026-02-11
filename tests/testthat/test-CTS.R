# Testing the Clinical Trial Simulation (CTS) module exported functions.
# The CTS module depends on the Model Builder (MB) module for source models
# and on the rxode2 family of packages for simulation. All tests are wrapped
# in a guard that checks for the rxode2 family availability.
suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))
if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(suppressWarnings(library(rxode2)))

  # ------------------------------------------------------------------
  # Basic session creation: verifies that CTS_test_mksession() can

  # build both a minimal (full=FALSE) and complete (full=TRUE) test
  # session without errors.
  # ------------------------------------------------------------------
  test_that("Load CTS test sessions", {
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=FALSE)))
    expect_true(sess_res[["isgood"]])
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=TRUE)))
    expect_true(sess_res[["isgood"]])
  })

  # ------------------------------------------------------------------
  # CRUD (Create, Read, Update, Delete) operations on CTS elements.
  # This mirrors the workflow in inst/test_apps/CTS_funcs.R:
  #   1. Create an MB session (CTS needs models from MB)
  #   2. Initialize a CTS state via CTS_fetch_state()
  #   3. Assign a source model to the element
  #   4. Add dosing rules and covariates
  #   5. Create and delete elements, verifying counts change
  # ------------------------------------------------------------------
  test_that("CTS element CRUD operations", {
    # CTS requires an MB session because cohort simulations reference
    # models built by the Model Builder module
    sess_res = suppressMessages(suppressWarnings(MB_test_mksession()))
    expect_true(sess_res[["isgood"]])
    session = sess_res[["session"]]

    # Initialize a fresh CTS state using the yaml configuration files
    id     = "CTS"
    input  = list()
    FM_yaml_file  = system.file(package = "formods",  "templates", "formods.yaml")
    MOD_yaml_file = system.file(package = "ruminate", "templates", "CTS.yaml")

    state = suppressMessages(suppressWarnings(
      CTS_fetch_state(id              = id,
                      input           = input,
                      session         = session,
                      FM_yaml_file    = FM_yaml_file,
                      MOD_yaml_file   = MOD_yaml_file,
                      react_state     = NULL)))

    # READ: Fetch the current (default) element from the state
    current_ele = CTS_fetch_current_element(state)
    expect_true(is.list(current_ele))

    # UPDATE: Assign the first MB model as the source for this cohort.
    # This triggers CTS_change_source_model() which updates the element's
    # rx_details, model_label, and MDLchecksum.
    state[["CTS"]][["ui"]][["source_model"]] = "MB_obj_1_rx"
    current_ele = suppressMessages(suppressWarnings(
      CTS_change_source_model(state, current_ele)))
    expect_true(is.list(current_ele))

    # Configure a single-visit design with 5 simulation steps
    current_ele[["ui"]][["visit_times"]]       = "0"
    current_ele[["ui"]][["cts_config_nsteps"]] = "5"

    # Define a single-dose rule: dose 1 unit into "central" at time 0.
    # The rule UI fields are set in the state, then CTS_add_rule() reads
    # them and appends the rule to the element's rule table.
    state[["CTS"]][["ui"]][["rule_condition"]]          = "time == 0"
    state[["CTS"]][["ui"]][["rule_type"]]               = "dose"
    state[["CTS"]][["ui"]][["action_dosing_state"]]     = "central"
    state[["CTS"]][["ui"]][["action_dosing_values"]]    = "c(1)"
    state[["CTS"]][["ui"]][["action_dosing_times"]]     = "c(0)"
    state[["CTS"]][["ui"]][["action_dosing_durations"]] = "c(0)"
    state[["CTS"]][["ui"]][["rule_name"]]               = "Single_Dose"

    current_ele = suppressMessages(suppressWarnings(
      CTS_add_rule(state, current_ele)))
    expect_true(is.list(current_ele))

    # Set plotting configuration and reduce subjects/steps for speed
    current_ele[["ui"]][["fpage"]]             = "1"
    current_ele[["ui"]][["dvcols"]]            = "Cc"
    current_ele[["ui"]][["nsub"]]              = "2"
    current_ele[["ui"]][["cts_config_nsteps"]] = "5"

    # UPDATE: Save the modified element back into the state. This also
    # triggers code generation for the element.
    state = suppressMessages(suppressWarnings(
      CTS_set_current_element(state = state, element = current_ele)))
    expect_true(is.list(state))

    # Add a log-normal body weight covariate (WT) with mean 70 and CV 0.1.
    # The covariate UI fields are set in state, then CTS_add_covariate()
    # parses them and attaches the covariate to the element.
    state[["CTS"]][["ui"]][["covariate_value"]]         = "70, .1"
    state[["CTS"]][["ui"]][["covariate_type_selected"]] = "cont_lognormal"
    state[["CTS"]][["ui"]][["selected_covariate"]]      = "WT"
    current_ele = CTS_fetch_current_element(state)
    current_ele = suppressMessages(suppressWarnings(
      CTS_add_covariate(state, current_ele)))
    expect_true(is.list(current_ele))

    # CREATE: Add a new empty cohort element and verify count increases
    n_before = length(names(state[["CTS"]][["elements"]]))
    state = suppressMessages(suppressWarnings(CTS_new_element(state)))
    n_after = length(names(state[["CTS"]][["elements"]]))
    expect_equal(n_after, n_before + 1)

    # DELETE: Remove the current element and verify count decreases
    state = suppressMessages(suppressWarnings(CTS_del_current_element(state)))
    n_del = length(names(state[["CTS"]][["elements"]]))
    expect_equal(n_del, n_after - 1)
  })

  # ------------------------------------------------------------------
  # Simulation and plotting: uses a full test session (which has a
  # completed simulation) to test CTS_plot_element() and CTS_fetch_ds().
  # ------------------------------------------------------------------
  test_that("CTS simulation and plotting", {
    # The full=TRUE session includes a pre-run simulation result
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=TRUE)))
    expect_true(sess_res[["isgood"]])
    state = sess_res[["state"]]

    # Generate timecourse and event plots from the simulation results.
    # CTS_plot_element() runs code that calls plot_sr_tc() and plot_sr_ev()
    # internally, storing the figures in element[["plotres"]].
    current_ele = CTS_fetch_current_element(state)
    current_ele = suppressMessages(suppressWarnings(
      CTS_plot_element(state, current_ele)))
    expect_true(is.list(current_ele))
    expect_true("plotres" %in% names(current_ele))

    # Save the plotted element back so the state reflects the plot results
    state = suppressMessages(suppressWarnings(
      CTS_set_current_element(state = state, element = current_ele)))

    # CTS_fetch_ds() collects timecourse and event datasets from all
    # elements that have successful simulations. Returns a list with
    # isgood, hasds, and ds (the dataset collection).
    ds = suppressMessages(suppressWarnings(CTS_fetch_ds(state)))
    expect_true(is.list(ds))
    expect_true("isgood" %in% names(ds))
    expect_true("hasds"  %in% names(ds))
    expect_true("ds"     %in% names(ds))
  })

  # ------------------------------------------------------------------
  # Utility functions: code generation, checksum updates, simulation
  # validation, configuration metadata, and preload state serialization.
  # ------------------------------------------------------------------
  test_that("CTS utility functions", {
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=TRUE)))
    expect_true(sess_res[["isgood"]])
    state = sess_res[["state"]]

    # CTS_fetch_code() returns the R code that reproduces the current
    # CTS module state (all cohort simulations)
    code = suppressMessages(suppressWarnings(CTS_fetch_code(state)))
    expect_true(is.character(code))

    # CTS_update_checksum() recalculates the module-level checksum from
    # individual element checksums; used to trigger downstream updates
    state = suppressMessages(suppressWarnings(CTS_update_checksum(state)))
    expect_true(!is.null(state[["CTS"]][["checksum"]]))

    # CTS_sim_isgood() inspects an element's simres to determine whether
    # the simulation completed successfully
    current_ele = CTS_fetch_current_element(state)
    sim_check = suppressMessages(suppressWarnings(
      CTS_sim_isgood(state, current_ele)))
    expect_true(is.list(sim_check))
    expect_true("isgood" %in% names(sim_check))
    expect_true("msgs"   %in% names(sim_check))

    # CTS_fetch_sc_meta() reads the CTS yaml configuration and returns
    # simulation config metadata (parameter names, UI mappings, types)
    sc_meta = suppressMessages(suppressWarnings(CTS_fetch_sc_meta()))
    expect_true(is.list(sc_meta))
    expect_true("config"      %in% names(sc_meta))
    expect_true("cfg_summary" %in% names(sc_meta))

    # CTS_mk_preload() serializes the current CTS state into a yaml-
    # compatible list that can be used to recreate the session later
    pl_res = suppressMessages(suppressWarnings(CTS_mk_preload(state)))
    expect_true(is.list(pl_res))
    expect_true("isgood"    %in% names(pl_res))
    expect_true("yaml_list" %in% names(pl_res))
  })

  # ------------------------------------------------------------------
  # Report generation: verifies that CTS_append_report() can produce
  # code for an xlsx report in code-only mode (no actual file written).
  # ------------------------------------------------------------------
  test_that("CTS_append_report generates report code", {
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=TRUE)))
    state = sess_res[["state"]]

    # Passing gen_code_only=TRUE means the function generates the code
    # to build the report but does not execute it
    rpt = list(summary = list(), sheets = list())
    rpt_res = suppressMessages(suppressWarnings(
      CTS_append_report(state,
                        rpt           = rpt,
                        rpttype       = "xlsx",
                        gen_code_only = TRUE)))
    expect_true(is.list(rpt_res))
    expect_true("isgood"    %in% names(rpt_res))
    expect_true("hasrptele" %in% names(rpt_res))
    expect_true("code"      %in% names(rpt_res))
  })

  # ------------------------------------------------------------------
  # Edge cases for CTS_sim_isgood(): verifies it correctly detects
  # a failed simulation and a missing simulation result.
  # ------------------------------------------------------------------
  test_that("CTS_sim_isgood detects bad simulation", {
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=TRUE)))
    state = sess_res[["state"]]

    # Case 1: Element with a simulation that reported isgood=FALSE
    bad_ele = list(simres = list(isgood = FALSE))
    bad_check = suppressMessages(suppressWarnings(
      CTS_sim_isgood(state, bad_ele)))
    expect_false(bad_check[["isgood"]])
    expect_true(length(bad_check[["msgs"]]) > 0)

    # Case 2: Element with an empty simres (no "isgood" key at all)
    no_sim_ele = list(simres = list())
    no_sim_check = suppressMessages(suppressWarnings(
      CTS_sim_isgood(state, no_sim_ele)))
    expect_false(no_sim_check[["isgood"]])
  })
}
