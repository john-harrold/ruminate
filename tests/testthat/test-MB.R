# Testing the Model Builder (MB) module exported functions.
# The MB module provides an interface for selecting, building, and managing
# rxode2 pharmacometric models from the nlmixr2lib catalog. All tests require
# the rxode2 family of packages and are guarded accordingly.
suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))
library(stringr)

if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(suppressWarnings(library(rxode2)))

  # Create a test session once and reuse across tests. MB_test_mksession()
  # initializes a session with a default model already attached.
  sess_res = suppressMessages(suppressWarnings(MB_test_mksession()))

  expect_true(sess_res[["isgood"]])

  # MB_test_catalog() writes files to disk, so we run it in a temp directory
  # to avoid polluting the working directory
  current_dir = getwd()
  on.exit( setwd(current_dir))

  setwd(tempdir())
    test_that("Building model catalog", {
      # MB_test_catalog() iterates through models in the nlmixr2lib catalog,
      # builds rxode2 objects from each, and verifies they compile correctly.
      # as_cran=FALSE tests all models (not just the CRAN-safe subset).
      state = sess_res$state
      mtres = suppressMessages(suppressWarnings(MB_test_catalog(state, as_cran=FALSE)))
      expect_true(mtres[["isgood"]])
    })
  setwd(current_dir)

  # ------------------------------------------------------------------
  # CRUD (Create, Read, Update, Delete) operations on MB model elements.
  # This mirrors the workflow in inst/test_apps/MB_funcs.R:
  #   1. Fetch state and model catalog
  #   2. Build an rxode2 object from a catalog entry
  #   3. Create a new element, attach the model, inspect components
  #   4. Generate reproducible code from the model
  #   5. Delete the element
  # ------------------------------------------------------------------
  test_that("MB element CRUD and model operations", {
    session = sess_res[["session"]]
    input   = sess_res[["input"]]

    # Initialize a fresh MB state from yaml configuration files
    FM_yaml_file  = system.file(package = "formods",  "templates", "formods.yaml")
    MOD_yaml_file = system.file(package = "ruminate", "templates", "MB.yaml")

    state = suppressMessages(suppressWarnings(
      MB_fetch_state(id              = "MB",
                     input           = input,
                     session         = session,
                     FM_yaml_file    = FM_yaml_file,
                     MOD_yaml_file   = MOD_yaml_file,
                     react_state     = NULL)))

    # MB_fetch_catalog() returns the available models from nlmixr2lib
    # with isgood status and a summary data frame of model metadata
    models = suppressMessages(suppressWarnings(MB_fetch_catalog(state)))
    expect_true(models[["isgood"]])
    expect_true(is.data.frame(models[["summary"]]))
    expect_true(nrow(models[["summary"]]) > 0)

    # Extract the first model's definition and build an rxode2 object.
    # mk_rx_obj() takes the model type and a list with fcn_def (the R
    # function text) and fcn_obj (the object name).
    fcn_def  = models[["summary"]][1, ][["Model"]]
    fcn_obj  = models[["summary"]][1, ][["Object"]]
    mdl_type = models[["summary"]][1, ][["Type"]]
    fcn_desc = models[["summary"]][1, ][["Description"]]

    mk_rx_res = suppressMessages(suppressWarnings(
      mk_rx_obj(type  = mdl_type,
                model = list(fcn_def = fcn_def,
                             fcn_obj = fcn_obj))))
    expect_true(is.list(mk_rx_res))

    # CREATE: Add a new empty model element and verify the count increases
    n_before = length(names(state[["MB"]][["elements"]]))
    state = suppressMessages(suppressWarnings(MB_new_element(state)))
    n_after = length(names(state[["MB"]][["elements"]]))
    expect_equal(n_after, n_before + 1)

    # READ: Fetch the newly created (now current) element
    element = MB_fetch_current_element(state)
    expect_true(is.list(element))

    # UPDATE: Attach the rxode2 model object to the element.
    # MB_update_model() stores the rx_obj, generates component tables,
    # and sets the element's isgood flag. reset=TRUE replaces any
    # existing model components.
    element = suppressMessages(suppressWarnings(
      MB_update_model(
        state       = state,
        session     = session,
        current_ele = element,
        rx_obj      = mk_rx_res[["capture"]][["rx_obj"]],
        note        = fcn_desc,
        reset       = TRUE)))
    expect_true(is.list(element))

    # Save the updated element back into the state (also updates checksums)
    state = suppressMessages(suppressWarnings(
      MB_set_current_element(state, element)))
    expect_true(is.list(state))

    # MB_fetch_component() retrieves the active component of the element,
    # which includes the model function definition (fcn_def), rxode2 object,
    # and any notes.
    component = suppressMessages(suppressWarnings(
      MB_fetch_component(state, element)))
    expect_true(is.list(component))
    expect_true("fcn_def" %in% names(component))

    # MB_fetch_appends() returns information about model layers/components
    # that have been appended to the element (used for layered model building)
    fares = suppressMessages(suppressWarnings(
      MB_fetch_appends(state, element)))
    expect_true(is.list(fares))

    # MB_build_code() generates the R code needed to reproduce the model.
    # It returns both model_code (for embedding in scripts) and
    # model_code_sa (standalone with all dependencies).
    gen_code = suppressMessages(suppressWarnings(
      MB_build_code(state        = state,
                    session      = session,
                    fcn_def      = component[["fcn_def"]],
                    time_scale   = element[["ui"]][["time_scale"]],
                    fcn_obj_name = "my_fcn_obj",
                    rx_obj_name  = "my_obj_name",
                    ts_obj_name  = "my_ts_name")))
    expect_true(is.list(gen_code))
    expect_true("model_code" %in% names(gen_code))

    # DELETE: Remove the current element and verify the count decreases.
    # If this was the last element, MB_del_current_element() will add
    # a new default element automatically.
    state = suppressMessages(suppressWarnings(MB_del_current_element(state)))
    n_del = length(names(state[["MB"]][["elements"]]))
    expect_equal(n_del, n_after - 1)
  })

  # ------------------------------------------------------------------
  # Utility functions: code generation, checksum updates, model
  # extraction, and preload state serialization.
  # ------------------------------------------------------------------
  test_that("MB utility functions", {
    state = sess_res[["state"]]

    # MB_fetch_code() returns the R code that reproduces all models
    # currently defined in the MB module
    code = suppressMessages(suppressWarnings(MB_fetch_code(state)))
    expect_true(is.character(code))

    # MB_update_checksum() recalculates the module-level checksum by
    # combining individual element checksums; downstream modules (like
    # CTS) use this to detect when models have changed
    state = suppressMessages(suppressWarnings(MB_update_checksum(state)))
    expect_true(!is.null(state[["MB"]][["checksum"]]))

    # MB_fetch_mdl() packages all valid models into a standardized list
    # for consumption by other modules (CTS uses this to get source models).
    # Returns isgood, hasmdl, and mdl (dict of model objects).
    mdl_res = suppressMessages(suppressWarnings(MB_fetch_mdl(state)))
    expect_true(is.list(mdl_res))
    expect_true("isgood" %in% names(mdl_res))
    expect_true("hasmdl" %in% names(mdl_res))
    expect_true("mdl"    %in% names(mdl_res))

    # MB_mk_preload() serializes the MB state into a yaml-compatible
    # list for session save/restore functionality
    pl_res = suppressMessages(suppressWarnings(MB_mk_preload(state)))
    expect_true(is.list(pl_res))
    expect_true("isgood"    %in% names(pl_res))
    expect_true("yaml_list" %in% names(pl_res))
  })

  # ------------------------------------------------------------------
  # Report generation: tests MB_append_report() with both xlsx (not
  # supported for MB, should return no report elements) and docx
  # (supported, should generate code for model documentation).
  # ------------------------------------------------------------------
  test_that("MB_append_report generates report code", {
    state = sess_res[["state"]]

    # xlsx is not a supported report type for MB, so hasrptele should be FALSE
    rpt = list(summary = list(), sheets = list())
    rpt_res = suppressMessages(suppressWarnings(
      MB_append_report(state,
                       rpt           = rpt,
                       rpttype       = "xlsx",
                       gen_code_only = TRUE)))
    expect_true(is.list(rpt_res))
    expect_true("isgood"    %in% names(rpt_res))
    expect_true("hasrptele" %in% names(rpt_res))

    # docx is supported for MB and should generate code that inserts
    # model function definitions into a Word document
    rpt_res_docx = suppressMessages(suppressWarnings(
      MB_append_report(state,
                       rpt           = rpt,
                       rpttype       = "docx",
                       gen_code_only = TRUE)))
    expect_true(is.list(rpt_res_docx))
    expect_true("code" %in% names(rpt_res_docx))
  })

  # ------------------------------------------------------------------
  # Detailed model structure: verifies that MB_fetch_mdl() returns
  # properly structured model objects with the expected fields.
  # ------------------------------------------------------------------
  test_that("MB_fetch_mdl returns model with correct structure", {
    state = sess_res[["state"]]
    mdl_res = suppressMessages(suppressWarnings(MB_fetch_mdl(state)))

    # When models are present, each entry in the mdl list should contain
    # the model label, the compiled rxode2 object, and the function
    # definition text
    if(mdl_res[["hasmdl"]]){
      first_mdl_name = names(mdl_res[["mdl"]])[1]
      first_mdl = mdl_res[["mdl"]][[first_mdl_name]]
      expect_true("label"   %in% names(first_mdl))
      expect_true("rx_obj"  %in% names(first_mdl))
      expect_true("fcn_def" %in% names(first_mdl))
    }
  })
}
