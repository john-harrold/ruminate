# Testing the Non-Compartmental Analysis (NCA) module exported functions.
# The NCA module wraps PKNCA to provide a Shiny interface for NCA analyses.
# Tests are split into:
#   - Session-dependent tests (inside readxl guard) that require a full
#     NCA test session with loaded datasets
#   - Standalone function tests (outside guards) that test utility
#     functions with no external dependencies
library(formods)
library(ruminate)
library(stringr)

# Create a shared test session for use across multiple test blocks.
# NCA_test_mksession() loads test data, configures column mappings,
# runs NCA analyses, and returns a fully populated state object.
sess_res = suppressMessages(suppressWarnings(NCA_test_mksession()))

expect_true(sess_res[["isgood"]])

# readxl is needed for the test data (Excel files) used in these tests
if(formods::is_installed("readxl")){
  library(readxl)

  # ------------------------------------------------------------------
  # Dose record extraction: tests dose_records_builder() which creates
  # the dosing dataframe needed by PKNCA from either column-based or
  # row-based dose specifications.
  # ------------------------------------------------------------------
  test_that("Extract dose records", {

  # Load the test dataset and filter to multiple-dose, 3 mg concentration data
  data_file =  system.file(package="formods","test_data","TEST_DATA.xlsx")
  DS_cols = readxl::read_excel(path=data_file, sheet="DATA")        |>
    dplyr::filter(EVID == 0)                                |>
    dplyr::filter(DOSE %in% c(3))                           |>
    dplyr::filter(str_detect(string=Cohort, "^MD"))         |>
    dplyr::filter(CMT == "C_ng_ml")

  # Column mapping tells dose_records_builder() which columns contain
  # the subject ID, dose amount, concentration, time, etc.
  col_map = list(
    col_id = c("ID"),
    col_dose = c("DOSE"),
    col_conc = c("DV"),
    col_dur = NULL,
    col_analyte = c("CMT"),
    col_route = c("ROUTE"),
    col_time = c("TIME_DY"),
    col_ntime = c("NTIME_DY"),
    col_group = NULL,
    col_evid = c("EVID"),
    col_cycle = c("DOSE_NUM")
  )

  # dose_from="cols": doses are inferred from columns in the observation dataset
  drb_res = dose_records_builder(
    NCA_DS     = DS_cols,
    col_map    = col_map,
    dose_from  = "cols")

  expect_true(drb_res[["isgood"]])

  # dose_from="rows": doses come from dosing event rows (EVID != 0) in the dataset
  DS_rows = readxl::read_excel(path=data_file, sheet="DATA")        |>
    dplyr::filter(DOSE %in% c(3))                                   |>
    dplyr::filter(str_detect(string=Cohort, "^MD"))                 |>
    dplyr::filter(CMT %in% c("Ac", "C_ng_ml"))

  drb_res = dose_records_builder(
    NCA_DS     = DS_rows,
    col_map    = col_map,
    dose_from  = "rows")

  expect_true(drb_res[["isgood"]])
  })

  # ------------------------------------------------------------------
  # Verify that each analysis in the preloaded test session completed
  # successfully (isgood=TRUE).
  # ------------------------------------------------------------------
  test_that("Test NCA analyses in test session", {
    state = sess_res$state
    # Walk through every analysis stored in the session and check its status
    for(aname in names(state[["NCA"]][["anas"]])){
      expect_true(state[["NCA"]][["anas"]][[aname]][["isgood"]])
    }
  })

  # ------------------------------------------------------------------
  # CRUD (Create, Read, Update, Delete) operations on NCA analyses.
  # This mirrors the workflow in inst/test_apps/NCA_funcs.R:
  #   1. Fetch the current analysis
  #   2. Create a new analysis, verify count increases
  #   3. Switch between analyses
  #   4. Process (validate) the analysis
  #   5. Fetch associated datasets
  #   6. Reset manual flags and add NCA intervals
  # ------------------------------------------------------------------
  test_that("NCA analysis CRUD and navigation", {
    state = sess_res$state

    # READ: Fetch the current active analysis, which should have
    # an id (internal identifier) and key (display name)
    current_ana = NCA_fetch_current_ana(state)
    expect_true(is.list(current_ana))
    expect_true("id"  %in% names(current_ana))
    expect_true("key" %in% names(current_ana))

    # Save the original ID so we can switch back later
    original_id = current_ana[["id"]]

    # CREATE: Add a new (empty) analysis and verify the count increases
    n_before = length(names(state[["NCA"]][["anas"]]))
    state = suppressMessages(suppressWarnings(NCA_new_ana(state)))
    n_after = length(names(state[["NCA"]][["anas"]]))
    expect_equal(n_after, n_before + 1)

    # NAVIGATE: Switch back to the original analysis using NCA_mkactive_ana()
    state = suppressMessages(suppressWarnings(
      NCA_mkactive_ana(state, original_id)))
    current_ana = NCA_fetch_current_ana(state)
    expect_equal(current_ana[["id"]], original_id)

    # UPDATE: Roundtrip test — set the current analysis and re-fetch it
    state = suppressMessages(suppressWarnings(
      NCA_set_current_ana(state, current_ana)))
    current_ana_2 = NCA_fetch_current_ana(state)
    expect_equal(current_ana_2[["id"]], original_id)

    # NCA_process_current_ana() validates the analysis configuration:
    # checks column mappings, NCA options, data uniqueness, etc.
    # Returns the current_ana list directly (not a wrapper) with
    # isgood and msgs fields updated.
    pca_res = suppressMessages(suppressWarnings(
      NCA_process_current_ana(state)))
    expect_true(is.list(pca_res))
    expect_true("isgood" %in% names(pca_res))
    expect_true("id"     %in% names(pca_res))

    # NCA_fetch_ana_ds() retrieves the source dataset for the given analysis
    ana_ds = suppressMessages(suppressWarnings(
      NCA_fetch_ana_ds(state, current_ana)))
    expect_true(is.data.frame(ana_ds))

    # NCA_fetch_ds() collects all datasets produced by the NCA module
    # (raw results, individual parameters, etc.) across all valid analyses
    ds_res = suppressMessages(suppressWarnings(NCA_fetch_ds(state)))
    expect_true(is.list(ds_res))
    expect_true("isgood" %in% names(ds_res))
    expect_true("hasds"  %in% names(ds_res))

    # NCA_reset_manual_flags() clears any manually applied data point
    # flags (censor, half-life include/exclude) from the current analysis
    state = suppressMessages(suppressWarnings(
      NCA_reset_manual_flags(state)))
    current_ana = NCA_fetch_current_ana(state)
    expect_equal(nrow(current_ana[["manual_ds_flags"]]), 0)

    # NCA_add_int() adds an NCA analysis interval (start/stop times
    # and which parameters to compute). Here we add an interval from
    # 0 to Inf for AUCinf and half-life.
    state = suppressMessages(suppressWarnings(
      NCA_mkactive_ana(state, original_id)))
    state = suppressMessages(suppressWarnings(
      NCA_add_int(state,
                  interval_start  = 0,
                  interval_stop   = "Inf",
                  nca_parameters  = c("aucinf.obs", "half.life"))))
    current_ana = NCA_fetch_current_ana(state)
    expect_true(!is.null(current_ana[["intervals"]]))
    expect_true(nrow(current_ana[["intervals"]]) >= 1)
  })

  # ------------------------------------------------------------------
  # Utility functions: code generation, checksum updates, and preload
  # state serialization for session save/restore.
  # ------------------------------------------------------------------
  test_that("NCA utility functions", {
    state = sess_res$state

    # NCA_fetch_code() returns the R code that reproduces all valid
    # NCA analyses in the module
    code = suppressMessages(suppressWarnings(NCA_fetch_code(state)))
    expect_true(is.character(code))

    # NCA_update_checksum() recalculates the module checksum used by
    # downstream modules (e.g., reporting) to detect changes
    state = suppressMessages(suppressWarnings(NCA_update_checksum(state)))
    expect_true(!is.null(state[["NCA"]][["checksum"]]))

    # NCA_mk_preload() serializes the NCA state for session save/restore
    pl_res = suppressMessages(suppressWarnings(NCA_mk_preload(state)))
    expect_true(is.list(pl_res))
    expect_true("isgood"    %in% names(pl_res))
    expect_true("yaml_list" %in% names(pl_res))
  })

  # ------------------------------------------------------------------
  # NCA_fetch_current_obj() retrieves the currently selected figure or
  # table object from the active analysis for display in the Shiny UI.
  # ------------------------------------------------------------------
  test_that("NCA_fetch_current_obj returns figures and tables", {
    state = sess_res$state

    # Fetch the active figure (individual observation plot)
    fig_res = suppressMessages(suppressWarnings(
      NCA_fetch_current_obj(state, "figure")))
    expect_true(is.list(fig_res))

    # Fetch the active table (individual observations or parameters)
    tab_res = suppressMessages(suppressWarnings(
      NCA_fetch_current_obj(state, "table")))
    expect_true(is.list(tab_res))
  })

  # ------------------------------------------------------------------
  # Report generation: verifies NCA_append_report() in code-only mode
  # for xlsx output. This generates the code to populate Excel sheets
  # with NCA results without actually writing a file.
  # ------------------------------------------------------------------
  test_that("NCA_append_report generates report code", {
    state = sess_res$state

    # gen_code_only=TRUE generates code without executing it
    rpt = list(summary = list(), sheets = list())
    rpt_res = suppressMessages(suppressWarnings(
      NCA_append_report(state,
                        rpt           = rpt,
                        rpttype       = "xlsx",
                        gen_code_only = TRUE)))
    expect_true(is.list(rpt_res))
    expect_true("isgood"    %in% names(rpt_res))
    expect_true("hasrptele" %in% names(rpt_res))
    expect_true("code"      %in% names(rpt_res))
  })

  # ------------------------------------------------------------------
  # Edge case: NCA_fetch_ana_ds() should gracefully return NULL when
  # the analysis has no dataset view assigned (ana_dsview is NULL).
  # ------------------------------------------------------------------
  test_that("NCA_fetch_ana_ds with NULL dsview returns NULL", {
    state = sess_res$state
    # Construct a fake analysis with NULL dsview to trigger the guard clause
    fake_ana = list(ana_dsview = NULL)
    ds = suppressMessages(suppressWarnings(
      NCA_fetch_ana_ds(state, fake_ana)))
    expect_null(ds)
  })
}

# ====================================================================
# Standalone functions: these do not require readxl, rxode2, or a
# test session. They test NCA utility functions that work with simple
# inputs or read from yaml configuration files.
# ====================================================================

# ------------------------------------------------------------------
# NCA_fetch_data_format() reads the NCA.yaml config and returns a
# data frame describing the expected data columns and their roles.
# ------------------------------------------------------------------
test_that("NCA_fetch_data_format returns a data frame", {
  df_res = suppressMessages(suppressWarnings(NCA_fetch_data_format()))
  expect_true(is.data.frame(df_res))
  expect_true(nrow(df_res) > 0)
})

# ------------------------------------------------------------------
# NCA_fetch_PKNCA_meta() queries PKNCA::get.interval.cols() and
# returns a list containing a "parameters" data frame with NCA
# parameter names, units, and descriptions.
# ------------------------------------------------------------------
test_that("NCA_fetch_PKNCA_meta returns metadata", {
  pknca_meta = suppressMessages(suppressWarnings(NCA_fetch_PKNCA_meta()))
  expect_true(is.list(pknca_meta))
  expect_true("parameters" %in% names(pknca_meta))
  expect_true(is.data.frame(pknca_meta[["parameters"]]))
})

# ------------------------------------------------------------------
# NCA_fetch_np_meta() reads NCA parameter metadata from the yaml
# config, returning display names in plain text, markdown, and LaTeX.
# ------------------------------------------------------------------
test_that("NCA_fetch_np_meta returns parameter meta information", {
  np_meta = suppressMessages(suppressWarnings(NCA_fetch_np_meta()))
  expect_true(is.list(np_meta))
  expect_true("choices" %in% names(np_meta))
  expect_true("summary" %in% names(np_meta))
  expect_true(is.data.frame(np_meta[["summary"]]))
})

# ------------------------------------------------------------------
# NCA_find_col() detects dataset columns using a priority hierarchy:
#   1. curr_ana (explicit column from saved analysis)
#   2. curr_ui  (current UI selection)
#   3. patterns (regex patterns from yaml config)
#   4. Fallback to first column (unless null_ok=TRUE)
# ------------------------------------------------------------------
test_that("NCA_find_col detects columns by pattern", {
  test_cols = c("SUBJ", "TIME", "DV", "DOSE", "ROUTE", "CMT")

  # Pattern match: "^SUBJ$" matches the first column exactly
  res_id = NCA_find_col(patterns = c("^SUBJ$", "^ID$"), dscols = test_cols)
  expect_equal(res_id, "SUBJ")

  # Direct match via curr_ana: if the column exists in the dataset, use it
  res_direct = NCA_find_col(curr_ana = "DV", dscols = test_cols)
  expect_equal(res_direct, "DV")

  # Fallback: when no pattern matches and null_ok=FALSE, return the first column
  res_fallback = NCA_find_col(patterns = c("^NONEXISTENT$"), dscols = test_cols)
  expect_equal(res_fallback, "SUBJ")

  # null_ok=TRUE: when no pattern matches, return NULL instead of falling back
  res_null = NCA_find_col(patterns = c("^NONEXISTENT$"), dscols = test_cols, null_ok = TRUE)
  expect_null(res_null)
})

# ------------------------------------------------------------------
# flag_nca_ds() adds flagging columns to a dataset for NCA analysis.
# It automatically flags BLQ (DV==0) and NS (DV==NA) records, and
# applies manual flags (censor, half-life exclude/include) from
# ds_flags. The output columns (rmnt_key, rmnt_flag, rmnt_cens, etc.)
# are used by PKNCA and the NCA plotting/reporting functions.
# ------------------------------------------------------------------
test_that("flag_nca_ds applies flags to dataset", {
  # Small test dataset: row 1 has DV=0 (BLQ), rows 2-3 are normal
  test_ds = data.frame(
    ID    = c(1, 1, 1),
    TIME  = c(0, 1, 2),
    DV    = c(0, 10, 5),
    DOSE  = c(100, 100, 100),
    ROUTE = c("iv", "iv", "iv"),
    CMT   = c("C_ng_ml", "C_ng_ml", "C_ng_ml")
  )

  # Flag map defines all possible flags with their display properties.
  # "manual=yes" flags can be applied by the user in the Shiny interface.
  flag_map = list(
    obs = list(manual = "no",  color = "black", sn = "OBS",
               description = "Observation", notes = "normal observation"),
    blq = list(manual = "no",  color = "blue",  sn = "BLQ",
               description = "BLQ", notes = "below limit of quantification"),
    ns  = list(manual = "no",  color = "black", sn = "NS",
               description = "Not Sampled", notes = "not sampled"),
    censor = list(manual = "yes", color = "orange", sn = "C",
                  description = "Censored", notes = "censored from analysis"),
    hlex = list(manual = "yes", color = "yellow", sn = "HX",
                description = "Exclude Half-life", notes = "exclude from half-life"),
    hlin = list(manual = "yes", color = "green",  sn = "HI",
                description = "Specify Half-life", notes = "specify for half-life")
  )

  col_map = list(
    col_id      = "ID",
    col_conc    = "DV",
    col_time    = "TIME",
    col_dose    = "DOSE",
    col_route   = "ROUTE",
    col_analyte = "CMT"
  )

  # Manually flag record 2 as censored
  ds_flags = dplyr::tribble(
    ~key,     ~flag,    ~note,
    "rec_2",  "censor", "test flag"
  )

  flagged = suppressMessages(suppressWarnings(
    flag_nca_ds(DS = test_ds, flag_map = flag_map, col_map = col_map,
                ds_flags = ds_flags)))
  expect_true(is.data.frame(flagged))
  expect_true("rmnt_key"  %in% names(flagged))
  expect_true("rmnt_flag" %in% names(flagged))
  # Row 1 (DV=0) should be auto-flagged as BLQ
  expect_equal(flagged[["rmnt_flag"]][1], "blq")
  # Row 2 should be manually flagged as censored
  expect_equal(flagged[["rmnt_flag"]][2], "censor")
})

# ------------------------------------------------------------------
# apply_route_map() standardizes route column values using regex
# patterns. For example, "iv" becomes "intravascular" and "sc"
# becomes "extravascular" based on the supplied route_map.
# ------------------------------------------------------------------
test_that("apply_route_map maps route values", {
  test_ds = data.frame(
    ID    = c(1, 2),
    ROUTE = c("iv", "sc")
  )

  # Each entry maps a standard route name to regex patterns that match it
  route_map = list(
    intravascular = c("^(?i)iv$"),
    extravascular = c("^(?i)sc$")
  )

  mapped_ds = suppressMessages(suppressWarnings(
    apply_route_map(route_map = route_map, route_col = "ROUTE", DS = test_ds)))
  expect_true(is.data.frame(mapped_ds))
  expect_equal(mapped_ds[["ROUTE"]][1], "intravascular")
  expect_equal(mapped_ds[["ROUTE"]][2], "extravascular")
})

# ------------------------------------------------------------------
# Testing NCA with NA values in the dataset. This uses a preload
# yaml file and a CSV with NA concentration values to verify that
# the NCA module handles missing data gracefully.
# ------------------------------------------------------------------
test_that("Testing NA values in NCA analysis", {

  # mk_preload_dir() creates a temporary directory structure with
  # config files and test data for preloading a session
  pldir = file.path(tempdir(), "preload_test")
  mpd_res = mk_preload_dir(
    directory = pldir,
    preload   = system.file(package="ruminate", "test_preload", "test_nca_na.yaml"),
    mod_yaml  = c(
      system.file(package="formods",  "templates", "formods.yaml"),
      system.file(package="formods",  "templates", "ASM.yaml"),
      system.file(package="formods",  "templates", "UD.yaml"),
      system.file(package="formods",  "templates", "DM.yaml"),
      system.file(package="formods",  "templates", "DW.yaml"),
      system.file(package="formods",  "templates", "FG.yaml"),
      system.file(package="ruminate", "templates", "NCA.yaml"),
      system.file(package="ruminate", "templates", "MB.yaml"),
      system.file(package="ruminate", "templates", "CTS.yaml")),
    include = list(
      UD = list(
        from = system.file(package="ruminate", "test_data", "test_nca_na.csv"),
        to   = "test_nca_na.csv" )
    )
  )

  # FM_app_preload() replays the preload yaml to restore a session
  # state, including loading data and running analyses
  old_dir = getwd()
  setwd(pldir)
  on.exit(setwd(old_dir))
  apl_res = suppressMessages(suppressWarnings(FM_app_preload(session=list(), sources="preload.yaml")))
  testthat::expect_true(apl_res[["isgood"]])

  # Verify the NCA analysis completed despite NA values in the data
  tmp_state = apl_res[["all_sess_res"]][["NCA"]][["state"]]
  current_ana = NCA_fetch_current_ana(tmp_state)
  testthat::expect_true(current_ana[["isgood"]])

  setwd(old_dir)
})
