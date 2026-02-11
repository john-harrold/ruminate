skip_on_cran()
skip_if(nzchar(Sys.getenv("R_COVR")), "Not running shinytest2 under covr")
skip_if_not_installed("shinytest2")
skip_if_not_installed("shinydashboard")
skip_if_not_installed("prompter")
library(shinytest2)

# ---------------------------------------------------------------------------
# Helper: build a preloaded ruminate app directory for NCA testing.
# Uses the ruminate.R template (NCA module only, no rxode2 dependency).
# ---------------------------------------------------------------------------
build_ruminate_nca_app_dir = function() {
  app_dir = file.path(tempdir(), paste0("ruminate_nca_", format(Sys.time(), "%H%M%S")))

  # Preload sources: formods modules (ASM, UD, DM, DW) plus NCA
  preload_sources = c(
    system.file(package = "formods",  "preload", "ASM_preload.yaml"),
    system.file(package = "formods",  "preload", "UD_preload.yaml"),
    system.file(package = "formods",  "preload", "DM_preload.yaml"),
    system.file(package = "formods",  "preload", "DW_preload.yaml"),
    system.file(package = "ruminate", "preload", "NCA_preload_minimal.yaml")
  )

  # Create the preload directory with config files and test data
  formods::mk_preload_dir(
    directory = app_dir,
    preload   = preload_sources,
    mod_yaml  = c(
      system.file(package = "formods",  "templates", "formods.yaml"),
      system.file(package = "formods",  "templates", "ASM.yaml"),
      system.file(package = "formods",  "templates", "UD.yaml"),
      system.file(package = "formods",  "templates", "DM.yaml"),
      system.file(package = "formods",  "templates", "DW.yaml"),
      system.file(package = "formods",  "templates", "FG.yaml"),
      system.file(package = "ruminate", "templates", "NCA.yaml")),
    include = list(
      UD = list(
        from = system.file(package = "formods", "test_data", "TEST_DATA.xlsx"),
        to   = "TEST_DATA.xlsx"),
      DM = list(
        path = file.path("data", "DM"),
        from = system.file(package = "formods", "test_data", "TEST_DATA.xlsx"),
        to   = "TEST_DATA.xlsx")
    )
  )

  # Read the ruminate.R template and strip shinybusy calls
  src   = system.file("templates", "ruminate.R", package = "ruminate")
  lines = readLines(src)
  lines = lines[!grepl("shinybusy::", lines)]
  writeLines(lines, file.path(app_dir, "app.R"))

  app_dir
}


# ---------------------------------------------------------------------------
# Helper: build a preloaded ruminate app directory for MB and CTS testing.
# Uses the ruminate_devel.R template (all modules including MB and CTS).
# Requires rxode2 family packages.
# ---------------------------------------------------------------------------
build_ruminate_devel_app_dir = function() {
  app_dir = file.path(tempdir(), paste0("ruminate_dev_", format(Sys.time(), "%H%M%S")))

  # Preload sources: ASM + MB + CTS (minimal)
  # MB and CTS do not depend on UD/DM/DW/FG data
  preload_sources = c(
    system.file(package = "formods",  "preload", "ASM_preload.yaml"),
    system.file(package = "ruminate", "preload", "MB_preload.yaml"),
    system.file(package = "ruminate", "preload", "CTS_preload_minimal.yaml")
  )

  # Create the preload directory with all module config files
  formods::mk_preload_dir(
    directory = app_dir,
    preload   = preload_sources,
    mod_yaml  = c(
      system.file(package = "formods",  "templates", "formods.yaml"),
      system.file(package = "formods",  "templates", "ASM.yaml"),
      system.file(package = "formods",  "templates", "UD.yaml"),
      system.file(package = "formods",  "templates", "DM.yaml"),
      system.file(package = "formods",  "templates", "DW.yaml"),
      system.file(package = "formods",  "templates", "FG.yaml"),
      system.file(package = "ruminate", "templates", "NCA.yaml"),
      system.file(package = "ruminate", "templates", "MB.yaml"),
      system.file(package = "ruminate", "templates", "CTS.yaml"))
  )

  # Read the ruminate_devel.R template and strip shinybusy calls
  src   = system.file("templates", "ruminate_devel.R", package = "ruminate")
  lines = readLines(src)
  lines = lines[!grepl("shinybusy::", lines)]
  writeLines(lines, file.path(app_dir, "app.R"))

  app_dir
}


# ---------------------------------------------------------------------------
# Helper: navigate to a shinydashboard tab by its tabName
# ---------------------------------------------------------------------------
goto_tab = function(app, tab_name) {
  app$run_js(sprintf("$('a[data-value=\"%s\"]').click()", tab_name))
  tryCatch(
    app$wait_for_idle(timeout = 20000),
    error = function(e) Sys.sleep(3)
  )
  Sys.sleep(1)
  tryCatch(
    app$wait_for_idle(timeout = 20000),
    error = function(e) Sys.sleep(3)
  )
}


# ---------------------------------------------------------------------------
# Helper: poll for an aceEditor input value to become non-empty.
# Module compact UIs have deeply nested renderUI calls that require
# multiple server round-trips before the aceEditor is populated.
# ---------------------------------------------------------------------------
wait_for_ace_value = function(app, input_id, timeout_s = 60) {
  val = NULL
  for (i in seq_len(timeout_s / 2)) {
    val = tryCatch(
      app$get_value(input = input_id),
      error = function(e) NULL
    )
    if (!is.null(val) && nzchar(val)) return(val)
    Sys.sleep(2)
    tryCatch(
      app$wait_for_idle(timeout = 5000),
      error = function(e) Sys.sleep(2)
    )
  }
  val
}


# ===========================================================================
# NCA module test (no rxode2 dependency)
# ===========================================================================
test_that("NCA -- shinytest2 preloaded analysis code verification", {
  app_dir = build_ruminate_nca_app_dir()
  app = AppDriver$new(
    app_dir      = app_dir,
    name         = "ruminate_nca",
    height       = 800,
    width        = 1200,
    timeout      = 60000,
    load_timeout = 120000
  )
  on.exit(app$stop(), add = TRUE)

  # Navigate to NCA tab so the compact module UI renders.
  goto_tab(app, "nca")

  # Wait for the NCA code editor to be populated.
  # The preload runs the NCA analysis during app initialization,
  # so the code should already contain the full analysis code.
  code_val = wait_for_ace_value(app, "NCA-ui_nca_code", timeout_s = 60)
  expect_true(!is.null(code_val) && nzchar(code_val),
              info = "Expected NCA code editor to have content after load")

  # Verify the generated code contains key NCA analysis patterns
  expect_true(grepl("PKNCA", code_val),
              info = paste("Expected PKNCA in NCA code, got:", substr(code_val, 1, 200)))
  expect_true(grepl("pk\\.nca", code_val),
              info = paste("Expected pk.nca call in NCA code, got:", substr(code_val, 1, 200)))
  expect_true(grepl("PKNCAdata", code_val),
              info = paste("Expected PKNCAdata in NCA code, got:", substr(code_val, 1, 200)))
  expect_true(grepl("PKNCAconc", code_val),
              info = paste("Expected PKNCAconc in NCA code, got:", substr(code_val, 1, 200)))
  expect_true(grepl("mk_figure_ind_obs", code_val),
              info = paste("Expected mk_figure_ind_obs in NCA code, got:", substr(code_val, 1, 200)))
  expect_true(grepl("mk_table_nca_params", code_val),
              info = paste("Expected mk_table_nca_params in NCA code, got:", substr(code_val, 1, 200)))
})


# ===========================================================================
# MB and CTS module tests (requires rxode2 family)
# Combined into a single test to share one AppDriver instance.
# CTS depends on MB model objects, so visiting the model tab first
# is required. Using one app avoids sequential startup issues.
# ===========================================================================
test_that("MB and CTS -- shinytest2 preloaded code verification", {
  skip_if_not_installed("rxode2")
  skip_if(Sys.getenv("ruminate_rxfamily_found") != "TRUE",
          "rxode2 family not found")

  app_dir = build_ruminate_devel_app_dir()
  app = AppDriver$new(
    app_dir      = app_dir,
    name         = "ruminate_mb_cts",
    height       = 800,
    width        = 1200,
    timeout      = 60000,
    load_timeout = 180000
  )
  on.exit(app$stop(), add = TRUE)

  # --- MB assertions ---
  # Navigate to model tab so the MB compact UI renders
  goto_tab(app, "model")

  # Wait for the model preview editor to be populated.
  # The MB preload includes a one-compartment PK model definition.
  model_val = wait_for_ace_value(app, "MB-ui_mb_model", timeout_s = 90)
  expect_true(!is.null(model_val) && nzchar(model_val),
              info = "Expected MB model editor to have content after load")

  # Verify the model code contains expected rxode2 model patterns
  expect_true(grepl("model", model_val),
              info = paste("Expected 'model' in MB model code, got:", substr(model_val, 1, 200)))
  expect_true(grepl("ini", model_val),
              info = paste("Expected 'ini' in MB model code, got:", substr(model_val, 1, 200)))

  # Wait for the code generation editor to be populated
  mb_code_val = wait_for_ace_value(app, "MB-ui_mb_code", timeout_s = 60)
  expect_true(!is.null(mb_code_val) && nzchar(mb_code_val),
              info = "Expected MB code editor to have content after load")

  # --- CTS assertions ---
  # Navigate to trials tab; MB model tab was already visited above
  # so the CTS compact UI can access MB model objects.
  goto_tab(app, "trials")

  # Wait for the CTS code editor to be populated.
  # The preload configures a minimal single-dose simulation
  # that sources a model from the MB module.
  cts_code_val = wait_for_ace_value(app, "CTS-ui_cts_code", timeout_s = 90)
  expect_true(!is.null(cts_code_val) && nzchar(cts_code_val),
              info = "Expected CTS code editor to have content after load")

  # Verify the generated code contains simulation-related patterns
  expect_true(grepl("rxSolve|rxode2|simulate_rules", cts_code_val),
              info = paste("Expected rxode2/simulation code in CTS, got:", substr(cts_code_val, 1, 200)))
})
