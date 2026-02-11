# Testing the simulate_rules() engine and related helper functions.
# simulate_rules() is the core rule-based clinical trial simulation engine
# that drives the CTS module. It takes an rxode2 model object, a set of
# subjects, evaluation times, and a list of conditional rules (dosing,
# state-reset, manual) and produces time-course simulation output. All
# tests require the rxode2 family of packages and are guarded accordingly.
suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))

if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(suppressWarnings(library(rxode2)))
  suppressMessages(suppressWarnings(library(ggplot2)))

  # Define a simple one-compartment PK model with inter-individual
  # variability on clearance. This model has two states (depot, central),
  # an oral absorption rate (Ka), clearance (CL), and volume (V). The
  # output variable Cc (central concentration) is used throughout testing.
  my_model = function() {
    description <- "One compartment PK model with linear clearance"
    ini({
      lka <- 0.45
      label("Absorption rate (Ka)")
      lcl <- 1
      label("Clearance (CL)")
      lvc <- 3.45
      label("Central volume of distribution (V)")
      propSd <- c(0, 0.5)
      label("Proportional residual error (fraction)")
      etalcl ~ 0.1
    })
    model({
      ka <- exp(lka)
      cl <- exp(lcl + etalcl)
      vc <- exp(lvc)
      kel <- cl/vc
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
      Cc <- central/vc
      Cc ~ prop(propSd)
    })
  }

  # Build the rxode2 object once and reuse across all tests to avoid
  # repeated compilation overhead
  object = suppressMessages(suppressWarnings(rxode2::rxode(my_model)))

  # ------------------------------------------------------------------
  # Basic simulation: verifies the full pipeline of creating subjects,
  # defining dosing rules, and running simulate_rules(). The rule uses
  # condition="TRUE" so it always fires, dosing 1 unit into central at

  # time 0. We verify the return structure contains isgood, simall
  # (the combined time-course data frame), and ev_history (the event
  # log tracking all rule-triggered actions).
  # ------------------------------------------------------------------
  test_that("simulate_rules basic simulation", {
    set.seed(8675309)
    rxode2::rxSetSeed(8675309)

    # mk_subjects() generates a subject-level data frame from the model's
    # parameter estimates, sampling inter-individual variability terms.
    # Returns a list with isgood and a subjects data frame.
    subs = suppressMessages(suppressWarnings(
      mk_subjects(object = object, nsub = 2)))
    expect_true(is.list(subs))
    expect_true("subjects" %in% names(subs))
    expect_true(is.data.frame(subs[["subjects"]]))

    # Define a simple dosing rule: always dose 1 unit into central at t=0.
    # Rules are lists keyed by rule name. Each rule has a condition string
    # (evaluated as R code) and an action list specifying the type (dose,
    # state_reset, or manual), target state, dose values, times, and
    # infusion durations.
    rules = list(
      dose = list(
        condition = "TRUE",
        action    = list(
          type      = "dose",
          state     = "central",
          values    = "c(1)",
          times     = "c(0)",
          durations = "c(0)")
      )
    )

    # Run the simulation with a single evaluation time (t=0) and output
    # times spanning 0 to 56 hours in 1-hour increments
    eval_times   = 0
    output_times = seq(0, 56, 1)

    simres = suppressMessages(suppressWarnings(
      simulate_rules(
        object       = object,
        subjects     = subs[["subjects"]],
        eval_times   = eval_times,
        output_times = output_times,
        rules        = rules)))

    # Verify the simulation completed successfully and returned all
    # expected components
    expect_true(is.list(simres))
    expect_true("isgood" %in% names(simres))
    expect_true(simres[["isgood"]])
    expect_true("simall" %in% names(simres))
    expect_true(is.data.frame(simres[["simall"]]))
    expect_true(nrow(simres[["simall"]]) > 0)
    expect_true("ev_history" %in% names(simres))
  })

  # ------------------------------------------------------------------
  # Plotting: verifies that plot_sr_tc() (timecourse) and plot_sr_ev()
  # (event) can generate ggplot figures from simulation output. Both
  # functions return a list with isgood and a fig (ggplot object).
  # ------------------------------------------------------------------
  test_that("plot_sr_tc and plot_sr_ev produce plots", {
    set.seed(8675309)
    rxode2::rxSetSeed(8675309)

    # Run a quick simulation to generate data for plotting
    subs = suppressMessages(suppressWarnings(
      mk_subjects(object = object, nsub = 2)))

    rules = list(
      dose = list(
        condition = "TRUE",
        action    = list(
          type      = "dose",
          state     = "central",
          values    = "c(1)",
          times     = "c(0)",
          durations = "c(0)")
      )
    )

    simres = suppressMessages(suppressWarnings(
      simulate_rules(
        object       = object,
        subjects     = subs[["subjects"]],
        eval_times   = 0,
        output_times = seq(0, 56, 1),
        rules        = rules)))

    # plot_sr_tc() creates a timecourse plot showing the specified
    # dependent variable columns (dvcols) over time for each subject
    psim = suppressMessages(suppressWarnings(
      plot_sr_tc(sro = simres, dvcols = "Cc")))
    expect_true(is.list(psim))
    expect_true("isgood" %in% names(psim))
    expect_true("fig"    %in% names(psim))

    # plot_sr_ev() creates an event plot showing dosing and other events
    # over time. ylog=FALSE uses a linear y-axis scale.
    pev = suppressMessages(suppressWarnings(
      plot_sr_ev(sro = simres, ylog = FALSE)))
    expect_true(is.list(pev))
    expect_true("isgood" %in% names(pev))
    expect_true("fig"    %in% names(pev))
  })

  # ------------------------------------------------------------------
  # Error handling: both plot functions should return isgood=FALSE when
  # given a failed simulation result rather than throwing an error.
  # ------------------------------------------------------------------
  test_that("plot functions handle bad input gracefully", {
    # Timecourse plot with a simulation result that reported failure
    psim_bad = suppressMessages(suppressWarnings(
      plot_sr_tc(sro = list(isgood = FALSE))))
    expect_true(is.list(psim_bad))
    expect_false(psim_bad[["isgood"]])

    # Event plot with a simulation result that reported failure
    pev_bad = suppressMessages(suppressWarnings(
      plot_sr_ev(sro = list(isgood = FALSE))))
    expect_true(is.list(pev_bad))
    expect_false(pev_bad[["isgood"]])
  })

  # ------------------------------------------------------------------
  # Model conversion: rx2other() translates an rxode2 model to other
  # formats (e.g., NONMEM control stream). Returns a list with isgood
  # and files (list of generated file contents).
  # ------------------------------------------------------------------
  test_that("rx2other converts to NONMEM format", {
    rx2o_res = suppressMessages(suppressWarnings(
      rx2other(object = object, out_type = "nonmem")))
    expect_true(is.list(rx2o_res))
    expect_true("isgood" %in% names(rx2o_res))
    expect_true("files"  %in% names(rx2o_res))
  })

  # ------------------------------------------------------------------
  # Subject generation with covariates: mk_subjects() can accept an
  # explicit covariates list that defines how to sample subject-level
  # covariates. Here we add a log-normally distributed body weight (WT)
  # with mean 70 kg and CV 15%. We verify the covariate column appears
  # in the subjects data frame with the correct number of rows.
  # ------------------------------------------------------------------
  test_that("mk_subjects with covariates", {
    covs = list(
      WT = list(type     = "continuous",
                sampling = "log-normal",
                values   = c(70, .15))
    )

    subs = suppressMessages(suppressWarnings(
      mk_subjects(object = object, nsub = 3, covs = covs)))
    expect_true(subs[["isgood"]])
    expect_true(is.data.frame(subs[["subjects"]]))
    expect_true("WT" %in% names(subs[["subjects"]]))
    expect_equal(nrow(subs[["subjects"]]), 3)
  })

  # ------------------------------------------------------------------
  # Model inspection: fetch_rxinfo() extracts structural information
  # from an rxode2 model object, including state names (compartments),
  # parameters, and output variables. The elements list should contain
  # a "states" entry listing the model's ODE compartments.
  # ------------------------------------------------------------------
  test_that("fetch_rxinfo returns model details", {
    rxinfo = suppressMessages(suppressWarnings(
      fetch_rxinfo(object)))
    expect_true(is.list(rxinfo))
    expect_true("elements" %in% names(rxinfo))
    expect_true("states"   %in% names(rxinfo[["elements"]]))
  })

  # ------------------------------------------------------------------
  # Multiple evaluation times: simulate_rules() can evaluate rules at
  # several time points (e.g., t=0, 7, 14). At each eval_time the
  # rule conditions are checked and actions fired. With condition="TRUE"
  # the dosing rule fires at every evaluation, producing repeated doses.
  # We verify the simulation succeeds and that the event history grows
  # with each evaluation time.
  # ------------------------------------------------------------------
  test_that("simulate_rules with multiple eval times", {
    set.seed(42)
    rxode2::rxSetSeed(42)

    subs = suppressMessages(suppressWarnings(
      mk_subjects(object = object, nsub = 2)))

    # Same dosing rule as before — fires unconditionally at each eval time
    rules = list(
      dose = list(
        condition = "TRUE",
        action    = list(
          type      = "dose",
          state     = "central",
          values    = "c(1)",
          times     = "c(0)",
          durations = "c(0)")
      )
    )

    # Three evaluation times produce three doses per subject
    eval_times   = c(0, 7, 14)
    output_times = seq(0, 28, 1)

    simres = suppressMessages(suppressWarnings(
      simulate_rules(
        object       = object,
        subjects     = subs[["subjects"]],
        eval_times   = eval_times,
        output_times = output_times,
        rules        = rules)))

    expect_true(simres[["isgood"]])
    expect_true(nrow(simres[["simall"]]) > 0)
    # Multiple eval times should produce multiple events in the history
    expect_true(nrow(as.data.frame(simres[["ev_history"]])) >= 2)
  })
}
