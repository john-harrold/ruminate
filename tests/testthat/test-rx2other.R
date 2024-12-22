Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")

suppressMessages(library(formods))
suppressMessages(library(ruminate))
library(stringr)


if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(library(rxode2))
  sess_res = suppressMessages(suppressWarnings(MB_test_mksession()))

  current_dir = getwd()
  on.exit( setwd(current_dir))
  setwd(tempdir())

  test_that("Testing NONMEM and Monolix conversion", {
    state   = sess_res$state
    catalog = suppressMessages(suppressWarnings(MB_fetch_catalog(state)))
    for(ridx in 1:nrow(catalog$summary)){
    #for(ridx in 16){
      tmp_ana_sol = catalog$summary[ridx, ]$ana_sol
      # We're only testing ODE models here
      if(tmp_ana_sol == "no"){
        # Walking through each model and building it
        tmp_obj = catalog$summary[ridx, ]$Object
        tmp_mod = catalog$summary[ridx, ]$Model

        # This is used as a general flag for the current model to determine
        # if it should be used for conversion
        model_isgood = TRUE

        # This will define and build the rxode2 model object
        cmd = paste0(tmp_mod, "\n", "tmp_rx = suppressMessages(suppressWarnings(rxode2::rxode2(", tmp_obj,")))")
        tcres =
          FM_tc(cmd     = cmd,
                tc_env  = list(),
                capture = "tmp_rx")

        expect_true(tcres[["isgood"]])
        if(tcres[["isgood"]]){
          tmp_rx = tcres[["capture"]][["tmp_rx"]]
          rxdetails  = fetch_rxinfo(tmp_rx)

          if(rxdetails[["isgood"]]){
            # If everything is good up to this point then we try to export in
            # different formats:
            covres = suppressMessages(suppressWarnings(rx2other(tmp_rx, out_type="nonmem")))
            expect_true(covres[["isgood"]])
            if(!covres[["isgood"]]){
              message(paste0("Failed conversion to NONMEM for ridx: ", ridx, ", model: ", tmp_obj))
              message(paste0(covres[["msgs"]], collapse="\n"))
            }

            # JMH this is a fix for f(depot) not being properly caught. it can
            # be removed once the bug is fixed
            TEST_MONOLIX = TRUE
            # Monolix fails with bioavailability, so we have to flag that here:
            if("cmtProp" %in% names(tmp_rx$props)){
              if("Property" %in% names(tmp_rx$props$cmtProp)){
                if(any(tmp_rx$props$cmtProp$Property == "f")){
                  TEST_MONOLIX = FALSE
                }
              }
            }
            if(TEST_MONOLIX){
              covres = suppressMessages(suppressWarnings(rx2other(tmp_rx, out_type="monolix")))
              expect_true(covres[["isgood"]])
              if(!covres[["isgood"]]){
                message(paste0("Failed conversion to Monolix for ridx: ", ridx, ", model: ", tmp_obj))
                message(paste0(covres[["msgs"]], collapse="\n"))
              }
            }
          } else {
            message(paste0("rxdetails were bad for ridx: ", ridx, ", model: ", tmp_obj))
          }
        }
      }
    }
  })
  setwd(current_dir)
}
