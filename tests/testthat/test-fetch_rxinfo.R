suppressMessages(suppressWarnings(library(ruminate)))

if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
  suppressMessages(suppressWarnings(library(rxode2)))
  suppressMessages(suppressWarnings(library(babelmixr2)))

  # Creating models with different attributes for testing
  # mod_1 - ODES, no endpoints, no IIV, no covariates
  mod_1 =function () {
      ini({
          TV_Vc <- c(2.22044604925031e-16, 1)
          TV_Vt <- c(2.22044604925031e-16, 1)
          TV_CL <- c(2.22044604925031e-16, 1)
          TV_Q <- c(2.22044604925031e-16, 1)
          TV_ka <- c(2.22044604925031e-16, 1)
          TV_fb <- c(2.22044604925031e-16, 1)
      })
      model({
          Vc          =  TV_Vc
          Vt          = TV_Vt
          CL          = TV_CL
          Q           = TV_Q
          ka          = TV_ka
          fb          = TV_fb
          Dinf        = 0
          d/dt(At)    = (-ka * At)
          d/dt(Cc)    = (ka * At * fb/Vc - CL/Vc * Cc - Q * (Cc -
              Cp)/Vc + Dinf/Vc)
          d/dt(Cp)    = (+Q * (Cc - Cp)/Vt)
      })
  }

  rxobj_1 = rxode2::rxode2(mod_1)

  rxnfo_1 = fetch_rxinfo(rxobj_1)

  expect_true(length(rxnfo_1[["elements"]][["iiv"]])             == 0)
  expect_true(length(rxnfo_1[["elements"]][["outputs"]])         == 0)
  expect_true(length(rxnfo_1[["elements"]][["covariates"]])      == 0)
  expect_true(length(rxnfo_1[["elements"]][["parameters"]])      == 6)
  expect_true(length(rxnfo_1[["elements"]][["population"]])      == 6)
  expect_true(length(rxnfo_1[["elements"]][["states"]])          == 3)
  expect_true(length(rxnfo_1[["elements"]][["residual_error"]])  == 0)
  expect_true(length(rxnfo_1[["elements"]][["secondary"]])       == 0)

  suppressMessages({
    rxobj_2 = rxobj_1 |>
      model({Vc  = exp(TV_Vc + eta.Vc)}, append=TRUE)  |>
      model({Cp_ng_ml <- Cp
             Cp_ng_ml ~  add(add.sd) }, append=TRUE)
  } )
  rxnfo_2 = fetch_rxinfo(rxobj_2)
  expect_true(length(rxnfo_2[["elements"]][["iiv"]])             == 1)
  expect_true(length(rxnfo_2[["elements"]][["residual_error"]])  == 1)
  expect_true(length(rxnfo_2[["elements"]][["outputs"]])         == 1)

}
