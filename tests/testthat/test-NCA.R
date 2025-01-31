library(formods)
library(ruminate)
library(stringr)

sess_res = suppressMessages(suppressWarnings(NCA_test_mksession()))

expect_true(sess_res[["isgood"]])

if(formods::is_installed("readxl")){
  library(readxl)

  test_that("Extract dose records", {

  data_file =  system.file(package="formods","test_data","TEST_DATA.xlsx")
  DS_cols = readxl::read_excel(path=data_file, sheet="DATA")        |>
    dplyr::filter(EVID == 0)                                |>
    dplyr::filter(DOSE %in% c(3))                           |>
    dplyr::filter(str_detect(string=Cohort, "^MD"))         |>
    dplyr::filter(CMT == "C_ng_ml")

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


  drb_res = dose_records_builder(
    NCA_DS     = DS_cols,
    col_map    = col_map,
    dose_from  = "cols")

  expect_true(drb_res[["isgood"]])

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

  test_that("Test session examples", {
    state = sess_res$state
    # This makes sure each analysis in the test
    # session was completed successfully
    for(aname in names(state[["NCA"]][["anas"]])){
      expect_true(state[["NCA"]][["anas"]][[aname]][["isgood"]])
    }
  })


}
