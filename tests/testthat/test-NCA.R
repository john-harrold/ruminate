library(formods)
library(ruminate)
library(stringr)
# Creating an NCA state object:
#sessres = NCA_test_mksession(session=list())
#state   = sessres[["state"]]

if(system.file(package="readxl") != ""){
library(readxl)

  test_that("Extract dose records", {

    data_file =  system.file(package="formods","test_data","TEST_DATA.xlsx")
    DS_cols = readxl::read_excel(path=data_file, sheet="DATA")        |>
      dplyr::filter(EVID == 0)                                |>
      dplyr::filter(DOSE %in% c(3))                           |>
      dplyr::filter(str_detect(string=Cohort, "^MD"))         |>
      dplyr::filter(CMT == "C_ng_ml")

    drb_res = dose_records_builder(
      NCA_DS     = DS_cols,
      dose_from  = "cols",
      col_id     = "ID",
      col_time   = "TIME_DY",
      col_ntime  = "NTIME_DY",
      col_route  = "ROUTE",
      col_cycle  = "DOSE_NUM",
      col_dose   = "DOSE",
      col_group  = "Cohort")

    expect_true(drb_res[["isgood"]])

    DS_rows = readxl::read_excel(path=data_file, sheet="DATA")        |>
      dplyr::filter(DOSE %in% c(3))                                   |>
      dplyr::filter(str_detect(string=Cohort, "^MD"))                 |>
      dplyr::filter(CMT %in% c("Ac", "C_ng_ml"))

    drb_res = dose_records_builder(
      NCA_DS     = DS_rows,
      dose_from  = "rows",
      col_id     = "ID",
      col_time   = "TIME_DY",
      col_ntime  = "NTIME_DY",
      col_route  = "ROUTE",
      col_dose   = "AMT",
      col_evid   = "EVID",
      col_group  = "Cohort")

    expect_true(drb_res[["isgood"]])


  })

  test_that("Run NCA", {
  })


}
