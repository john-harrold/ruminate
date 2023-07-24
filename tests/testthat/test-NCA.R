library(formods)
library(ruminate)
library(stringr)
if(system.file(package="readxl") !=""){
  readxl_found = TRUE 
  library(readxl)
} else {
  readxl_found = FALSE
}
# Creating an NCA state object:
#sessres = NCA_test_mksession(session=list())
#state   = sessres[["state"]]

sess_res = suppressMessages(suppressWarnings(NCA_test_mksession(session=list())))


if(system.file(package="readxl") != ""){

  test_that("Extract dose records", {

  if(readxl_found){
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
  }


  })

  test_that("Test session examples", {
    state = sess_res$state
    # This makes sure each analysis in the test 
    # session was completed successfully
    for(aname in names(state[["NCA"]][["anas"]])){
      expect_true(state[["NCA"]][["anas"]][[aname]][["isgood"]])
    }
  })

 #test_that("Run NCA", {
 #})


  # JMH add test for NCA_test_mksession to make sure all of the state$NCA$anas
  # are good

  #*NCA_fetch_state
  #*NCA_fetch_code
  #*NCA_init_state
  #*NCA_new_ana   
  #*NCA_set_current_ana   
  #*NCA_find_col
  # NCA_add_int 
  #*NCA_fetch_ana_ds
  #*NCA_process_current_ana
  #*nca_builder
  # run_nca_components
  #*mk_table_ind_obs   
  #*mk_figure_ind_obs   
  #*mk_table_nca_params
  # NCA_load_scenario

}
