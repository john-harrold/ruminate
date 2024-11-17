suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))
if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(suppressWarnings(library(rxode2)))
  sess_res = suppressMessages(suppressWarnings(CTS_test_mksession()))
  expect_true(sess_res[["isgood"]])
}
