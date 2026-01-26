suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))
if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(suppressWarnings(library(rxode2)))

  test_that("Load CTS test sessions", {
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=FALSE)))
    expect_true(sess_res[["isgood"]])
    sess_res = suppressMessages(suppressWarnings(CTS_test_mksession(full=TRUE)))
    expect_true(sess_res[["isgood"]])
  })
}
