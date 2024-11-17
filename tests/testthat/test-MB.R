suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))
library(stringr)

if( Sys.getenv("ruminate_rxfamily_found")){
  suppressMessages(suppressWarnings(library(rxode2)))
  sess_res = suppressMessages(suppressWarnings(MB_test_mksession()))

  expect_true(sess_res[["isgood"]])

  current_dir = getwd()
  on.exit( setwd(current_dir))

  setwd(tempdir())
    test_that("Building model catalog", {
      state = sess_res$state
      mtres = suppressMessages(suppressWarnings(MB_test_catalog(state, as_cran=FALSE)))
      expect_true(mtres[["isgood"]])
    })
  setwd(current_dir)
}
