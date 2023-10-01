library(formods)
library(ruminate)
library(stringr)

sess_res = suppressMessages(suppressWarnings(MB_test_mksession(session=list())))

current_dir = getwd()
on.exit( setwd(current_dir))

setwd(tempdir())

test_that("Building model catalog", {
  state = sess_res$state

  mtres = suppressMessages(suppressWarnings(MB_test_catalog(state, as_cran=TRUE)))

  expect_true(mtres[["isgood"]])

})


setwd(current_dir)
