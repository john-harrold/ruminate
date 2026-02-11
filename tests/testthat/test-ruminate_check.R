# Testing the ruminate_check() function which inspects suggested package
# dependencies and reports which are found and which are missing. This
# function does not require rxode2 or any other optional dependency.
suppressMessages(suppressWarnings(library(formods)))
suppressMessages(suppressWarnings(library(ruminate)))

test_that("ruminate_check returns expected structure with verbose=FALSE", {
  # Running the check silently (verbose=FALSE suppresses FM_message output)
  rcres = suppressMessages(suppressWarnings(ruminate_check(verbose = FALSE)))

  # The return value should be a list with three named elements:
  #   - all_found:    logical, TRUE if every suggested package is installed
  #   - found_pkgs:   character vector of package names that were found
  #   - missing_pkgs: character vector of package names that were not found
  expect_true(is.list(rcres))
  expect_true("all_found"    %in% names(rcres))
  expect_true("found_pkgs"   %in% names(rcres))
  expect_true("missing_pkgs" %in% names(rcres))

  # Verify the types are correct; either vector may be NULL if empty
  expect_true(is.logical(rcres[["all_found"]]))
  expect_true(is.character(rcres[["found_pkgs"]]) || is.null(rcres[["found_pkgs"]]))
  expect_true(is.character(rcres[["missing_pkgs"]]) || is.null(rcres[["missing_pkgs"]]))
})

test_that("ruminate_check returns expected structure with verbose=TRUE", {
  # Running with verbose=TRUE exercises the FM_message() code path
  # that prints success/danger messages for each package
  rcres = suppressMessages(suppressWarnings(ruminate_check(verbose = TRUE)))

  # Same structural checks as above
  expect_true(is.list(rcres))
  expect_true(is.logical(rcres[["all_found"]]))

  # Since formods and ruminate are loaded, at least some packages should be found
  expect_true(length(rcres[["found_pkgs"]]) > 0)
})
