#' ruminate: Shiny app and module to facilitate pharamacometrics analysis
#'
#' This is done by creating a Shiny interface to different tools for data
#' transformation (`dplyr` and `tidyr`), plotting (`ggplot2`), and
#' noncompartmental analysis (`PKNCA`). These results can be reported in Excel,
#' Word or PowerPoint. The state of the app can be saved and loaded at a later
#' date. When saved, a script is generated to reproduce the different actions in
#' the Shiny interface.
#'
#' @seealso \url{https://ruminate.ubiquity.tools/}
#' @docType package
#' @name ruminate
"_PACKAGE"

#'@import shiny

.onAttach <- function(libname, pkgname){

  #------------------------------------
  # Checking for rxpackages
  # If all the suggested packages are found this will be true:
  suggested_found = TRUE
  #mr = FM_message("Loading ruminate", entry_type="h1")
  #mr = FM_message("Checking for required nlmixr2 family of tools", entry_type="h2")

  pkgs = c("rxode2", "nonmem2rx", "nlmixr2lib")
  for(pkg in pkgs){
    pkg_var = paste0("ruminate_", pkg, "_found")
    if(!requireNamespace(pkg, quietly=TRUE)){
      pkg_val = FALSE
      suggested_found = FALSE
      #mr = FM_message(paste0("missing ", pkg), entry_type="danger")
    } else {
      pkg_val = TRUE
      #mr = FM_message(paste0("found ", pkg), entry_type="success")
    }
   eval(parse( text= paste0("Sys.setenv(", pkg_var, "=", pkg_val,")") ))
  }

  Sys.setenv(ruminate_rxfamily_found = suggested_found)

  packageStartupMessage("Loading ruminate")
  rcres = ruminate_check(verbose = FALSE)
  if(!rcres[["all_found"]]){
    packageStartupMessage("Missing suggested packages")
    for(pkg in rcres[["missing_pkgs"]]){
      packageStartupMessage(paste0(" - ",pkg))
    }
    packageStartupMessage("")
    packageStartupMessage("Install with the following:")
    packageStartupMessage(paste0('install.packages(c("', paste0(rcres[["missing_pkgs"]], collapse='", "'), '"))'))
  }
}

#'@export
#'@title Checks `ruminate` Dependencies
#'@description  Looks at the suggested dependencies and checks to make sure
#'they are installed.
#'@param verbose Logical indicating if messages should be displayed
#'@return List with the following elements:
#' \itemize{
#'   \item{all_found:}    Boolean indicating if all packages were found
#'   \item{found_pkgs:}   Character vector of found packages
#'   \item{missing_pkgs:} Character vector of missing packages
#'}
#'@examples
#' fcres =ruminate_check()
ruminate_check <- function(verbose=TRUE){

  #------------------------------------
  # Checking for rxpackages
  # If all the suggested packages are found this will be true:
  suggested_found = TRUE
  if(verbose){
    mr = FM_message("Checking ruminate for suggested packages", entry_type="h1")
  }

  pkgs = c(
   "clipr",
   "gridExtra",
   "knitr",
   "nlmixr2lib",
   "nonmem2rx",
   "prompter",
   "rmarkdown",
   "readxl",
   "shinydashboard",
   "testthat",
   "ubiquity")

  pkg_found   = c()
  pkg_missing =  c()
  for(pkg in pkgs){
    if(!requireNamespace(pkg, quietly=TRUE)){
      if(verbose){
        mr = FM_message(paste0("missing ", pkg), entry_type="danger")
      }
      pkg_missing = c(pkg_missing, pkg)
      suggested_found = FALSE
    } else {
      if(verbose){
        mr = FM_message(paste0("found ", pkg), entry_type="success")
      }
      pkg_found   = c(pkg_found  , pkg)
    }
  }

  res = list(
    all_found     = suggested_found,
    found_pkgs    = pkg_found,
    missing_pkgs  = pkg_missing
  )
res}



#'@export
#'@title Run the {ruminate} Shiny App
#'@description Runs the pharmacometrics ruminate app.
#'@param host Hostname of the server ("127.0.0.1")
#'@param port Port number for the app (3838)
#'@param server_opts List of options (names) and their vlues (value) e.g.
#'\code{list(shiny.maxRequestSize = 30 * 1024^2)}.
#'@param mksession Boolean value, when TRUE will load test session data
#'for app testing
#'@return Nothing is returned, this function just runs the built-in ruminate
#'app.
#'@examples
#'if (interactive()) {
#' ruminate()
#'}
ruminate = function(host        = "127.0.0.1",
                    port        = 3838,
                    server_opts = list(shiny.maxRequestSize = 30 * 1024^2),
                    mksession   = FALSE){



  if(exists("server_opts")){
    for(oname in names(server_opts)){
      eval(parse(text=paste0('options(',oname,'= server_opts[[oname]])')))
    }
  }

  # File used to indicate we're in test mode
  ftmptest = file.path(tempdir(), "ruminate.test")

  # Deleteing any existing files
  if(file.exists(ftmptest)){
    unlink(ftmptest)
  }

  # If mksession is true we create the temporary file
  if(mksession){
    file.create(ftmptest)
  }

  shiny::runApp(system.file(package="ruminate", "templates","ruminate.R"),
                host  = host,
                port  = port)

}
