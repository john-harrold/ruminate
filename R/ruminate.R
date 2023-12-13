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

.onLoad <- function(libname, pkgname){


  #------------------------------------
  # Checking for rxpackages
  # If all the suggested packages are found this will be true:
  suggested_found = TRUE
  #mr = FM_message("Loading ruminate", entry_type="h1")

  mr = FM_message("Checking for required nlmixr2 family of tools", entry_type="alert")
  pkgs = c("rxode2", "nonmem2rx", "nlmixr2lib")
  for(pkg in pkgs){
    pkg_var = paste0("ruminate_", pkg, "_found")
    if(!requireNamespace(pkg, quietly=TRUE)){
      pkg_val = FALSE
      suggested_found = FALSE
      mr = FM_message(paste0("missing ", pkg), entry_type="danger")
    } else {
      pkg_val = TRUE
      mr = FM_message(paste0("found ", pkg), entry_type="success")
    }
   eval(parse( text= paste0("Sys.setenv(", pkg_var, "=", pkg_val,")") ))
  }

  Sys.setenv(ruminate_rxfamily_found = suggested_found)
}

#'@export
#'@title Run the {ruminate} Shiny App
#'@description Runs the pharmacometrics ruminate app.
#'@param host Hostname of the server ("127.0.0.1")
#'@param port Port number for the app (3838)
#'@param development Boolean variable indicating
#'@param server_opts List of options (names) and their vlues (value) e.g.
#'\code{list(shiny.maxRequestSize = 30 * 1024^2)}.
#'@param mksession Boolean value, when TRUE will load test session data
#'for app testing and will also load development modules (\code{FALSE}).
#'@return Nothing is returned, this function just runs the built-in ruminate
#'app.
#'@examples
#'if (interactive()) {
#' ruminate()
#'}
ruminate = function(host        = "127.0.0.1",
                    port        = 3838,
                    development = FALSE,
                    server_opts = list(shiny.maxRequestSize = 30 * 1024^2),
                    mksession   = FALSE){



  if(exists("server_opts")){
    for(oname in names(server_opts)){
      eval(parse(text=paste0('options(',oname,'= server_opts[[oname]])')))
    }
  }

  # File used to indicate we're in test mode
  ftmptest = file.path(tempdir(), "ruminate.test")

  # File used to indicate if we are running development versions
  ftmpdev  = file.path(tempdir(), "RUMINTE_DEVELOPMENT")

  # Deleteing any existing files
  if(file.exists(ftmptest)){
    unlink(ftmptest)
  }
  if(file.exists(ftmpdev)){
    unlink(ftmpdev)
  }

  # If mksession is true we create the temporary file
  if(mksession){
    file.create(ftmptest)
  }
  if(development){
    file.create(ftmpdev)
  }

  shiny::runApp(system.file(package="ruminate", "templates","ruminate.R"),
                host  = host,
                port  = port)

}



