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


#'@export
#'@title Run the {ruminate} Shiny App
#'@description Runs the pharmacometrics ruminate app.
#'@param host Hostname of the server ("127.0.0.1")
#'@param port Port number for the app (3838)
#'@param server_opts List of options (names) and their vlues (value) e.g.
#'\code{list(shiny.maxRequestSize = 30 * 1024^2)}.
#'@param mksession Boolean value, when TRUE will load test session data
#'for app testing (FALSE)
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

  # If mksession is true we set this value to true
  if(mksession){
    file.create(ftmptest)
  }

  shiny::runApp(system.file(package="ruminate", "templates","ruminate.R"),
                host  = host,
                port  = port)

}
