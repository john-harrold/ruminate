% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CTS_Server.R
\name{CTS_fetch_sc_meta}
\alias{CTS_fetch_sc_meta}
\title{Fetches Simulation Parameter Meta Information}
\usage{
CTS_fetch_sc_meta(
  MOD_yaml_file = system.file(package = "ruminate", "templates", "CTS.yaml")
)
}
\arguments{
\item{MOD_yaml_file}{Module configuration file with MC as main section.}
}
\value{
List with the following elements:
\itemize{
\item{config} List from the YAML->MC->sim_config.
\item{summary:} Dataframe with elements of config in tabular format.
\item{ui_config} Vector of  all the ui_ids for configuration options.
}
}
\description{
This provides meta information about simulatino options. This
includes option names, text descriptions, ui_names used, etc.
}
\examples{
CTS_fetch_sc_meta()
}
