if(interactive()){
library(formods)
library(ruminate)

# These are suggested packages
library(shinydashboard)
#library(ggpubr)
#library(plotly)
#library(shinybusy)
library(prompter)
#library(utils)


# You can copy these locally and customize them for your own needs. Simply
# change the assignment to the local copy you've modified.
formods.yaml  = system.file(package="formods","templates", "formods.yaml")
ASM.yaml      = system.file(package="formods","templates", "ASM.yaml")
UD.yaml       = system.file(package="formods","templates", "UD.yaml")
DW.yaml       = system.file(package="formods","templates", "DW.yaml")
FG.yaml       = system.file(package="formods","templates", "FG.yaml")

# Making sure that the deployed object is created
if(!exists("deployed")){
  deployed = FALSE
}

# If the DEPLOYED file marker existrs we set deployed to TRUE
if(file.exists("DEPLOYED")){
  deployed = TRUE
}

CSS <- "
.wrapfig {
  float: right;
  shape-margin: 20px;
  margin-right: 20px;
  margin-bottom: 20px;
}
"
#https://fontawesome.com/icons?from=io
logo_url = "https://raw.githubusercontent.com/john-harrold/ruminate/main/man/figures/logo.png"
#logo_url = paste0("file://", system.file(package = "ruminate","help","figures","logo.png"))
data_url =
"https://github.com/john-harrold/formods/raw/master/inst/test_data/TEST_DATA.xlsx"

intro_text = tags$p(
"Ruminate is a shiny module for pharmacometric data process,
visualization, and analysis. It consists of separate shiny modules that
provide interfaces into common R packages and provides the underlying code
to facilitate usage of those packages and to provide reproducable analyses.
To give it a try you can Download the test dataset ",
tags$a("here", href=data_url),".")

# intro_text = "Ruminate is a shiny module for pharmacometric data process,
# visualization, and analysis. It consists of separate shiny modules that
# provide interfaces into common R packages. The data transformation tools act
# as an interface to {dplyr} and {tidyr}, the visualization module acts as an
# interface to {ggplot}, and the NCA module acts as an interface to {PKNCA}. It
# offers reporting of the components in Word, PowerPoint and Excel. The
# internals of ruminate generate code on the fly and allow you view the code for
# each module element. It will also generate a script that when you save an
# analysis to reproduce all you didn within the interface."

ui <- shinydashboard::dashboardPage(
  skin="black",
  shinydashboard::dashboardHeader(title="ruminate"),
  shinydashboard::dashboardSidebar(
     shinydashboard::sidebarMenu(
       shinydashboard::menuItem("Load/Save",       tabName="loadsave",    icon=icon("arrow-down-up-across-line")) ,
       shinydashboard::menuItem("Transform Data",  tabName="wrangle",     icon=icon("shuffle")),
       shinydashboard::menuItem("Visualize",       tabName="plot",        icon=icon("chart-line")),
       shinydashboard::menuItem("NCA",             tabName="nca",         icon=icon("chart-area"))
     )
  ),
  shinydashboard::dashboardBody(
  tags$head(
    tags$style(HTML(CSS))
  ),
    shinydashboard::tabItems(
       shinydashboard::tabItem(tabName="nca",
               shinydashboard::box(title="Run Non-Compartmental Analysis", width=12,
               fluidRow( prompter::use_prompt(),
               column(width=12,
               htmlOutput(NS("NCA",  "NCA_ui_compact")))))
               ),
       shinydashboard::tabItem(tabName="loadsave",
               shinydashboard::box(title="Load Dataset", width=12,
                 fluidRow(
                   column(width=6,
                     htmlOutput(NS("UD",  "UD_ui_compact"))),
                   column(width=6,
                       tags$p(
                           tags$img(
                           class = "wrapfig",
                           src   = logo_url,
                           width = 150,
                           alt = "formods logo" ),
                       intro_text
                       ))
                 )
               ),
               shinydashboard::box(title="Save or Load Analysis", width=12,
                   htmlOutput(NS("ASM", "ui_asm_compact")))
               ),
       shinydashboard::tabItem(tabName="wrangle",
               shinydashboard::box(title="Transform and Create Views of Your Data", width=12,
               fluidRow(
               column(width=12,
               htmlOutput(NS("DW",  "DW_ui_compact")))))
               ),
       shinydashboard::tabItem(tabName="plot",
               shinydashboard::box(title="Visualize Data", width=12,
               htmlOutput(NS("FG",  "FG_ui_compact"))))
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module IDs and the order they are needed for code generation
  mod_ids = c("UD", "DW", "FG", "NCA")

  # If the ftmptest file is present we load test data
  ftmptest = file.path(tempdir(), "ruminate.test")
  if(file.exists(ftmptest)){
    NCA_test_mksession(
      session,
      id     = "NCA",
      id_UD  = "UD",
      id_DW  = "DW",
      id_ASM = "ASM"
    )
  }

  # Module servers
  formods::ASM_Server( id="ASM",
                       deployed     = deployed,
                       react_state  = react_FM,
                       FM_yaml_file = formods.yaml,
                       mod_ids      = mod_ids)
  formods::UD_Server(  id="UD",  id_ASM = "ASM",
                       deployed     = deployed,
                       react_state=react_FM,
                       FM_yaml_file=formods.yaml)
  formods::DW_Server(  id="DW",       id_ASM = "ASM",
                       id_UD = "UD",
                       deployed     = deployed,
                       react_state=react_FM,
                       FM_yaml_file=formods.yaml)
  formods::FG_Server(  id="FG",     id_ASM = "ASM",
                       id_UD = "UD", id_DW = "DW",
                       deployed     = deployed,
                       react_state=react_FM,
                       FM_yaml_file=formods.yaml)
  ruminate::NCA_Server(id="NCA",     id_ASM = "ASM",
                       id_UD = "UD", id_DW = "DW",
                       deployed     = deployed,
                       react_state=react_FM)
}

shinyApp(ui, server)
}
