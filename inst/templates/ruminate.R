# original file: inst/templates/ruminate.R
library(formods)
library(ruminate)

# These are suggested packages
library(shinydashboard)
#library(ggpubr)
#library(plotly)
#library(shinybusy)
library(prompter)
#library(utils)

tags$style("@import url(https://use.fontawesome.com/releases/v6.4.0/css/all.css);")

# You can copy these locally and customize them for your own needs. Simply
# change the assignment to the local copy you've modified.
formods.yaml  = system.file(package="formods",  "templates",  "formods.yaml")
ASM.yaml      = system.file(package="formods",  "templates",  "ASM.yaml")
UD.yaml       = system.file(package="formods",  "templates",  "UD.yaml")
DW.yaml       = system.file(package="formods",  "templates",  "DW.yaml")
FG.yaml       = system.file(package="formods",  "templates",  "FG.yaml")
MB.yaml       = system.file(package="ruminate", "templates",  "MB.yaml")
NCA.yaml      = system.file(package="ruminate", "templates",  "NCA.yaml")

# Making sure that the deployed object is created
if(!exists("deployed")){
  deployed = FALSE
}

# Making sure that the run_dev object is created
if(file.exists(file.path(tempdir(), "RUMINTE_DEVELOPMENT"))){
  run_dev  = TRUE
}else{
  run_dev  = FALSE
}

# If the SETUP.R file exists we source it 
if(file.exists("SETUP.R")){
  source("SETUP.R")
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
logo_url =
  "https://raw.githubusercontent.com/john-harrold/ruminate/main/man/figures/logo.png"
data_url =
  "https://github.com/john-harrold/formods/raw/master/inst/test_data/TEST_DATA.xlsx"
run_url  =
  "https://runruminate.ubiquity.tools/"
use_url  =
  "https://useruminate.ubiquity.tools/"
main_url =
  "https://ruminate.ubiquity.tools/"
issue_url =
  "https://github.com/john-harrold/ruminate/issues"

intro_text = tags$p(
"Ruminate is a shiny module for pharmacometric data processing,
visualization, and analysis. It consists of separate shiny modules that
provide interfaces into common R packages and provides the underlying code.
This is done to facilitate usage of those packages and to provide reproducible
analyses." ,
tags$li( "To find out more visit  ",
        tags$a("ruminate.ubiquity.tools", href=main_url),""),
tags$li( "To give it a try you can download a test dataset ",
        tags$a("here", href=data_url),""),
tags$li( "Go to  ",
        tags$a("useruminate.ubiquity.tools", href=use_url)," for a video
        demonstrating how to use ruminate"),
tags$li( "If you run into any problems, have questions, or want a feature please
        visit the ",
        tags$a("issues", href=issue_url)," page")
)

ftmptest = file.path(tempdir(), "ruminate.test")

# If the ftmptest file is present we load the development modules
if(run_dev){
  dev_modules = shinydashboard::menuItem("Models",          
                                         tabName = "model",       
                                         icon    = icon("trowel-bricks"))
}else {
  dev_modules = NULL
}

ui <- shinydashboard::dashboardPage(
  skin="black",
  shinydashboard::dashboardHeader(title="ruminate"),
  shinydashboard::dashboardSidebar(
     shinydashboard::sidebarMenu(
       shinydashboard::menuItem("Load/Save",
                                tabName="loadsave",
                                icon=icon("arrow-down-up-across-line")) ,
       shinydashboard::menuItem("Transform Data", tabName="wrangle", icon=icon("shuffle")),
       shinydashboard::menuItem("Visualize",      tabName="plot",    icon=icon("chart-line")),
       shinydashboard::menuItem("NCA",            tabName="nca",     icon=icon("chart-area")),
       dev_modules,
      #shinydashboard::menuItem("Models",         tabName="model",   icon=icon("trowel-bricks")),
       shinydashboard::menuItem("App Info",       tabName="sysinfo", icon=icon("book-medical"))
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
       shinydashboard::tabItem(tabName="model",
               shinydashboard::box(title="Build PK/PD Models", width=12,
               fluidRow( 
               column(width=12,
               htmlOutput(NS("MB",  "MB_ui_compact")))))
               ),
       shinydashboard::tabItem(tabName="loadsave",
         #     shinydashboard::box(title=NULL, width=12,
               shinydashboard::tabBox(
                 width = 12,
                 title = NULL,
                 shiny::tabPanel(id="load_data",
                          title=tagList(shiny::icon("file-arrow-up"),
                                        "Load Data"),
                   fluidRow(
                     column(width=6,
                       div(style="display:inline-block;width:100%",
                       htmlOutput(NS("UD", "ui_ud_load_data"))),
                       htmlOutput(NS("UD", "ui_ud_clean")),
                       htmlOutput(NS("UD", "ui_ud_select_sheets")),
                       htmlOutput(NS("UD", "ui_ud_text_load_result"))),
                     column(width=6,
                         tags$p(
                             tags$img(
                             class = "wrapfig",
                             src   = logo_url,
                             width = 150,
                             alt = "formods logo" ),
                         intro_text
                         ))
                   ),
                 fluidRow(
                   column(width=12,
                          div(style="display:inline-block;vertical-align:top",
                                    htmlOutput(NS("UD", "ui_ud_data_preview")))
                          ))
                 ),
                 shiny::tabPanel(id="save_state",
                          title=tagList(shiny::icon("arrow-down-up-across-line"),
                                        "Save or Load Analysis"),
                 fluidRow(
                   column(width=5,
                          div(style="display:inline-block;vertical-align:top",
                   htmlOutput(NS("ASM", "ui_asm_compact"))
                   ))
                   )
                 )
               )
         #     ),
               ),
       shinydashboard::tabItem(tabName="wrangle",
               shinydashboard::box(title="Transform and Create Views of Your Data", width=12,
               fluidRow(
               column(width=12,
               htmlOutput(NS("DW",  "DW_ui_compact")))))
               ),
       shinydashboard::tabItem(tabName="plot",
               shinydashboard::box(title="Visualize Data", width=12,
               htmlOutput(NS("FG",  "FG_ui_compact")))),
       shinydashboard::tabItem(tabName="sysinfo",
         #     box(title="System Details", width=12,
               shinydashboard::tabBox(
                 width = 12,
                 title = NULL,
                 shiny::tabPanel(id="sys_packages",
                          title=tagList(shiny::icon("box-open"),
                                        "Installed Packages"),
                 htmlOutput(NS("ASM", "ui_asm_sys_packages"))
                 ),
                 shiny::tabPanel(id="sys_modules",
                          title=tagList(shiny::icon("cubes"),
                                        "Loaded Modules"),
                 htmlOutput(NS("ASM", "ui_asm_sys_modules"))
                 ),
                 shiny::tabPanel(id="sys_log",
                          title=tagList(shiny::icon("clipboard-list"),
                                        "Log"),
                 verbatimTextOutput(NS("ASM", "ui_asm_sys_log"))
                 ),
                 shiny::tabPanel(id="sys_options",
                          title=tagList(shiny::icon("sliders"),
                                        "R Options"),
                 htmlOutput(NS("ASM", "ui_asm_sys_options"))
                 )
         #       )
               ))
      )
    )
  )

# Main app server
server <- function(input, output, session) {

  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module IDs and the order they are needed for code generation
  mod_ids = c("UD", "DW", "FG", "NCA", "MB")

  # If the ftmptest file is present we load test data
  if(file.exists(ftmptest)){
    NCA_test_mksession(
      session,
      id     = "NCA",
      id_UD  = "UD",
      id_DW  = "DW",
      id_ASM = "ASM"
    )
    MB_test_mksession(
      session,
      full_session=TRUE
    )
  }

  # Module servers
  formods::ASM_Server( id="ASM",
                       deployed      = deployed,
                       react_state   = react_FM,
                       FM_yaml_file  = formods.yaml,
                       MOD_yaml_file = ASM.yaml,
                       mod_ids       = mod_ids)
  formods::UD_Server(  id ="UD", id_ASM = "ASM",
                       deployed         = deployed,
                       react_state      = react_FM,
                       MOD_yaml_file    = UD.yaml,
                       FM_yaml_file     = formods.yaml)
  formods::DW_Server(  id="DW",       id_ASM = "ASM",
                       id_UD = "UD",
                       deployed         = deployed,
                       react_state      = react_FM,
                       MOD_yaml_file    = DW.yaml,
                       FM_yaml_file     = formods.yaml)
  formods::FG_Server(  id="FG",     id_ASM = "ASM",
                       id_UD = "UD", id_DW = "DW",
                       deployed         = deployed,
                       react_state      = react_FM,
                       MOD_yaml_file    = FG.yaml,
                       FM_yaml_file     = formods.yaml)
  ruminate::NCA_Server(id    ="NCA", id_ASM = "ASM",
                       id_UD = "UD", id_DW  = "DW",
                       deployed         = deployed,
                       react_state      = react_FM,
                       MOD_yaml_file    = NCA.yaml,
                       FM_yaml_file     = formods.yaml)

  ruminate::MB_Server(id="MB", id_ASM = "ASM", 
                      deployed         = deployed,
                      react_state      = react_FM,
                      MOD_yaml_file    = MB.yaml,
                      FM_yaml_file     = formods.yaml)

}

shinyApp(ui, server)
