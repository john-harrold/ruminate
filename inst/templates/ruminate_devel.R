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

if(is_installed("rxode2")){
  library(rxode2)
}

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
CTS.yaml      = system.file(package="ruminate", "templates",  "CTS.yaml")

# Making sure that the deployed object is created
if(!exists("deployed")){
  deployed = FALSE
}

# If the DEPLOYED file marker existrs we set deployed to TRUE
if(file.exists("DEPLOYED")){
  deployed = TRUE
}


# If the SETUP.R file exists we source it
if(file.exists("SETUP.R")){
  source("SETUP.R")
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
#https://fontawesome.com/search?o=r&m=free
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

ui <- shinydashboard::dashboardPage(
  skin="black",
  shinydashboard::dashboardHeader(title="ruminate"),
  shinydashboard::dashboardSidebar(
     shinydashboard::sidebarMenu(
       shinydashboard::menuItem("Load/Save",
                                tabName="loadsave",
                                icon=icon("arrow-down-up-across-line")) ,
       shinydashboard::menuItem("Transform Data",  tabName="wrangle", icon=icon("shuffle")),
       shinydashboard::menuItem("Visualize",       tabName="plot",    icon=icon("chart-line")),
       shinydashboard::menuItem("NCA",             tabName="nca",     icon=icon("chart-area")),
       shinydashboard::menuItem("Build Models",    tabName="model",   icon=icon("trowel-bricks")),
       shinydashboard::menuItem("Trial Simulator", tabName="trials",  icon=icon("laptop-medical")),
       shinydashboard::menuItem("App Info",        tabName="sysinfo", icon=icon("book-medical"))
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
               shinydashboard::box(title="Build ODE-Based PK/PD Models", width=12,
               fluidRow(
               column(width=12,
               tagList(tags$b("This module is currently under development. Due to the experimental nature some portions may change. If you run into any issues please report them on the ", tags$a("ruminate issues", href=issue_url), "page.")),
               tags$br(),
               tags$br(),
               htmlOutput(NS("MB",  "MB_ui_compact")))))
               ),
       shinydashboard::tabItem(tabName="trials",
               shinydashboard::box(title="Define and Simulate Cohorts", width=12,
               fluidRow(
               column(width=12,
               tagList(tags$b("This module is currently under development. Due to the experimental nature some portions may change. If you run into any issues please report them on the ", tags$a("ruminate issues", href=issue_url), "page.")),
               tags$br(),
               tags$br(),
               htmlOutput(NS("CTS",  "CTS_ui_compact")))))
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
                       htmlOutput(NS(id, "ui_ud_workflows")),
                       div(style="display:inline-block;vertical-align:top;width:40px", uiele_code_button),
                       htmlOutput(NS("UD", "ui_ud_text_load_result"))),
                     column(width=6,
                         tags$p(
                             tags$img(
                             class = "wrapfig",
                             src   = logo_url,
                             width = 150,
                             alt = "ruminate logo" ),
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
  mod_ids = c("UD", "DW", "FG", "NCA", "MB", "CTS")

  # If the ftmptest file is present we load test data
  if(file.exists(ftmptest)){
    sources = c(system.file(package="formods",  "preload", "ASM_preload.yaml"),
                system.file(package="formods",  "preload", "UD_preload.yaml"),
                system.file(package="formods",  "preload", "FG_preload.yaml"),
                system.file(package="formods",  "preload", "DW_preload.yaml"),
                system.file(package="ruminate", "preload", "NCA_preload.yaml"),
                system.file(package="ruminate", "preload", "MB_preload.yaml"),
                system.file(package="ruminate", "preload", "CTS_preload.yaml"))

    res = FM_app_preload(session=session, sources=sources, react_state=react_FM)
  # Otherwise we look for a preload file and load that if it exists
  } else if(file.exists("preload.yaml")){
    shinybusy::show_modal_spinner(text="Preloading analysis, be patient", session=session)
    res = FM_app_preload(session=session, sources="preload.yaml")
    shinybusy::remove_modal_spinner(session = session)
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

  ruminate::CTS_Server(id="CTS", id_ASM = "ASM",
                      deployed         = deployed,
                      react_state      = react_FM,
                      MOD_yaml_file    = CTS.yaml,
                      FM_yaml_file     = formods.yaml)
}

shinyApp(ui, server)
