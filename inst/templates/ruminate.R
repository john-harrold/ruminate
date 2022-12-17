library(formods)

# These are suggested packages
library(shinydashboard)
library(ggpubr)
library(plotly)
library(shinybusy)
library(utils)

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

intro_text = "Ruminate"
ui <- dashboardPage(
  skin="black",
  dashboardHeader(title="ruminate"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Load/Save",       tabName="loadsave",    icon=icon("arrow-down-up-across-line")) ,
       menuItem("Transform Data",  tabName="wrangle",     icon=icon("shuffle")),
       menuItem("Visualize",       tabName="plot",        icon=icon("chart-line")),
       menuItem("NCA",             tabName="nca",         icon=icon("chart-area"))
     )
  ),
  dashboardBody(
  tags$head(
    tags$style(HTML(CSS))
  ),
    tabItems(
       tabItem(tabName="nca", 
               htmlOutput(NS("NCA",  "NCA_ui_compact")),
               ),
       tabItem(tabName="loadsave",
               box(title="Load Dataset", width=12,
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
               box(title="Save or Load Analysis", width=12,
                   htmlOutput(NS("ASM", "ui_asm_compact")))
               ),
       tabItem(tabName="wrangle",
               box(title="Transform and Create Views of Your Data", width=12,
               htmlOutput(NS("DW",  "DW_ui_compact")))),
       tabItem(tabName="plot",
               box(title="Visualize Data", width=12,
               htmlOutput(NS("FG",  "FG_ui_compact"))))
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  #FM_load_test_state(session=session, react_state=react_FM, input=input)

  # Module servers
  ASM_Server(id="ASM",                                           react_state=react_FM)
  UD_Server( id="UD", id_ASM = "ASM",                            react_state=react_FM)
  DW_Server( id="DW", id_ASM = "ASM",id_UD = "UD",               react_state=react_FM)
  FG_Server( id="FG", id_ASM = "ASM",id_UD = "UD", id_DW = "DW", react_state=react_FM)
}

shinyApp(ui, server)
