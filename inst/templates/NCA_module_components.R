library(formods)
library(shinydashboard)

library(devtools)
load_all()

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="NCA Module Template"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("NCA",    tabName="ncatab",  icon=icon("chart-area")) ,
       menuItem("Other",  tabName="other", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="ncatab",
       fluidRow(
         box(title="Current NCAs",
             "ui_nca_curr_ncas",
             htmlOutput(NS("NCAFG", "ui_nca_curr_ncas")), width=5),
         box(title="Current Data Views",
             "ui_nca_curr_views",
             htmlOutput(NS("NCAFG", "ui_nca_curr_views")), width=5)),
       fluidRow(
         box(title="NCA Actions",
             div(style="display:inline-block",
                 "ui_nca_new_nca",
                 htmlOutput(NS("NCA", "ui_nca_new_nca"))),
             div(style="display:inline-block",
                 "ui_nca_save_nca",
                 htmlOutput(NS("NCA", "ui_nca_save_nca"))),
             div(style="display:inline-block",
                 "ui_nca_clip_code",
                 htmlOutput(NS("NCA", "ui_nca_clip_code")) ),
             div(style="display:inline-block",
                 "ui_nca_del_nca",
                 htmlOutput(NS("NCA", "ui_nca_del_nca"))),
             div(style="display:inline-block",
                 "ui_nca_copy_nca",
                 htmlOutput(NS("NCA", "ui_nca_copy_nca"))),
             width = 12)
       ),
       fluidRow(
         box(title="NCA Caption",
             "ui_nca_nca_name",
             htmlOutput(NS("NCA", "ui_nca_nca_name")),
             tags$br(),
             "ui_nca_nca_notes",
             htmlOutput(NS("NCA", "ui_nca_nca_notes")),
             width=12)),
       fluidRow(
         box(title="Messages",
           "ui_nca_msg",
           verbatimTextOutput(NS("NCA", "ui_nca_msg")), width=12)),
       fluidRow(
         box(title="Generated Code",
           "NCA_ui_ace_code",
           shinyAce::aceEditor(NS("NCA", "NCA_ui_ace_code")), width=12)),
       fluidRow(
         box(title="Current Module State",
           verbatimTextOutput("ui_state"),width=12))
       ),
       tabItem(tabName="other", "Here you can put other elements of your App")
      )
    )
  )

# Main app server
server <- function(input, output, session) {
   # Empty reactive object to track and react to
   # changes in the module state outside of the module
   react_FM = reactiveValues()

 # # Module server
 # NCA_Server(id="NCA", react_state=react_FM)
#
 # # Current state outside of the module
 # output$ui_state  =  renderText({
 #   uiele = paste(capture.output(str(react_FM[["NCA"]])), collapse="\n")
 # uiele})
}

shinyApp(ui, server)
