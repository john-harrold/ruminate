library(formods)
library(shinydashboard)
library(prompter)
library(plotly)

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
         prompter::use_prompt(),
         box(title="Current Analysis",
             "ui_nca_curr_anas",
             htmlOutput(NS("NCA", "ui_nca_curr_anas")), width=5),
         box(title="Current Data Views",
             "ui_nca_curr_views",
             htmlOutput(NS("NCA", "ui_nca_curr_views")), width=5)),
       fluidRow(
         box(title="Analysis Actions",
             div(style="display:inline-block",
                 "ui_nca_new_ana",
                 htmlOutput(NS("NCA", "ui_nca_new_ana"))),
             div(style="display:inline-block",
                 "ui_nca_save_ana",
                 htmlOutput(NS("NCA", "ui_nca_save_ana"))),
             div(style="display:inline-block",
                 "ui_nca_clip_code",
                 htmlOutput(NS("NCA", "ui_nca_clip_code")) ),
             div(style="display:inline-block",
                 "ui_nca_del_ana",
                 htmlOutput(NS("NCA", "ui_nca_del_ana"))),
             div(style="display:inline-block",
                 "ui_nca_copy_ana",
                 htmlOutput(NS("NCA", "ui_nca_copy_ana"))),
             width = 12)
       ),
       fluidRow(
         box(title="NCA Caption",
             "ui_nca_ana_name",
             htmlOutput(NS("NCA", "ui_nca_ana_name")),
             tags$br(),
             "ui_nca_ana_notes",
             htmlOutput(NS("NCA", "ui_nca_ana_notes")),
             width=12)),
       fluidRow(
         box(title="Messages",
           "ui_nca_msg",
           verbatimTextOutput(NS("NCA", "ui_nca_msg")), width=12)),
       fluidRow(
         box(title="Run Analysis",
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_source_sampling",
             htmlOutput(NS("NCA", "ui_nca_ana_source_sampling")),
             ),
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_run",
             htmlOutput(NS("NCA", "ui_nca_ana_run")), tags$br()
             ),
             width=6),
         box(title="Scenarios",
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_scenario_use",
             htmlOutput(NS("NCA", "ui_nca_ana_scenario_use")), tags$br()
             ),
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_scenario",
             htmlOutput(NS("NCA", "ui_nca_ana_scenario"))
             ),
             width=6)),
       fluidRow(
         box(title="Current Intervals",
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_int_start",
             htmlOutput(NS("NCA", "ui_nca_ana_int_start")),
             ),
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_int_stop",
             htmlOutput(NS("NCA", "ui_nca_ana_int_stop")),
             ),
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_params",
             htmlOutput(NS("NCA", "ui_nca_ana_params")),
             ),
           div(style="display:inline-block;vertical-align:bottom",
             "ui_nca_ana_add_int",
             htmlOutput(NS("NCA", "ui_nca_ana_add_int")), tags$br()
             ),
          width=12)),
       fluidRow(
         box(title="Current Intervals",
           "hot_nca_intervals",
          rhandsontable::rHandsontableOutput(NS("NCA", "hot_nca_intervals")),
          width=12)),
       fluidRow(
         box(title="Column Identification",
           div(style="display:inline-block",
             "ui_nca_ana_col_id",
             htmlOutput(NS("NCA", "ui_nca_ana_col_id"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_time",
             htmlOutput(NS("NCA", "ui_nca_ana_col_time"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_ntime",
             htmlOutput(NS("NCA", "ui_nca_ana_col_ntime"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_conc",
             htmlOutput(NS("NCA", "ui_nca_ana_col_conc"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_dose",
             htmlOutput(NS("NCA", "ui_nca_ana_col_dose"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_route",
             htmlOutput(NS("NCA", "ui_nca_ana_col_route"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_cycle",
             htmlOutput(NS("NCA", "ui_nca_ana_col_cycle"))),
           tags$br(), tags$br(),
           div(style="display:inline-block",
             "ui_nca_ana_col_group",
             htmlOutput(NS("NCA", "ui_nca_ana_col_group"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_analyte",
             htmlOutput(NS("NCA", "ui_nca_ana_col_analyte"))),
           div(style="display:inline-block",
             "ui_nca_ana_col_dur",
             htmlOutput(NS("NCA", "ui_nca_ana_col_dur"))),
             width=12)),
       fluidRow(
         box(title="Specify Units",
           div(style="display:inline-block",
             "ui_nca_ana_check_units",
             htmlOutput(NS("NCA", "ui_nca_ana_check_units"))),
           tags$br(),
           div(style="display:inline-block",
             "ui_nca_ana_units_time",
             htmlOutput(NS("NCA", "ui_nca_ana_units_time"))),
           div(style="display:inline-block",
             "ui_nca_ana_units_dose",
             htmlOutput(NS("NCA", "ui_nca_ana_units_dose"))),
           div(style="display:inline-block",
             "ui_nca_ana_units_conc",
             htmlOutput(NS("NCA", "ui_nca_ana_units_conc"))),
           div(style="display:inline-block",
             "ui_nca_ana_units_amt",
             htmlOutput(NS("NCA", "ui_nca_ana_units_amt"))),
             width=12)),
       fluidRow(
         tabBox(
           title = NULL,
           # The id lets us use input$tabset1 on the server to find the current tab
           id = "tabset1", # height = "250px",
           tabPanel(id="panel_figure", title=tagList(shiny::icon("chart-line"), "Figures"),
             tagList(
             "ui_nca_ana_results_fig",
             htmlOutput(NS("NCA", "ui_nca_ana_results_fig"))
             )
           ),
           tabPanel(id="panel_table",  title=tagList(shiny::icon("table"), "Tables"),   
             tagList(
             "ui_nca_ana_results_tab",
             htmlOutput(NS("NCA", "ui_nca_ana_results_tab"))
             )
           ),
           tabPanel(id="panel_config", title=tagList(shiny::icon("gear"), "Config."),  
            tagList(
             "ui_nca_ana_options",
             htmlOutput(NS("NCA", "ui_nca_ana_options"))
             )
             ),
       width=12)
           ),
       fluidRow(
         box(title="Generated Code",
           "ui_nca_code",
           shinyAce::aceEditor(NS("NCA", "ui_nca_code")), width=12)),
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

   #UD_test_mksession(session)
   DW_test_mksession(session)

 # # Module server
   NCA_Server(id="NCA", react_state=react_FM)
#
   # Current state outside of the module
   output$ui_state  =  renderText({
     uiele = paste(capture.output(str(react_FM[["NCA"]])), collapse="\n")
   uiele})
}

shinyApp(ui, server)
