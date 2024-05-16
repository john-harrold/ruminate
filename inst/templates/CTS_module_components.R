#library(formods)
library(shinydashboard)
library(rhandsontable)  

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="CTS Module Template"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Module UI Components",    tabName="appstate",  icon=icon("archive")),
       menuItem("Compact View",  tabName="compact", icon=icon("archive"))
     )
  ),
  dashboardBody(
    tabItems(
       tabItem(tabName="appstate",
       # Required for tooltips
       prompter::use_prompt(),
       fluidRow(
         box(title=NULL,
             "This app demonstrates how to use the CTS
             module with each ui component isolated to make
             it easy to see the behavior.",
        fluidRow(
          box(title="Analysis Actions",
              div(style="display:inline-block",
                  "ui_cts_new_btn",
                  htmlOutput(NS("CTS", "ui_cts_new_btn"))),
              div(style="display:inline-block",
                  "ui_cts_save_btn",
                  htmlOutput(NS("CTS", "ui_cts_save_btn"))),
              div(style="display:inline-block",
                  "ui_cts_clip_code",
                  htmlOutput(NS("CTS", "ui_cts_clip_code")) ),
              div(style="display:inline-block",
                  "ui_cts_del_btn",
                  htmlOutput(NS("CTS", "ui_cts_del_btn"))),
              div(style="display:inline-block",
                  "ui_cts_copy_btn",
                  htmlOutput(NS("CTS", "ui_cts_copy_btn"))),
              width = 12)
        ),
       width=12)),
       fluidRow(
         box(title="Select current element",
           "CTS_ui_select_element",
           htmlOutput(NS("CTS", "CTS_ui_select_element")),
           "CTS_ui_text_element_name",
           htmlOutput(NS("CTS", "CTS_ui_text_element_name")),
           width=6),
         box(title="Source Model",
           "CTS_ui_source_model",
           htmlOutput(NS("CTS", "CTS_ui_source_model")), 
           width=6)
         ),
       fluidRow(
         box(title="Trial Options",
           div(style="display:inline-block",
             "CTS_ui_nsub",           
             htmlOutput(NS("CTS", "CTS_ui_nsub"))), 
           div(style="display:inline-block",
             "CTS_ui_visit_times",           
             htmlOutput(NS("CTS", "CTS_ui_visit_times"))), 
           div(style="display:inline-block",
             "CTS_ui_trial_end",           
             htmlOutput(NS("CTS", "CTS_ui_trial_end"))), 
           width=12)
         ),
       fluidRow(
         box(title="Covariates",
           div(style="display:inline-block",
             "CTS_ui_covariates_none",           
             htmlOutput(NS("CTS", "CTS_ui_covariates_none"))), tags$br(),
           div(style="display:inline-block",
             "CTS_ui_covariates_selection",           
             htmlOutput(NS("CTS", "CTS_ui_covariates_selection"))), 
           div(style="display:inline-block",
             "CTS_ui_covariates_type",           
             htmlOutput(NS("CTS", "CTS_ui_covariates_type"))), 
           div(style="display:inline-block",
             "CTS_ui_covariates_value",           
             htmlOutput(NS("CTS", "CTS_ui_covariates_value"))), 
           div(style="display:inline-block",
             "CTS_ui_covariates_button",           
             htmlOutput(NS("CTS", "CTS_ui_covariates_button"))), 
           div(style="display:inline-block",
             "CTS_ui_covariates_table",           
             htmlOutput(NS("CTS", "CTS_ui_covariates_table"))), 
           width=12)
         ),
       fluidRow(
         box(title="Rules",
           "CTS_ui_select_rule_type",
           htmlOutput(NS("CTS", "CTS_ui_select_rule_type")), 
           "CTS_ui_rule_name",
           htmlOutput(NS("CTS", "CTS_ui_rule_name")), 
           "CTS_ui_add_rule_btn",
           htmlOutput(NS("CTS", "CTS_ui_add_rule_btn")), 
           "CTS_ui_rule_condition",
           htmlOutput(NS("CTS", "CTS_ui_rule_condition")), 
           width=12)
         ),
       fluidRow(
         box(title="Dosing",
           "CTS_ui_action_dosing_state",
           htmlOutput(NS("CTS", "CTS_ui_action_dosing_state")), 
           "CTS_ui_action_dosing_values",
           htmlOutput(NS("CTS", "CTS_ui_action_dosing_values")), 
           "CTS_ui_action_dosing_times",
           htmlOutput(NS("CTS", "CTS_ui_action_dosing_times")), 
           "CTS_ui_action_dosing_durations",
           htmlOutput(NS("CTS", "CTS_ui_action_dosing_durations")), 
           width=4),
         box(title="Set State",
           "CTS_ui_action_set_state_state",
           htmlOutput(NS("CTS", "CTS_ui_action_set_state_state")), 
           "CTS_ui_action_set_state_value",
           htmlOutput(NS("CTS", "CTS_ui_action_set_state_value")), 
           width=4),
         box(title="Manual",
           "CTS_ui_action_manual_code",
           htmlOutput(NS("CTS", "CTS_ui_action_manual_code")), 
           width=4)
         ),
       fluidRow(
         box(title="Current Rules",
             "hot_current_rules",
             rhandsontable::rHandsontableOutput(NS("CTS", "hot_current_rules")), width=12)),
       fluidRow(
         box(title="Simulation Results",
            "CTS_ui_simres",
            tags$br(),
            htmlOutput(NS("CTS", "CTS_ui_simres")) , width=12)),
       fluidRow(
         box(title="Configuration",
           "CTS_ui_sim_cfg",      
           htmlOutput(NS("CTS", "CTS_ui_sim_cfg")), 
           width=12)
         ),
       fluidRow(
         box(title="Messages",
           "ui_cts_msg",
           verbatimTextOutput(NS("CTS", "ui_cts_msg")), width=12)),
       fluidRow(
         box(title="Generated Code",
           "ui_cts_code",
           shinyAce::aceEditor(NS("CTS", "ui_cts_code")), width=12)),
       fluidRow(
         box(title="Current Module State",
           verbatimTextOutput("ui_state"),width=12))
       ),
       tabItem(tabName="compact",
          "CTS_ui_compact",
          htmlOutput(NS("CTS", "CTS_ui_compact")))
      )
    )
  )

# Main app server
server <- function(input, output, session) {

  #CTS_test_mksession(session, full_session=TRUE)
  #CTS_test_mksession(session, full_session=FALSE)

  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module server
  MB_Server(id="MB", id_ASM = "ASM", react_state=react_FM)

  # Module server
  CTS_Server(id="CTS", id_ASM = "ASM", id_MB = "MB", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["CTS"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
