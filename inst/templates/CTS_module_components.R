#library(formods)
library(shinydashboard)

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
         box(title="Simulation Environment",
           "CTS_ui_sim_env",           
           htmlOutput(NS("CTS", "CTS_ui_sim_env")), 
           width=12)
         ),
       fluidRow(
         box(title="Rules",
           "CTS_ui_select_rule_type",
           htmlOutput(NS("CTS", "CTS_ui_select_rule_type")), 
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

  CTS_test_mksession(session, full_session=TRUE)

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
