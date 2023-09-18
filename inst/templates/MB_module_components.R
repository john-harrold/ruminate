#library(formods)
library(shinydashboard)

#https://fontawesome.com/icons?from=io

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title="MB Module Template"),
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
             "This app demonstrates how to use the MB
             module with each ui component isolated to make
             it easy to see the behavior.",
        fluidRow(
          box(title="Analysis Actions",
              div(style="display:inline-block",
                  "ui_mb_new_btn",
                  htmlOutput(NS("MB", "ui_mb_new_btn"))),
              div(style="display:inline-block",
                  "ui_mb_save_btn",
                  htmlOutput(NS("MB", "ui_mb_save_btn"))),
              div(style="display:inline-block",
                  "ui_mb_clip_code",
                  htmlOutput(NS("MB", "ui_mb_clip_code")) ),
              div(style="display:inline-block",
                  "ui_mb_del_btn",
                  htmlOutput(NS("MB", "ui_mb_del_btn"))),
              div(style="display:inline-block",
                  "ui_mb_copy_btn",
                  htmlOutput(NS("MB", "ui_mb_copy_btn"))),
              width = 12)
        ),
       width=12)),
       fluidRow(
         box(title="Select current element",
           "MB_ui_select_element",
           htmlOutput(NS("MB", "MB_ui_select_element")),
           "MB_ui_text_element_name",
           htmlOutput(NS("MB", "MB_ui_text_element_name")),
         ),
         box(title="Element",
           "MB_ui_element",
           htmlOutput(NS("MB", "MB_ui_element"))
         )
         ),
       fluidRow(
         box(title="Model Catalog",
           "select_model_catalog",
           htmlOutput(NS("MB", "select_model_catalog")),
         ),
         box(title="Element 2",
           "MB_ui_ele1",
           htmlOutput(NS("MB", "MB_ui_ele2"))
         )
         ),
       fluidRow(
         box(title="Messages",
           "ui_mb_msg",
           verbatimTextOutput(NS("MB", "ui_mb_msg")), width=12)),
       fluidRow(
         box(title="Generated Code",
           "ui_mb_code",
           shinyAce::aceEditor(NS("MB", "ui_mb_code")), width=12)),
       fluidRow(
         box(title="Current Module State",
           verbatimTextOutput("ui_state"),width=12))
       ),
       tabItem(tabName="compact",
          "ui_mb_compact",
          htmlOutput(NS("MB", "ui_mb_compact")))
      )
    )
  )

# Main app server
server <- function(input, output, session) {
  # Empty reactive object to track and react to
  # changes in the module state outside of the module
  react_FM = reactiveValues()

  # Module server
  MB_Server(id="MB", id_ASM = "ASM", react_state=react_FM)

  # Current state outside of the module
  output$ui_state  =  renderText({
    uiele = paste(capture.output(str(react_FM[["MB"]])), collapse="\n")
  uiele})
}

shinyApp(ui, server)
