#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor

#'@export
#'@title Clinical Trial Simulator State Server
#'@description Server function for the Clinical Trial Simulator  Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM ID string for the app state managment module used to save and load app states
#'@param id_MB An ID string that corresponds with the ID used to call the MB modules
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return UD Server object
# JMH Add example
CTS_Server <- function(id,
               id_ASM        = "ASM",
               id_MB         = "MB",
               FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
               MOD_yaml_file = system.file(package = "ruminate",  "templates", "CTS.yaml"),
               deployed      = FALSE,
               react_state   = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Select the active cohort
    output$CTS_ui_select_element = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choices = list()
      for(element_id in names(state[["CTS"]][["elements"]])){
        choices[[ state[["CTS"]][["elements"]][[element_id]][["ui"]][["element_name"]] ]] = element_id
      }

      uiele =
      shinyWidgets::pickerInput(
        selected   = state[["CTS"]][["current_element"]],
        inputId    = NS(id, "element_selection"),
        label      = state[["MC"]][["labels"]][["current_element"]],
        choices    = choices,
        width      = state[["MC"]][["formatting"]][["current_element"]][["width"]])

      uiele})
    #------------------------------------
    # Select the active cohort
    output$CTS_ui_select_rule_type = renderUI({
      input$element_selection
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choices = list()
      subtext = c()

      for(rule_type in names(state[["MC"]][["formatting"]][["rule_type"]][["options"]])){
        choices[[
                 state[["MC"]][["formatting"]][["rule_type"]][["options"]][[rule_type]][["choice"]]
                 ]] =
                 state[["MC"]][["formatting"]][["rule_type"]][["options"]][[rule_type]][["value"]]

        subtext = c(subtext,
          state[["MC"]][["formatting"]][["rule_type"]][["options"]][[rule_type]][["subtext"]])
      }



      uiele =
      shinyWidgets::pickerInput(
        selected   = state[["CTS"]][["current_element"]],
        inputId    = NS(id, "rule_type"),
        label      = state[["MC"]][["labels"]][["rule_type"]],
        choices    = choices,
        choicesOpt = list( subtext = subtext),
        width      = state[["MC"]][["formatting"]][["rule_type"]][["width"]])
      uiele})
    #------------------------------------
    output$CTS_ui_rule_condition = renderUI({
      input$element_selection
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele =
        textAreaInput(
          inputId     = NS(id, "rule_condition"),
          label       = state[["MC"]][["labels"]][["rule_condition"]],
          width       = state[["MC"]][["formatting"]][["rule_condition"]][["width"]] ,
          height      = state[["MC"]][["formatting"]][["rule_condition"]][["height"]] ,
          value       = "",
          placeholder = state[["MC"]][["formatting"]][["rule_condition"]][["placeholder"]]
        )

      uiele = formods::FM_add_ui_tooltip(state, uiele,
        tooltip     = state[["MC"]][["formatting"]][["rule_condition"]][["tooltip"]],
        position    = state[["MC"]][["formatting"]][["rule_condition"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    output$CTS_ui_action_set_state_state = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "set state"){
        if(rx_details[["isgood"]]){
          value = current_ele[["ui"]][["action_set_state_state"]]
          if(!is.null(value)){
            if(!(value %in% rx_details[["elements"]][["states"]])){
              value = NULL
            }
          }
          uiele =
            shinyWidgets::pickerInput(
              selected   = NULL,
              inputId    = NS(id, "action_set_state_state"),
              label      = state[["MC"]][["labels"]][["action_set_state_state"]],
              choices    = rx_details[["elements"]][["states"]],
              width      = state[["MC"]][["formatting"]][["action_set_state_state"]][["width"]])
        }else{
          uiele = rx_details[["msgs"]]
        }
      }
      uiele})
    #------------------------------------
    output$CTS_ui_action_set_state_value = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "set state"){
        if(rx_details[["isgood"]]){
          uiele =
             textAreaInput(
               inputId     = NS(id, "action_set_state_value"),
               placeholder = state[["MC"]][["formatting"]][["action_set_state_value"]][["placeholder"]],
               width       = state[["MC"]][["formatting"]][["action_set_state_value"]][["width"]] ,
               value       = current_ele[["ui"]][["action_set_state_value"]],
               label       = state[["MC"]][["labels"]][["action_set_state_value"]]
             )
          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["action_set_state_value"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["action_set_state_value"]][["tooltip_position"]])
        }else{
          # The state UI will generate an error this will just return nothing
          uiele = NULL
        }
      }
      uiele})
    #------------------------------------
    output$CTS_ui_action_manual_code = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "manual"){
        if(rx_details[["isgood"]]){
          uiele =
             textAreaInput(
               inputId     = NS(id, "action_manual_code"),
               placeholder = state[["MC"]][["formatting"]][["action_manual_code"]][["placeholder"]],
               width       = state[["MC"]][["formatting"]][["action_manual_code"]][["width"]] ,
               height      = state[["MC"]][["formatting"]][["action_manual_code"]][["height"]] ,
               value       = current_ele[["ui"]][["action_manual_code"]],
               label       = state[["MC"]][["labels"]][["action_manual_code"]]
             )
          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["action_manual_code"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["action_manual_code"]][["tooltip_position"]])
        }else{
          # The state UI will generate an error this will just return nothing
          uiele = rx_details[["msgs"]]
        }
      }
      uiele})
    #------------------------------------
    output$CTS_ui_action_dosing_state = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "dose"){
        if(rx_details[["isgood"]]){
          value = current_ele[["ui"]][["action_dosing_state"]]
          if(!is.null(value)){
            if(!(value %in% rx_details[["elements"]][["states"]])){
              value = NULL
            }
          }
          uiele =
            shinyWidgets::pickerInput(
              selected   = NULL,
              inputId    = NS(id, "action_dosing_state"),
              label      = state[["MC"]][["labels"]][["action_dosing_state"]],
              choices    = rx_details[["elements"]][["states"]],
              width      = state[["MC"]][["formatting"]][["action_dosing_state"]][["width"]])
        }else{
          uiele = rx_details[["msgs"]]
        }
      }
      uiele})
    #------------------------------------
    output$CTS_ui_action_dosing_values = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "dose"){
        if(rx_details[["isgood"]]){
          uiele =
             textAreaInput(
               inputId     = NS(id, "action_dosing_values"),
               placeholder = state[["MC"]][["formatting"]][["action_dosing_values"]][["placeholder"]],
               width       = state[["MC"]][["formatting"]][["action_dosing_values"]][["width"]] ,
               value       = current_ele[["ui"]][["action_dosing_values"]],
               label       = state[["MC"]][["labels"]][["action_dosing_values"]]
             )
          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["action_dosing_values"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["action_dosing_values"]][["tooltip_position"]])
        }else{
          # The state UI will generate an error this will just return nothing
          uiele = NULL
        }
      }
      uiele})
    #------------------------------------
    output$CTS_ui_action_dosing_times = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "dose"){
        if(rx_details[["isgood"]]){
          uiele =
             textAreaInput(
               inputId     = NS(id, "action_dosing_times"),
               placeholder = state[["MC"]][["formatting"]][["action_dosing_times"]][["placeholder"]],
               width       = state[["MC"]][["formatting"]][["action_dosing_times"]][["width"]] ,
               value       = current_ele[["ui"]][["action_dosing_times"]],
               label       = state[["MC"]][["labels"]][["action_dosing_times"]]
             )

          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["action_dosing_times"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["action_dosing_times"]][["tooltip_position"]])

        }else{
          # The state UI will generate an error this will just return nothing
          uiele = NULL
        }
      }
      uiele})
    #------------------------------------
    output$CTS_ui_action_dosing_durations = renderUI({
      input$element_selection
      input$button_clk_save
      req(input$rule_type)
      state = CTS_fetch_state(id             = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele =NULL
      if(input$rule_type == "dose"){
        if(rx_details[["isgood"]]){
          uiele =
             textAreaInput(
               inputId     = NS(id, "action_dosing_durations"),
               placeholder = state[["MC"]][["formatting"]][["action_dosing_durations"]][["placeholder"]],
               width       = state[["MC"]][["formatting"]][["action_dosing_durations"]][["width"]] ,
               value       = current_ele[["ui"]][["action_dosing_durations"]],
               label       = state[["MC"]][["labels"]][["action_dosing_durations"]]
             )
          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["action_dosing_durations"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["action_dosing_durations"]][["tooltip_position"]])
        }else{
          # The state UI will generate an error this will just return nothing
          uiele = NULL
        }
      }
      uiele})
    #------------------------------------
    # Current cohort name:
    output$CTS_ui_text_element_name = renderUI({
      input$element_selection
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)

      uiele =
      textInput(
        inputId     = NS(id, "element_name"),
        label       = NULL,
        width       = state[["MC"]][["formatting"]][["element_name"]][["width"]] ,
        value       = current_ele[["ui"]][["element_name"]],
        placeholder = state[["MC"]][["labels"]][["element_name"]]
      )

      uiele})


    #------------------------------------
    # Create an empty UI for the source model. It will update based on the
    # observe function below it.
    output$CTS_ui_source_model = renderUI({
      input$element_selection
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choicesOpt = NULL
      uiele =
        shinyWidgets::pickerInput(
          selected   = "PH",
          inputId    = NS(id, "source_model"),
          label      = state[["MC"]][["labels"]][["source_model"]],
          choices    = c("PH"),
          width      = state[["MC"]][["formatting"]][["source_model"]][["width"]],
          choicesOpt = choicesOpt)

      uiele})
    #------------------------------------
    # Text description of simulation environment
    output$CTS_ui_sim_env      = renderUI({
      input$source_model
      input$button_clk_save
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]
      cmd = state[["MC"]][["rule_env_content"]]

      tcres =
      FM_tc(cmd = cmd,
            tc_env  = list(
              rx_details=rx_details,
              state = state),
            capture = c("uiele"))
      if(tcres[["isgood"]]){
        uiele = tcres[["capture"]][["uiele"]]
      } else {
        uiele = paste0(tcres[["msgs"]], collapse="<br/>")
      }

      uiele})
    #------------------------------------
    #  Simulation results
    output$CTS_ui_simres       = renderUI({
      input$element_selection
      input$button_clk_runsim
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele = NULL
      # Current simulation results
      simres = current_ele[["simres"]][["capture"]][[ current_ele[["simres_object_name"]] ]]

      #------------------------------------
      # This creates a switch to change from the interactive and report
      # views of tables and figures:
      choiceValues = c("report", "interactive")
      choiceNames  = c(state[["MC"]][["labels"]][["switch_output_report"]],
                       state[["MC"]][["labels"]][["switch_output_interactive"]])

      switch_selected = state[["CTS"]][["ui"]][["switch_output"]]
      if(!(switch_selected %in% choiceValues)){
        switch_selected = "report"
      }

      uiele_switch =
        shinyWidgets::radioGroupButtons(
         inputId       = NS(id, "switch_output"),
         label         = state[["MC"]][["labels"]][["switch_output"]],
         selected      = switch_selected,
         choiceValues  = choiceValues,
         choiceNames   = choiceNames ,
         status        = "primary")
      #------------------------------------
      uiele_res_tabs =
          shinydashboard::tabBox(
            width = 10,
            title = NULL,
            # The id lets us use input$tabset1 on the server to find the current tab
            shiny::tabPanel(id=NS(id, "tab_sim_env"),
                     title=tagList(shiny::icon("puzzle-piece"),
                                   state[["MC"]][["labels"]][["tab_sim_env"]]),
              htmlOutput(NS(id, "CTS_ui_sim_env"))
            ),
            shiny::tabPanel(id=NS(id, "tab_res_tc_figure"),
                     title=tagList(shiny::icon("chart-line"),
                                   state[["MC"]][["labels"]][["tab_res_tc_figure"]]),
              htmlOutput(NS(id, "ui_res_tc_figure"))
            ),
            shiny::tabPanel(id=NS(id, "tab_res_event_figure"),
                     title=tagList(shiny::icon("chart-line"),
                                   state[["MC"]][["labels"]][["tab_res_events_figure"]]),
              htmlOutput(NS(id, "ui_res_events_figure"))
            )
          )


      uiele_btn_update = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_update_plot"),
                label   = state[["MC"]][["labels"]][["update_plot_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_update_plot"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_update_plot"]][["block"]],
                color   = "primary",
                icon    = icon("circle-play"))

      # Optinally adding the tooltip:
      uiele_btn_update =
        formods::FM_add_ui_tooltip(state, uiele_btn_update,
               tooltip     = state[["MC"]][["formatting"]][["button_clk_update_plot"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_update_plot"]][["tooltip_position"]])


      # Creating the pulldown for the plot output dimensions
      choices = list()
      for(dim_id in names(state[["MC"]][["formatting"]][["tc_dim"]][["choices"]])){
        choices[[ state[["MC"]][["formatting"]][["tc_dim"]][["choices"]][[dim_id]][["verb"]] ]] = dim_id
      }

      uiele_tc_dim =
        shinyWidgets::pickerInput(
          selected   = current_ele[["ui"]][["tc_dim"]],
          inputId    = NS(id, "tc_dim"),
          label      = state[["MC"]][["labels"]][["tc_dim"]],
          choices    = choices,
          width      = state[["MC"]][["formatting"]][["tc_dim"]][["width"]])

      uiele_tc_dim =
        formods::FM_add_ui_tooltip(state, uiele_tc_dim,
               tooltip     = state[["MC"]][["formatting"]][["tc_dim"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["tc_dim"]][["tooltip_position"]])

      # Figure page selection
      uiele_fpage = htmlOutput(NS(id, "CTS_ui_fpage"))

      # DV cols selection
      choices = list()
      all_values = c()
      for(etype in names(state[["MC"]][["formatting"]][["dvcols"]][["choices"]])){
        verb   = state[["MC"]][["formatting"]][["dvcols"]][["choices"]][[etype]][["verb"]]
        values = rx_details[["elements"]][[etype]]
        if(!is.null(values)){
          values = values[(values %in% names(simres[["simall"]]))]
          if(length(values) > 0){
            choices[[ verb ]] = values
            all_values = c(all_values, values)
          }
        }
      }


      selected = current_ele[["ui"]][["dvcols"]]

      # If selected gets confused because of changes in the model this will
      # just default to the first output
      if(!all(selected %in% all_values)){
        selected = all_values[1]
      }

      uiele_dvcols =
        shinyWidgets::pickerInput(
          selected   = selected,
          multiple   = TRUE,
          inputId    = NS(id, "dvcols"),
          label      = state[["MC"]][["labels"]][["dvcols"]],
          choices    = choices,
          options = list(
           size      = state[["MC"]][["formatting"]][["dvcols"]][["size"]]),
          width      = state[["MC"]][["formatting"]][["dvcols"]][["width"]])

      uiele_dvcols =
        formods::FM_add_ui_tooltip(state, uiele_dvcols,
               tooltip     = state[["MC"]][["formatting"]][["dvcols"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["dvcols"]][["tooltip_position"]])

      # EV ids to plot
      uiele_evplot =  NULL
      choices = list()
      all_values = c()
      for(etype in names(state[["MC"]][["formatting"]][["evplot"]][["choices"]])){
        verb   = state[["MC"]][["formatting"]][["evplot"]][["choices"]][[etype]][["verb"]]
        value  = state[["MC"]][["formatting"]][["evplot"]][["choices"]][[etype]][["value"]]
        choices[[ verb ]] = value
      }
      selected = current_ele[["ui"]][["evplot"]]

      uiele_evplot =
        shinyWidgets::pickerInput(
          selected   = selected,
          multiple   = TRUE,
          inputId    = NS(id, "evplot"),
          label      = state[["MC"]][["labels"]][["evplot"]],
          choices    = choices,
          width      = state[["MC"]][["formatting"]][["evplot"]][["width"]])

      uiele_evplot =
        formods::FM_add_ui_tooltip(state, uiele_evplot,
               tooltip     = state[["MC"]][["formatting"]][["evplot"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["evplot"]][["tooltip_position"]])

      uiele_btn_runsim = htmlOutput(NS(id, "ui_cts_runsim_btn"))

      div_style ="display:inline-block;vertical-align:top;align-items:center"
      uiele_btn_update = div(style=div_style,     uiele_btn_update)
      uiele_btn_runsim = div(style=div_style,     uiele_btn_runsim)
      uiele_switch     = div(style=div_style,     uiele_switch)
      uiele_tc_dim     = div(style=div_style,     uiele_tc_dim)
      uiele_fpage      = div(style=div_style,     uiele_fpage)
      uiele_dvcols     = div(style=div_style,     uiele_dvcols)
      uiele_evplot     = div(style=div_style,     uiele_evplot)

      div_style= "width:80%;display:flex;align-items:center;justify-content:center"

      uiele_top_btn_row = 
        div(style=div_style,
          uiele_btn_update,
          HTML('&nbsp;'),
          uiele_tc_dim,
          HTML('&nbsp;'),
          uiele_fpage,
          HTML('&nbsp;'),
          uiele_dvcols,
          HTML('&nbsp;'),
          uiele_evplot)

      uiele_switch  = 
        div(style=div_style,
            uiele_btn_runsim,
            HTML('&nbsp;'),
            uiele_switch
            )

      uiele = tagList(
        uiele_top_btn_row,
        tags$br(),
        uiele_res_tabs,
        tags$br(),
        uiele_switch )
      uiele})
    #------------------------------------
    # The fpage is rendered separately so it can responnd to the update_plot
    # button clicks separately
    output$CTS_ui_fpage        = renderUI({
      input$element_selection
      input$button_clk_runsim
      input$button_clk_update_plot
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      rx_details = current_ele[["rx_details"]]

      uiele_fpage = NULL

      if(!is.null(current_ele[["plotres"]])){
      if(!is.null(current_ele[["isgood"]])){
      if(current_ele[["isgood"]]){
        plotres = current_ele[["plotres"]][["capture"]][[ current_ele[["fgtc_object_name"]] ]]
        npages = plotres[["npages"]]
        # If there isn't a value in selected we select the first one
        if(is.null(current_ele[["ui"]][["fpage"]])){
          selected = 1
        } else {
          selected = current_ele[["ui"]][["fpage"]]
        }
        # This will catch the case where we previously had more figures than
        # we currently do
        if(as.numeric(as.character(selected)) > npages){
          selected = 1
        }
        choices = c(1:npages)

        uiele_fpage =
          shinyWidgets::pickerInput(
            selected   = current_ele[["ui"]][["fpage"]],
            inputId    = NS(id, "fpage"),
            label      = state[["MC"]][["labels"]][["fpage"]],
            choices    = choices,
            width      = state[["MC"]][["formatting"]][["fpage"]][["width"]])

        uiele_fpage =
          formods::FM_add_ui_tooltip(state, uiele_fpage,
                 tooltip     = state[["MC"]][["formatting"]][["fpage"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["fpage"]][["tooltip_position"]])

      }
      }
      }
      uiele_fpage})
    #------------------------------------
    output$ui_res_tc_figure      = renderUI({
      input$element_selection
      input$button_clk_runsim
      input$switch_output
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)

      figs_found = FALSE
      uiele      = state[["MC"]][["errors"]][["cts_no_fig"]]

      simgood    = TRUE
      if(is.null(current_ele[["simres"]])){
        simgood = FALSE
        uiele = state[["MC"]][["errors"]][["no_sim_found"]]
      } else if(!current_ele[["simres"]][["isgood"]]){
        uiele = uiele = state[["MC"]][["errors"]][["bad_sim"]]
        simgood = FALSE
      }

      if(simgood){
        if("plotres" %in% names(current_ele)){
           if(current_ele[["plotres"]][["isgood"]]){
             figs_found = TRUE
           }else{
             uiele = current_ele[["plotres"]][["msgs"]]
           }
        }
        
        if(figs_found){
        
          pvw          = state[["MC"]][["formatting"]][["preview"]][["width"]]
          pvh          = state[["MC"]][["formatting"]][["preview"]][["height"]]
          pv_div_style = paste0("height:",pvh,";width:",pvw,";display:inline-block;vertical-align:top")
        
          output_type = state[["CTS"]][["ui"]][["switch_output"]]
          if(output_type == "report"){
            uiele =
               div(style=pv_div_style,
                   plotOutput(
                     NS(id, "ui_res_tc_figure_ggplot"),
                     width=pvw, height=pvh))
          } else {
            uiele =
               div(style=pv_div_style,
                   plotly::plotlyOutput(
                     NS(id, "ui_rec_tc_figure_plotly"),
                     width=pvw, height=pvh))
          }
        }
      }
      uiele})
    #------------------------------------
    # timecourse plotly
    output$ui_res_tc_figure_plotly  = plotly::renderPlotly({
      input$element_selection
      input$button_clk_runsim
      input$button_clk_update_plot
      input$switch_output
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)
      uiele = NULL
      figs_found = FALSE
      msgs       = state[["MC"]][["errors"]][["cts_no_fig"]]

      if("plotres" %in% names(current_ele)){
         if(current_ele[["plotres"]][["isgood"]]){
           figs_found = TRUE
         }
      }

      if(figs_found){
        fig_obj = current_ele[["fgtc_object_name"]]
        fig     = current_ele[["plotres"]][["capture"]][[fig_obj]][["fig"]]
      } else {
        fig     = FM_mk_error_fig(msgs)
      }

      uiele = plotly::ggplotly(fig)

      uiele})
    # timecourse ggplot
    output$ui_res_tc_figure_ggplot             = renderPlot({
      input$element_selection
      input$button_clk_runsim
      input$button_clk_update_plot
      input$switch_output
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)
      uiele = NULL
      figs_found = FALSE
      msgs       = state[["MC"]][["errors"]][["cts_no_fig"]]

      if("plotres" %in% names(current_ele)){
         if(current_ele[["plotres"]][["isgood"]]){
           figs_found = TRUE
         }
      }

      if(figs_found){
        fig_obj = current_ele[["fgtc_object_name"]]
        uiele = current_ele[["plotres"]][["capture"]][[fig_obj]][["fig"]]
      } else {
        uiele = FM_mk_error_fig(msgs)
      }

      uiele})
    #------------------------------------
    # Events figure
    output$ui_res_events_figure      = renderUI({
      input$element_selection
      input$button_clk_runsim
      input$switch_output
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)

      figs_found = FALSE
      uiele      = state[["MC"]][["errors"]][["cts_no_fig"]]

      simgood    = TRUE
      if(is.null(current_ele[["simres"]])){
        simgood = FALSE
        uiele = state[["MC"]][["errors"]][["no_sim_found"]]
      } else if(!current_ele[["simres"]][["isgood"]]){
        uiele = uiele = state[["MC"]][["errors"]][["bad_sim"]]
        simgood = FALSE
      }

      if(simgood){
        if("plotres" %in% names(current_ele)){
           if(current_ele[["plotres"]][["isgood"]]){
             figs_found = TRUE
           }else{
             uiele = current_ele[["plotres"]][["msgs"]]
           }
        }
        
        if(figs_found){
          pvw          = state[["MC"]][["formatting"]][["preview"]][["width"]]
          pvh          = state[["MC"]][["formatting"]][["preview"]][["height"]]
          pv_div_style = paste0("height:",pvh,";width:",pvw,";display:inline-block;vertical-align:top")
        
          output_type = state[["CTS"]][["ui"]][["switch_output"]]
          if(output_type == "report"){
            uiele =
               div(style=pv_div_style,
                   plotOutput(
                     NS(id, "ui_res_events_figure_ggplot"),
                     width=pvw, height=pvh))
          } else {
            uiele =
               div(style=pv_div_style,
                   plotly::plotlyOutput(
                     NS(id, "ui_rec_events_figure_plotly"),
                     width=pvw, height=pvh))
          }
        }
      }
      uiele})
    #------------------------------------
    # events plotly
    output$ui_res_events_figure_plotly  = plotly::renderPlotly({
      input$element_selection
      input$button_clk_runsim
      input$button_clk_update_plot
      input$switch_output
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)
      uiele = NULL
      figs_found = FALSE
      msgs       = state[["MC"]][["errors"]][["cts_no_fig"]]

      if("plotres" %in% names(current_ele)){
         if(current_ele[["plotres"]][["isgood"]]){
           figs_found = TRUE
         }
      }

      if(figs_found){
        fig_obj = current_ele[["fgev_object_name"]]
        fig = current_ele[["plotres"]][["capture"]][[fig_obj]][["fig"]]
      } else {
        fig = FM_mk_error_fig(msgs)
      }

      uiele = plotly::ggplotly(fig)

      uiele})
    # events ggplot
    output$ui_res_events_figure_ggplot             = renderPlot({
      input$element_selection
      input$button_clk_runsim
      input$button_clk_update_plot
      input$switch_output
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)
      uiele = NULL
      figs_found = FALSE
      msgs       = state[["MC"]][["errors"]][["cts_no_fig"]]

      if("plotres" %in% names(current_ele)){
         if(current_ele[["plotres"]][["isgood"]]){
           figs_found = TRUE
         }
      }

      if(figs_found){
        fig_obj = current_ele[["fgev_object_name"]]
        uiele = current_ele[["plotres"]][["capture"]][[fig_obj]][["fig"]]
      } else {
        uiele = FM_mk_error_fig(msgs)
      }

      uiele})
    #------------------------------------
    # Configuration options
    output$CTS_ui_sim_cfg   = renderUI({
      input$element_selection
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      uiele = NULL

      sc_meta     = state[["CTS"]][["sc_meta"]]
      cfg_summary = sc_meta[["cfg_summary"]]

      groups =  unique(cfg_summary[["group"]])

      for(gname in groups){
        group_summary = cfg_summary[ cfg_summary[["group"]] == gname, ]
        group_ele = NULL

        # Walking through each configuration option for the current grup
        cnames = sort(group_summary[["name"]])
        for(cname in cnames){
          tmp_cfg_ui_ele = NULL
          tmp_cfg_ui_id  = sc_meta[["config"]][[cname]][["ui"]]

          # This is the value of current elements
          tmp_cfg_val    =  current_ele[["ui"]][[tmp_cfg_ui_id]]

          if(sc_meta[["config"]][[cname]][["uitype"]] == "text"){
            tmp_cfg_ui_ele =
              textInput(
                inputId     = NS(id, tmp_cfg_ui_id),
                label       = sc_meta[["config"]][[cname]][["label"]],
                placeholder = sc_meta[["config"]][[cname]][["placeholder"]],
                width       = state[["MC"]][["formatting"]][["config_text"]][["width"]],
                value       = tmp_cfg_val)
          } else if(sc_meta[["config"]][[cname]][["uitype"]] == "select"){
            # JMH  add selection ui

            tmp_cfg_ui_ele =
              shinyWidgets::pickerInput(
                inputId     = NS(id, tmp_cfg_ui_id),
                selected   = tmp_cfg_val,
                width      = state[["MC"]][["formatting"]][["config_select"]][["width"]],
                label      = sc_meta[["config"]][[cname]][["label"]],
                choices    = sc_meta[["config"]][[cname]][["choices"]])

          } else if(sc_meta[["config"]][[cname]][["uitype"]] == "textarea"){
            tmp_cfg_ui_ele =
              textAreaInput(
                inputId     = NS(id, tmp_cfg_ui_id),
                label       = sc_meta[["config"]][[cname]][["label"]],
                placeholder = sc_meta[["config"]][[cname]][["placeholder"]],
                width       = state[["MC"]][["formatting"]][["config_textarea"]][["width"]],
                height      = state[["MC"]][["formatting"]][["config_textarea"]][["height"]],
                value       = tmp_cfg_val)

          } else{
            FM_le(state, paste0("Uknown config uitype: ",
                                sc_meta[["config"]][[cname]][["uitype"]],
                                " for config option: ",
                                cname
                                ), entry_type="warning")
          }


          #adding the tool tip
          if(!is.null(tmp_cfg_ui_ele)){
            tmp_cfg_ui_ele =
              FM_add_ui_tooltip(
                state,
                tmp_cfg_ui_ele,
                tooltip = sc_meta[["config"]][[cname]][["tooltip"]]
              )
          }

          # building out the group elements
          group_ele = tagList(group_ele,
            div(style="display:inline-block;vertical-align:top", tmp_cfg_ui_ele))
        }

        # Adding the current group as a tab:
        uiele = tagList(uiele,tags$h3(gname), group_ele)

      }
      # JMH create config ui here


      uiele})
    #------------------------------------
    # Configuration options
    output$CTS_ui_add_rule_btn   = renderUI({
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele =
          shinyWidgets::actionBttn(
            inputId  = NS(id, "button_clk_add_rule"),
             label   = state[["MC"]][["labels"]][["add_rule_btn"]],
             style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
             size    = state[["MC"]][["formatting"]][["button_clk_add_rule"]][["size"]],
             block   = state[["MC"]][["formatting"]][["button_clk_add_rule"]][["block"]],
             color   = "success",
             icon    = icon("plus-sign", lib="glyphicon")
             )



      uiele})
    #------------------------------------
    output$hot_current_rules =  rhandsontable::renderRHandsontable({
      input$button_clk_add_rule
      input$element_selection
      input$hot_current_rules
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      current_ele = CTS_fetch_current_element(state)

      # By default there are no rules:
      ctdf = data.frame(Rules=state[["MC"]][["formatting"]][["hot_current_rules"]][["no_rules"]])

      # Now we look at the table tracking rules to see if it exists and if it
      # has at lest one rule
      if(!is.null(current_ele[["components_table"]])){
        if(nrow(current_ele[["components_table"]]) > 0){
          ctdf = current_ele[["components_table"]]
          ctdf[["rule_id"]] = as.factor(ctdf[["rule_id"]])
          ctdf[["hash"]] = NULL
        }
      }

     uiele = rhandsontable::rhandsontable(
       ctdf,
       width      = state[["MC"]][["formatting"]][["hot_current_rules"]][["width"]],
       height     = state[["MC"]][["formatting"]][["hot_current_rules"]][["height"]],
       rowHeaders = NULL
       )
    uiele })
    #------------------------------------
    output$hot_current_covariates =  rhandsontable::renderRHandsontable({
      input$element_selection
      input$button_clk_save
      input$button_clk_add_cov
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)


    if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) > 0){
      ccdf = NULL
      for(tmp_cov in current_ele[["rx_details"]][["elements"]][["covariates"]]){
        tmp_details = state[["MC"]][["formatting"]][["hot_current_covariates"]][["no_covariates"]]

        if(tmp_cov %in% names(current_ele[["covariates"]])){
          type     = current_ele[["covariates"]][[tmp_cov]][["type"]]
          sampling = current_ele[["covariates"]][[tmp_cov]][["sampling"]]
          values   = current_ele[["covariates"]][[tmp_cov]][["values"]]
          if(is.null(sampling)){
            tmp_details = paste0(type, ": ", paste0(values, collapse=", "))
          } else {
            tmp_details = paste0(type, ", ", sampling, ": ", paste0(values, collapse=", "))
          }
        }


        ccdf = rbind(ccdf,
          data.frame(Covariate = tmp_cov,
                     Details   = tmp_details))

      }

      uiele = rhandsontable::rhandsontable(
        ccdf,
        width      = state[["MC"]][["formatting"]][["hot_current_covariates"]][["width"]],
        height     = state[["MC"]][["formatting"]][["hot_current_covariates"]][["height"]],
        rowHeaders = NULL
        )
    } else{
      uiele = NULL
    }

    uiele })
    #------------------------------------
    output$CTS_ui_trial_end =  renderUI({
      input$element_selection
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
    current_ele = CTS_fetch_current_element(state)
    uiele =
      textInput(
        inputId     = NS(id, "trial_end"),
        label       = state[["MC"]][["labels"]][["trial_end"]],
        width       = state[["MC"]][["formatting"]][["trial_end"]][["width"]] ,
        value       = current_ele[["ui"]][["trial_end"]],
        placeholder = state[["MC"]][["formatting"]][["trial_end"]][["placeholder"]]
      )
    uiele = formods::FM_add_ui_tooltip(state, uiele,
                                       tooltip     = state[["MC"]][["formatting"]][["trial_end"]][["tooltip"]],
                                       position    = state[["MC"]][["formatting"]][["trial_end"]][["tooltip_position"]])
    uiele })
    #------------------------------------
    output$CTS_ui_visit_times =  renderUI({
      input$element_selection
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
    current_ele = CTS_fetch_current_element(state)
    uiele =
      textInput(
        inputId     = NS(id, "visit_times"),
        label       = state[["MC"]][["labels"]][["visit_times"]],
        width       = state[["MC"]][["formatting"]][["visit_times"]][["width"]] ,
        value       = current_ele[["ui"]][["visit_times"]],
        placeholder = state[["MC"]][["formatting"]][["visit_times"]][["placeholder"]]
      )
    uiele = formods::FM_add_ui_tooltip(state, uiele,
                                       tooltip     = state[["MC"]][["formatting"]][["visit_times"]][["tooltip"]],
                                       position    = state[["MC"]][["formatting"]][["visit_times"]][["tooltip_position"]])
    uiele })
    #------------------------------------
    output$CTS_ui_nsub        =  renderUI({
      input$element_selection
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
    current_ele = CTS_fetch_current_element(state)
    uiele =
    textInput(
      inputId     = NS(id, "nsub"),
      label       = state[["MC"]][["labels"]][["nsub"]],
      width       = state[["MC"]][["formatting"]][["nsub"]][["width"]] ,
      value       = current_ele[["ui"]][["nsub"]],
      placeholder = state[["MC"]][["formatting"]][["nsub"]][["placeholder"]]
    )

    uiele = formods::FM_add_ui_tooltip(state, uiele,
                                       tooltip     = state[["MC"]][["formatting"]][["nsub"]][["tooltip"]],
                                       position    = state[["MC"]][["formatting"]][["nsub"]][["tooltip_position"]])
    uiele })
    #------------------------------------
    # No covariate found message
    output$CTS_ui_covariates_none  =  renderUI({
      input$element_selection
      input$button_clk_save
      input$button_clk_add_cov

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
    current_ele = CTS_fetch_current_element(state)
    uiele = NULL
    if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) == 0){
      uiele = tags$em(state[["MC"]][["formatting"]][["covariates"]][["none_found"]])
    }
    uiele })
    #------------------------------------
    # Covariate selection
    output$CTS_ui_covariates_selection  =  renderUI({
      input$element_selection
      input$button_clk_save

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
    current_ele = CTS_fetch_current_element(state)


    uiele = NULL
    if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) > 0){
      uiele =
      shinyWidgets::pickerInput(
        selected   = state[["CTS"]][["ui"]][["selected_covariate"]],
        inputId    = NS(id, "selected_covariate"),
        label      = state[["MC"]][["labels"]][["selected_covariate"]],
        choices    = current_ele[["rx_details"]][["elements"]][["covariates"]],
        width      = state[["MC"]][["formatting"]][["selected_covariate"]][["width"]])

      uiele = formods::FM_add_ui_tooltip(state, uiele,
        tooltip     = state[["MC"]][["formatting"]][["selected_covariate"]][["tooltip"]],
        position    = state[["MC"]][["formatting"]][["selected_covariate"]][["tooltip_position"]])
    }
    uiele })
    #------------------------------------
    # Covariate type
    output$CTS_ui_covariates_type       =  renderUI({
      input$element_selection
      input$button_clk_save

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
    current_ele = CTS_fetch_current_element(state)


    uiele = NULL
    if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) > 0){
      if(state[["CTS"]][["ui"]][["covariate_type_selected"]] %in% names(state[["MC"]][["covariate_generation"]][["types"]])){
        covariate_type = state[["CTS"]][["ui"]][["covariate_type_selected"]]
      } else {
        covariate_type = names(state[["MC"]][["covariate_generation"]][["types"]])[1]
      }

      choices = list()
      for(tmp_cov_type in names(state[["MC"]][["covariate_generation"]][["types"]])){
        choices[[
                 state[["MC"]][["covariate_generation"]][["types"]][[tmp_cov_type]][["choice"]]
                 ]] = tmp_cov_type
      }

      uiele =
      shinyWidgets::pickerInput(
        selected   = covariate_type,
        inputId    = NS(id, "covariate_type_selected"),
        label      = state[["MC"]][["labels"]][["covariate_type"]],
        choices    = choices,
        width      = state[["MC"]][["covariate_generation"]][["width"]])
    }
    uiele })
    #------------------------------------
    # Covariate value
    output$CTS_ui_covariates_value       =  renderUI({
      input$element_selection
      input$button_clk_save
      input$covariate_type_selected

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

    current_ele = CTS_fetch_current_element(state)

    uiele = NULL
    if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) > 0){

      if(state[["CTS"]][["ui"]][["covariate_type_selected"]] %in% names(state[["MC"]][["covariate_generation"]][["types"]])){
        covariate_type = state[["CTS"]][["ui"]][["covariate_type_selected"]]
      } else {
        covariate_type = names(state[["MC"]][["covariate_generation"]][["types"]])[1]
      }

      uiele =
      textInput(
        inputId     = NS(id, "covariate_value"),
        label       = state[["MC"]][["labels"]][["covariate_value"]],
        placeholder = state[["MC"]][["covariate_generation"]][["types"]][[covariate_type]][["placeholder"]],
        width       = state[["MC"]][["covariate_generation"]][["width"]])

      uiele = formods::FM_add_ui_tooltip(state, uiele,
        tooltip     = state[["MC"]][["covariate_generation"]][["types"]][[covariate_type]][["tooltip"]],
        position    = state[["MC"]][["covariate_generation"]][["tooltip_position"]])
    }
    uiele })
    #------------------------------------
    # Covariate button
    output$CTS_ui_covariates_button      =  renderUI({
      input$element_selection
      input$button_clk_save

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)


      uiele = NULL
      if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) > 0){
       uiele = shinyWidgets::actionBttn(
         inputId = NS(id, "button_clk_add_cov"),
         label   = state[["MC"]][["labels"]][["add_cov_btn"]],
         style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
         size    = state[["MC"]][["formatting"]][["button_clk_add_cov"]][["size"]],
         block   = state[["MC"]][["formatting"]][["button_clk_add_cov"]][["block"]],
         color   = "success",
         icon    = icon("plus-sign", lib="glyphicon"))
      }
    uiele })
    #------------------------------------
    # Covariate table
    output$CTS_ui_covariates_table       =  renderUI({
      input$element_selection
      input$button_clk_save
      input$button_clk_add_cov

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      current_ele = CTS_fetch_current_element(state)


      uiele = NULL
      if(length(current_ele[["rx_details"]][["elements"]][["covariates"]]) > 0){
        uiele =
          rhandsontable::rHandsontableOutput(NS(id, "hot_current_covariates"),
             width  = state[["MC"]][["formatting"]][["hot_current_covariates"]][["width"]],
             height = state[["MC"]][["formatting"]][["hot_current_covariates"]][["height"]])
      }
    uiele })
    #------------------------------------
    # Current rule name:
    output$CTS_ui_rule_name = renderUI({
      input$element_selection
      input$button_clk_add_rule
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)

      rule_name = state[["CTS"]][["ui"]][["rule_name"]]
      if(is.null(rule_name)){
        rule_name = state[["MC"]][["formatting"]][["rule_name"]][["placeholder"]]
      } else if (rule_name == ""){
        rule_name = state[["MC"]][["formatting"]][["rule_name"]][["placeholder"]]
      }

      uiele =
      textInput(
        inputId     = NS(id, "rule_name"),
        label       = state[["MC"]][["labels"]][["rule_name"]],
        width       = state[["MC"]][["formatting"]][["rule_name"]][["width"]] ,
        value       = rule_name,
        placeholder = state[["MC"]][["formatting"]][["rule_name"]][["placeholder"]]
      )

      uiele = formods::FM_add_ui_tooltip(state, uiele,
        tooltip     = state[["MC"]][["formatting"]][["rule_name"]][["tooltip"]],
        position    = state[["MC"]][["formatting"]][["rule_name"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # ui bottom
    #------------------------------------
    # This forces the model selection to update
    observe({
      input$element_selection
      input$button_clk_save
      react_state[[id_MB]]
      react_state[[id_ASM]]
      # Forcing a reaction to changes in other modules
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_cht = CTS_fetch_current_element(state)

      # This only updates if there are models
      if( !is.null(state[["CTS"]][["MDL"]][["hasmdl"]]) ){
      if( state[["CTS"]][["MDL"]][["hasmdl"]] ){

        catalog = state[["CTS"]][["MDL"]][["catalog"]]

        if(current_cht[["ui"]][["source_model"]] %in% catalog[["object"]]){
          current_source_model =  current_cht[["ui"]][["source_model"]]
        } else {
          current_source_model = catalog[["object"]][1]
          FM_le(state, paste0("source_model: model missing missing."   ))
          FM_le(state, paste0("key: ",            current_cht[["id"]]       ))
          FM_le(state, paste0("source_model: ",   current_cht[["ui"]][["source_model"]]))
          FM_le(state, paste0("switching model:", current_source_model ))
        }

        choices        = catalog[["object"]]
        names(choices) = catalog[["label"]]

        choicesOpt = NULL
        shinyWidgets::updatePickerInput(
          session    = session,
          selected   = current_source_model,
          inputId    = "source_model",
          choices    = choices,
          choicesOpt = choicesOpt)
      }
      }
    })
    #------------------------------------
    # Generated data reading code
    observe({
      input$element_selection
      input$rule_condition
      input$action_dosing_state
      input$action_dosing_values
      input$action_dosing_times
      input$action_dosing_durations
      input$action_set_state_state
      input$action_set_state_value
      input$action_manual_code
      input$button_clk_add_rule
      input$button_clk_update_plot
      input$hot_current_rules
      input$nsub
      input$visit_times
      input$button_clk_add_cov
      input$fpage
      input$tc_dim
      input$dvcols
      input$evplot
      # Reacting to file changes
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = CTS_fetch_current_element(state)
      if(is.null(current_ele[["code"]])){
        uiele = "# No code to generate"
      } else {
        uiele = current_ele[["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_cts_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # Copying element code to the clipboard
    observeEvent(input$button_clk_clip, {
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # This is all conditional on the whether clipr is installed $
      # and if the app isn't deployed
      if((system.file(package="clipr") != "") &
         !deployed){

          # Pulling out the current element
          current_ele = CTS_fetch_current_element(state)
          uiele = current_ele[["code"]]

          clipr::write_clip(uiele)
        }
    })
    #------------------------------------
    # Side buttons:
    # new
    output$ui_cts_new_btn = renderUI({
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
        inputId = NS(id, "button_clk_new"),
        label   = state[["MC"]][["labels"]][["new_btn"]],
        style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
        size    = state[["MC"]][["formatting"]][["button_clk_new"]][["size"]],
        block   = state[["MC"]][["formatting"]][["button_clk_new"]][["block"]],
        color   = "success",
        icon    = icon("plus"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
                                         tooltip     = state[["MC"]][["formatting"]][["button_clk_new"]][["tooltip"]],
                                         position    = state[["MC"]][["formatting"]][["button_clk_new"]][["tooltip_position"]])

      uiele})

    #------------------------------------
    # Save
    output$ui_cts_save_btn = renderUI({
      state = CTS_fetch_state(id        = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_save"),
                label   = state[["MC"]][["labels"]][["save_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_save"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_save"]][["block"]],
                color   = "primary",
                icon    = icon("arrow-down"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip     = state[["MC"]][["formatting"]][["button_clk_save"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_save"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # run simulation
    output$ui_cts_runsim_btn = renderUI({
      state = CTS_fetch_state(id        = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_runsim"),
                label   = state[["MC"]][["labels"]][["runsim_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_runsim"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_runsim"]][["block"]],
                color   = "primary",
                icon    = icon("circle-play"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip     = state[["MC"]][["formatting"]][["button_clk_runsim"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_runsim"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # clip code
    output$ui_cts_clip_code = renderUI({
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = NULL
      if((system.file(package="clipr") != "") & !deployed){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_clk_clip"),
                  label   = state[["MC"]][["labels"]][["clip_btn"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_clk_clip"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_clk_clip"]][["block"]],
                  color   = "royal",
                  icon    = icon("clipboard", lib="font-awesome"))
        # Optinally adding the tooltip:
        uiele = formods::FM_add_ui_tooltip(state, uiele,
                 tooltip             = state[["MC"]][["formatting"]][["button_clk_clip"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_clk_clip"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    # delete
    output$ui_cts_del_btn   = renderUI({
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_del"),
                label   = state[["MC"]][["labels"]][["del_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_del"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_del"]][["block"]],
                color   = "danger",
                icon    = icon("minus"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip     = state[["MC"]][["formatting"]][["button_clk_del"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_del"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    # copy
    output$ui_cts_copy_btn   = renderUI({
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_clk_copy"),
                label   = state[["MC"]][["labels"]][["copy_btn"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_clk_copy"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_clk_copy"]][["block"]],
                color   = "royal",
                icon    = icon("copy"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip             = state[["MC"]][["formatting"]][["button_clk_copy"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_clk_copy"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    # User messages:
    output$ui_cts_msg = renderText({
      input$element_selection
      input$rule_condition
      input$action_dosing_state
      input$action_dosing_values
      input$action_dosing_times
      input$action_dosing_durations
      input$action_set_state_state
      input$action_set_state_value
      input$action_manual_code
      input$button_clk_add_rule
      input$hot_current_rules
      input$nsub
      input$visit_times
      input$button_clk_add_cov
      input$button_clk_runsim
      input$button_clk_update_plot
      input$switch_output

      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = state[["CTS"]][["ui_msg"]]

      uiele})
    # Creates the ui for the compact view of the module
    #------------------------------------
    # Compact ui
    output$CTS_ui_compact  =  renderUI({
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      uiele_code_button = NULL
      # Generating code button if enabled
      if( state[["MC"]][["compact"]][["code"]]){
        uiele_code = tagList(shinyAce::aceEditor(
          NS(id, "ui_cts_code"),
          height  = state[["MC"]][["formatting"]][["code"]][["height"]]
          ))

        uiele_code_button = tagList(
         shinyWidgets::dropdownButton(
           uiele_code,
           inline  = FALSE,
           right   = TRUE ,
           size    = "sm",
           circle  = FALSE,
           width   = state[["MC"]][["formatting"]][["code"]][["width"]],
           status  = "danger btn-custom-cts",
           icon    = icon("code", lib="font-awesome"),
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
        )

      }

      # Button with CTS elements table
      #uiele_cts_elements_button = NULL
     # Uncomment this if your cohort has a components table
      uiele_cts_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_current_rules"))
      uiele_cts_elements_button = tagList(
       shinyWidgets::dropdownButton(
         uiele_cts_elements,
         inline  = FALSE,
         right   = TRUE ,
         size    = "sm",
         circle  = FALSE,
         status  = "primary btn-custom-cts",
         icon    = icon("layer-group", lib="font-awesome"),
         tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["elements"]]))
      )

      uiele_upper = tagList(
        div(style="display:inline-block",
            htmlOutput(NS(id, "CTS_ui_select_element"))),
        div(style="display:inline-block",
            htmlOutput(NS(id, "CTS_ui_text_element_name"))),
        div(style="display:inline-block",
            htmlOutput(NS(id, "CTS_ui_source_model"))),
        tags$br(),
        div(style="display:inline-block", verbatimTextOutput(NS(id, "ui_cts_msg")))
      )

      # We only show the clip button if it's enabled
      uiele_clip_button = NULL
      if(state[["MC"]][["compact"]][["clip"]]){
        uiele_clip_button = htmlOutput(NS(id, "ui_cts_clip_code"))
      }

      uiele_buttons_right = tagList(
               tags$style(".btn-custom-cts {width: 100px;}"),
               div(style="display:inline-block;vertical-align:top;height:100px",
               uiele_cts_elements_button,
               uiele_code_button,
               uiele_clip_button,
               htmlOutput(NS(id, "ui_cts_save_btn")),
               htmlOutput(NS(id, "ui_cts_copy_btn")),
               htmlOutput(NS(id, "ui_cts_del_btn")),
               htmlOutput(NS(id, "ui_cts_new_btn"))
               ))

      # Appending the preview
    # div_style = paste0("display:inline-block;vertical-align:top;",
    #   "width:",   state[["MC"]][["formatting"]][["preview"]][["width"]],  ";",
    #   "height: ", state[["MC"]][["formatting"]][["preview"]][["height"]])

      div_style = paste0("display:inline-block;vertical-align:top;",
        "width:",   state[["MC"]][["formatting"]][["preview"]][["width"]])

      uiele_preview = div(style=div_style,
                htmlOutput(NS(id, "CTS_ui_simres")))


      td_style ="display:inline-block;vertical-align:top;text-align:top"
      # These are the different cohort building elements
      # Covariates
      uiele_chrt_ele_covs = 
        shiny::tabPanel(id=NS(id, "tab_res_chrt_ele_covs"),
                 title=tagList(shiny::icon("users-between-lines"),
                               state[["MC"]][["labels"]][["tab_chrt_ele_covs"]]),
          htmlOutput(NS(id, "CTS_ui_covariates_none")),
          tags$br(),
          tags$table(
            tags$tr(
              tags$td(style=td_style,
                htmlOutput(NS(id, "CTS_ui_covariates_selection")),
                htmlOutput(NS(id, "CTS_ui_covariates_type")),
                htmlOutput(NS(id, "CTS_ui_covariates_value")),
                htmlOutput(NS(id, "CTS_ui_covariates_button"))
              ), 
              tags$td(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')), 
              tags$td(style=td_style,
                htmlOutput(NS(id, "CTS_ui_covariates_table"))
              )
            )
          )
        )
      # Rules
      uiele_chrt_ele_rules = 
        shiny::tabPanel(id=NS(id, "tab_res_chrt_ele_rules"),
                 title=tagList(shiny::icon("syringe"),
                               state[["MC"]][["labels"]][["tab_chrt_ele_rules"]]),
          tags$table(
            tags$tr(
              tags$td(style=td_style,
                htmlOutput(NS(id, "CTS_ui_select_rule_type")),
                htmlOutput(NS(id, "CTS_ui_rule_name")),
                htmlOutput(NS(id, "CTS_ui_rule_condition")),
                htmlOutput(NS(id, "CTS_ui_add_rule_btn"))
              ), 
              tags$td(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')), 
              tags$td(style=td_style,
                htmlOutput(NS(id, "CTS_ui_action_dosing_state")),
                htmlOutput(NS(id, "CTS_ui_action_dosing_values")),
                htmlOutput(NS(id, "CTS_ui_action_dosing_times")),
                htmlOutput(NS(id, "CTS_ui_action_dosing_durations")),
                htmlOutput(NS(id, "CTS_ui_action_set_state_state")),
                htmlOutput(NS(id, "CTS_ui_action_set_state_value")),
                htmlOutput(NS(id, "CTS_ui_action_manual_code"))
              )
            )
          )
        )
      # Trial 
      uiele_chrt_ele_trial = 
        shiny::tabPanel(id=NS(id, "tab_res_chrt_ele_trial"),
                 title=tagList(shiny::icon("chart-gantt"),
                               state[["MC"]][["labels"]][["tab_chrt_ele_trial"]]),
                div(style="display:inline-block",
                htmlOutput(NS(id, "CTS_ui_nsub"))),
                div(style="display:inline-block",
                htmlOutput(NS(id, "CTS_ui_visit_times"))),
                div(style="display:inline-block",
                htmlOutput(NS(id, "CTS_ui_trial_end")))
        )
      # Simulation Options
      uiele_chrt_ele_sim = 
        shiny::tabPanel(id=NS(id, "tab_res_chrt_ele_sim"),
                 title=tagList(shiny::icon("gear"),
                               state[["MC"]][["labels"]][["tab_chrt_ele_sim"]]),
                 htmlOutput(NS(id, "CTS_ui_sim_cfg"))
        )

      uiele_chrt_ele = 
        shinydashboard::tabBox(
          width = 10,
          title = NULL,
          uiele_chrt_ele_trial, 
          uiele_chrt_ele_rules,
          uiele_chrt_ele_covs,
          uiele_chrt_ele_sim 
      )

      uiele = tagList(
        uiele_upper,
        tags$br(), 
        uiele_preview,
        uiele_buttons_right,
        tags$br(), 
        uiele_chrt_ele
      )

      uiele
    })
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
             #react_state[[id_MB]],
             input$button_clk_new,
             input$button_clk_del,
             input$button_clk_copy,
             input$button_clk_save)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = CTS_fetch_state(id        = id,
                               id_ASM          = id_ASM,
                               id_MB           = id_MB,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        react_state[[id]][["CTS"]][["checksum"]] = state[["CTS"]][["checksum"]]
      }, priority=99)
    }
    #------------------------------------
    # This can be used to trigger notifications
    # You need to add reactive inputs here when those
    # inputs can trigger a notification.
    toNotify <- reactive({
      list(
       input$button_clk_add_rule,
       input$button_clk_add_cov,
       input$button_clk_save,
       input$button_clk_copy,
       input$button_clk_del,
       input$button_clk_new
      )
    })
    observeEvent(toNotify(), {
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      # Triggering optional notifications
      notify_res = formods::FM_notify(
        state   = state,
        session = session)
    })
    #------------------------------------
    # Removing holds
    remove_hold_listen  <- reactive({
        list(
             react_state[[id_ASM]],
           # input$button_clk_new,
           # input$button_clk_del,
           # input$button_clk_copy,
           # input$button_clk_save,
             input$element_selection
           # input$current_element
           )
      })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = CTS_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             id_MB           = id_MB,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["CTS"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)

  })
}

#'@export
#'@title Fetch Clinical Trial Simulator State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param id_MB An ID string that corresponds with the ID used to call the MB modules
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The list
#'has the following structure:
#' \itemize{
#' \item{yaml:} Full contents of the supplied yaml file.
#' \item{MC:} Module components of the yaml file.
#' \item{CTS:}
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the state.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"CTS"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = CTS_test_mksession(session=list(), full_session=FALSE)
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "ruminate", "templates", "CTS.yaml")
#'
#' # Creating an empty state object
#' state = CTS_fetch_state(id              = "CTS",
#'                        id_ASM          = "ASM",
#'                        id_MB           = "MB",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
CTS_fetch_state = function(id, id_ASM, id_MB, input, session, FM_yaml_file, MOD_yaml_file, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it

  if(is.null(state)){
    # General state information
    state = CTS_init_state(FM_yaml_file, MOD_yaml_file, id, id_MB, session)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["CTS"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["CTS"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       if(ui_name %in% names(state[["CTS"]][["button_counters"]])){
         state[["CTS"]][["ui"]][[ui_name]] = 0
       } else {
         state[["CTS"]][["ui"]][[ui_name]] = ""
       }

       # initializing the previous ui values as well:
       if(is.null(state[["CTS"]][["ui_old"]][[ui_name]])){
         state[["CTS"]][["ui_old"]][[ui_name]] = state[["CTS"]][["ui"]][[ui_name]]
       }
     }
   }
   msgs = c()

  #---------------------------------------------
  # Now we sync the ui in the state with the button click
  # tracking or current element. This ensures that every
  # time the state is fetched all of the components of
  # the current element are in sync.

  # This is a list of ui changes that were detected and
  # can be used to trigger different actions below:
  changed_uis = c()

  # We need to pull out the current element for updating:
  current_ele = CTS_fetch_current_element(state)
  # There are scenarios where you wouldn't want to do this. Like when
  # switching elements in the ui. You would need to add some logic to
  # only update below conditionally.

  for(ui_name in state[["CTS"]][["ui_ids"]]){
    if(!fetch_hold(state, ui_name)){
      if(ui_name %in% names(state[["CTS"]][["button_counters"]])){
        # Button changes are compared to the button click tracking values
        change_detected =
          has_changed(ui_val  = state[["CTS"]][["ui"]][[ui_name]],
                      old_val = state[["CTS"]][["button_counters"]][[ui_name]])

        if(change_detected){
          formods::FM_le(state, paste0("button click: ", ui_name, " = ", state[["CTS"]][["ui"]][[ui_name]]))

          # Saving the change:
          state[["CTS"]][["button_counters"]][[ui_name]] =
            state[["CTS"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)
        }
      }else{
        change_detected =
          has_changed(ui_val  = state[["CTS"]][["ui"]][[ui_name]],
                      old_val = state[["CTS"]][["ui_old"]][[ui_name]])

        if(change_detected){
          formods::FM_le(state, paste0("setting cohort: ", ui_name, " = ", paste(state[["CTS"]][["ui"]][[ui_name]], collapse=", ")))

          # Saving the change:
          state[["CTS"]][["ui_old"]][[ui_name]] = state[["CTS"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)

          # This also updates the current element if that ui_name is part of
          # an element
          if(ui_name %in% state[["CTS"]][["ui_ele"]]){
            current_ele[["ui"]][[ui_name]] = state[["CTS"]][["ui"]][[ui_name]]
          }
        }
      }
    }
  }
  # Updating the element with any changes:
  state = CTS_set_current_element(
    state   = state,
    element = current_ele)
  #---------------------------------------------
  # Here we react to changes between the UI and the current state
  # save cohort
  if("button_clk_save" %in% changed_uis){
    FM_le(state, "save cohort")
    current_ele = CTS_fetch_current_element(state)

    # Saving the name
    current_ele[["ui"]][["element_name"]] =
      state[["CTS"]][["ui"]][["element_name"]]

    # Saving the changes to the source model
    current_ele = CTS_change_source_model(state, current_ele)

    # Saving the element:
    state = CTS_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # save cohort
  if("button_clk_runsim" %in% changed_uis){
    FM_le(state, "run simulation")
  }
  #---------------------------------------------
  # clip cohort
  if("button_clk_clip" %in% changed_uis){
    FM_le(state, "clip cohort")
  }
  #---------------------------------------------
  # copy cohort
  if("button_clk_copy" %in% changed_uis){
    FM_le(state, "copy cohort")

    # First we pull out the current element:
    old_ele = CTS_fetch_current_element(state)

    # Now we create a new element and make it the current element
    state   = CTS_new_element(state)
    new_ele = CTS_fetch_current_element(state)

    # This is a list of UI elements to skip when copying:
    ui_copy_skip = c("element_name")

    # Here we copy all the ui elements from old to new skipping those flagged
    # for skipping.
    for(tmp_ui_name in names(new_ele[["ui"]])){
      if(!(tmp_ui_name %in% ui_copy_skip)){
        new_ele[["ui"]][[tmp_ui_name]]  = old_ele[["ui"]][[tmp_ui_name]]
      }
    }

    new_ele[["components_table"]] = old_ele[["components_table"]]
    new_ele[["components_list"]]  = old_ele[["components_list"]]
    new_ele[["covariates"]]       = old_ele[["covariates"]]
    new_ele[["rx_details"]]       = old_ele[["rx_details"]]

    state = CTS_set_current_element(
      state   = state,
      element = new_ele)
  }
  #---------------------------------------------
  # del cohort
  if("button_clk_del" %in% changed_uis){
    FM_le(state, "delete cohort")
    state = CTS_del_current_element(state)
  }
  #---------------------------------------------
  # new cohort
  if("button_clk_new" %in% changed_uis){
    FM_le(state, "new cohort")
    state = CTS_new_element(state)
  }
  #---------------------------------------------
  # save cohort
  if("button_clk_update_plot" %in% changed_uis){
    FM_le(state, "update plot")
    # Pull out the current element
    current_ele = CTS_fetch_current_element(state)

    # Next we plot the element
    current_ele = CTS_plot_element(state, current_ele)

    # Putting the element back in the state
    state = CTS_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # rule table clicked
  if("hot_current_rules" %in% changed_uis){
    FM_le(state, "current rules changed")
    hdf = rhandsontable::hot_to_r(state[["CTS"]][["ui"]][["hot_current_rules"]])
    if("Delete" %in% names(hdf)){
      if(any(hdf[["Delete"]])){
        #Pulling out the current element
        current_ele = CTS_fetch_current_element(state)

        # Getting the rule id(s) to delete:
        del_rule_ids  = unfactor(hdf[which(hdf[["Delete"]]), ][["rule_id"]])
        for(del_rule_id in del_rule_ids){
          # This pulls out the corresponding hash
          del_rule_hash =
            current_ele[["components_table"]][
            current_ele[["components_table"]][["rule_id"]] == del_rule_id,
            ][["hash"]]


          if(length(del_rule_hash) > 0){
            # Rmeoving the rule from the table
            current_ele[["components_table"]] =
              current_ele[["components_table"]][
                current_ele[["components_table"]][["hash"]] != del_rule_hash,
              ]
            # Removing the rule from the hash
            current_ele[["components_list"]][[del_rule_hash]] = NULL
          }else{
            # This shouldn't happen so we need to throw a message if it does.
            FM_le(state, paste0("Unable to delete rule id: ", del_rule_id), entry_type="danger")
          }
        }

        # Putting the element back in the state
        state = CTS_set_current_element(
          state   = state,
          element = current_ele)
      }
    }
  }
  #---------------------------------------------
  # add covariate
  if("button_clk_add_cov" %in% changed_uis){
    FM_le(state, "adding covariate")

    #Pulling out the current element
    current_ele = CTS_fetch_current_element(state)

    # Adding the covariate
    current_ele = CTS_add_covariate(state, current_ele)

    # Appending any messages that were generated.
    msgs = c(msgs, current_ele[["cares"]][["msgs"]])

    selected_covariate = state[["CTS"]][["ui"]][["selected_covariate"]]

    if(current_ele[["cares"]][["COV_IS_GOOD"]]){
      state = formods::FM_set_notification(state,
        notify_text = paste0("Covariate ", selected_covariate, " Added"),
        notify_id   = "COV_IS_GOOD",
        type        = "success")
      # Putting the element back in the state if it's good
      state = CTS_set_current_element( state   = state, element = current_ele)
    } else {
      state = formods::FM_set_notification(state,
        notify_text = paste0("Unable to Add Covariate: ", selected_covariate),
        notify_id   = "COV_IS_BAD",
        type        = "failure")
    }
  }
  #---------------------------------------------
  # add rule
  if("button_clk_add_rule" %in% changed_uis){


    #Pulling out the current element
    current_ele = CTS_fetch_current_element(state)

    # Adding the rule
    current_ele = CTS_add_rule(state, current_ele)

    # Putting the element back in the state
    state = CTS_set_current_element(
      state   = state,
      element = current_ele)

    # Appending any messages that were generated.
    msgs = c(msgs, current_ele[["rares"]][["msgs"]])

    # Setting the appropriate notification
    state = formods::FM_set_notification(state,
      notify_text = current_ele[["rares"]][["notify_text"]],
      notify_id   = current_ele[["rares"]][["notify_id"]],
      type        = current_ele[["rares"]][["notify_type"]])


  }
  #---------------------------------------------
  # selected cohort changed
  if("element_selection" %in% changed_uis){
    state[["CTS"]][["current_element"]] =
       state[["CTS"]][["ui"]][["element_selection"]]

    # Setting the hold for all the other UI elements
    state = set_hold(state)
  }
  #---------------------------------------------
  # clip cohort
  if("covariate_type" %in% changed_uis){
    FM_le(state, "covariate type changed")
    state = set_hold(state)
  }

  # Appending any messages in the current element (these are errors and
  # whatnot that are generated on save) to the general messages returned
  # to the user
  current_ele = CTS_fetch_current_element(state)
  msgs = c(msgs, current_ele[["msgs"]])
  #---------------------------------------------
  # Passing any messages back to the user
  # NOTE: this only occurs when ui changes have been detected you may need to
  # add additional logic for a given module
  if(!is.null(changed_uis)){
    state = FM_set_ui_msg(state, msgs)
  }


  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@export
#'@title Initialize CTS Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section
#'@param MOD_yaml_file  Module configuration file with MC as main section
#'@param id ID string for the module
#'@param id_MB An ID string that corresponds with the ID used to call the MB modules
#'@param session Shiny session variable
#'@return list containing an empty CTS state
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = CTS_test_mksession(session=list(), full_session=FALSE)
#' session = sess_res$session
#' input   = sess_res$input
#'
#' state = CTS_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "ruminate",
#'                                "templates",
#'                                "CTS.yaml"),
#'    id              = "CTS",
#'    id_MB           = "MB",
#'    session         = session)
#'
#' state
CTS_init_state = function(FM_yaml_file, MOD_yaml_file,  id, id_MB, session){

  sc_meta = CTS_fetch_sc_meta(MOD_yaml_file)

  button_counters = c("button_clk_runsim",
                      "button_clk_update_plot",
                      "button_clk_save",
                      "button_clk_clip",
                      "button_clk_del",
                      "button_clk_copy",
                      "button_clk_new",
                      "button_clk_add_cov",
                      "button_clk_add_rule")

  # These are the module ui elements that are associated with
  # the current element
  ui_ele          = c("element_name",
                      "nsub",
                      "fpage",
                      "dvcols",
                      "tc_dim",
                      "evplot",
                      "visit_times",
                      "trial_end",
                      "rule_condition",
                      "action_dosing_state",
                      "action_dosing_values",
                      "action_dosing_times",
                      "action_dosing_durations",
                      "action_set_state_state",
                      "action_set_state_value",
                      "action_manual_code",
                      sc_meta[["ui_config"]])

  # This contains all of the relevant ui_ids in the module. You need to append
  # ui_ids that are outside of the current element here as well.
  ui_ids          = c(button_counters,
                      ui_ele,
                      "rule_type",
                      "rule_name",
                      "source_model",
                      "hot_current_rules",
                      "covariate_type_selected",
                      "covariate_value",
                      "selected_covariate",
                      "switch_output",
                      "element_selection")

  # Making all the ui_ids holdable
  ui_hold         = ui_ids

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    # Add module dependencies here
    dep_mod_ids     = c(id_MB),
    MT              = "CTS",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  # Getting the currently defined models
  # JMH update this when reacting to id_MB
  MDL = FM_fetch_mdl(state, session, ids = id_MB)

  # Storing the ui_ids for the elements
  state[["CTS"]][["MDL"]]                  = MDL

  # Storing the ui_ids for the elements
  state[["CTS"]][["ui_ele"]]               = ui_ele

  # This tracks elements for the module
  state[["CTS"]][["code_previous"]]        = NULL
  state[["CTS"]][["elements"]]             = NULL
  state[["CTS"]][["current_element"]]      = NULL
  state[["CTS"]][["element_cntr"]]         = 0

  # Metadata about the simulation configuration
  state[["CTS"]][["sc_meta"]]              = sc_meta

  # Creating a default element:
  state = CTS_new_element(state)

  # initializing the module checksum:
  state = CTS_update_checksum(state)

  FM_le(state, "State initialized")
state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state CTS state from \code{CTS_fetch_state()}
#'@return Character object vector with the lines of code
#'@example inst/test_apps/CTS_funcs.R
CTS_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Add Rule to Element
#'@description Takes the ui elements in the state and element and attempts to add a rule.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param element Element list from \code{CTS_fetch_current_element()}
#'@return Element with the results of adding the rule. The \code{rares} list
#'element can be used to determine the exit status of the function.
#'\itemize{
#'  \item{RULE_IS_GOOD}  If true it indicates that the pieces of the rule from
#'  the UI check out.
#'  \item{RULE_UPDATED}  If RULE_IS_GOOD and RULE_UPDATED is true then a
#'  previous rule definition was overwritten. If RULE_IS_GOOD is TRUE and
#'  RULE_UPDATED is FALSE then a new rule was added.
#' \item{notify_text}  Text for notify message
#' \item{notify_id}    Notification ID
#' \item{notify_type}  Notification type
#' \item{msgs}         Vector of messages.
#'}
#'@details This depends on the following UI values in the state and element
#'\itemize{
#'  \item{}  \code{state[["CTS"]][["ui"]][["rule_name"]]}
#'  \item{}  \code{state[["CTS"]][["ui"]][["rule_condition"]]}
#'  \item{}  \code{state[["CTS"]][["ui"]][["rule_type"]]}
#'    \itemize{
#'      \item{}  For rule type "dose"
#'      \itemize{
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_dosing_state"]]}
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_dosing_values"]]}
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_dosing_times"]]}
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_dosing_durations"]]}
#'      }
#'    }
#'    \itemize{
#'      \item{}  For rule type "set state"
#'      \itemize{
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_set_state_state"]]}
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_set_state_values"]]}
#'      }
#'    }
#'    \itemize{
#'      \item{}  For rule type "manual code"
#'      \itemize{
#'         \item{}  \code{state[["CTS"]][["ui"]][["action_manual_code"]]}
#'      }
#'    }
#'}
#'@example inst/test_apps/CTS_funcs.R
CTS_add_rule  = function(state, element){

  RULE_IS_GOOD = TRUE
  RULE_UPDATED = FALSE
  rule_desc    = ""
  rule_name    = state[["CTS"]][["ui"]][["rule_name"]]
  rule_action  = list()
  msgs         = c()

  # Converting the name into a "good" name
  if(!is.null(rule_name)){
    if(rule_name == ""){
      RULE_IS_GOOD = FALSE
      msgs = c(msgs, paste0("Rule name undefined."))
    } else {
      if(!grepl("^[[:alpha:]]", rule_name, perl=TRUE)){
        RULE_IS_GOOD = FALSE
        msgs = c(msgs, paste0("Rule name must start with a letter."))

      }
      if(!grepl("^[A-Za-z\\d_]+$", rule_name, perl=TRUE)){
        RULE_IS_GOOD = FALSE
        msgs = c(msgs, paste0("Rule name can only contain letters, numbers and underscores (_)."))
      }
    }
  } else {
    RULE_IS_GOOD = FALSE
    msgs = c(msgs, paste0("Rule name undefined."))
  }

  # Making sure the rule type is defined
  if(!is.null(state[["CTS"]][["ui"]][["rule_type"]])){
    ui_req = c("rule_condition")
    if(state[["CTS"]][["ui"]][["rule_type"]] == "dose"){
      rule_desc    = paste0("dose into ", state[["CTS"]][["ui"]][["action_dosing_state"]])
      rule_action  = list(
         condition        = state[["CTS"]][["ui"]][["rule_condition"]],
         action = list(
           type             = "dose",
           state            = state[["CTS"]][["ui"]][["action_dosing_state"]],
           values           = state[["CTS"]][["ui"]][["action_dosing_values"]],
           times            = state[["CTS"]][["ui"]][["action_dosing_times"]],
           durations        = state[["CTS"]][["ui"]][["action_dosing_durations"]]))

      ui_req = c(ui_req,
        "action_dosing_state",
        "action_dosing_values",
        "action_dosing_times",
        "action_dosing_durations")

    } else if(state[["CTS"]][["ui"]][["rule_type"]] == "set state"){
      rule_desc = paste0("set state ", state[["CTS"]][["ui"]][["action_set_state_state"]])
      rule_action  = list(
         condition       = state[["CTS"]][["ui"]][["rule_condition"]],
         action = list(
           type   = "set state",
           state  = state[["CTS"]][["ui"]][["action_set_state_state"]],
           value  = state[["CTS"]][["ui"]][["action_set_state_values"]]))
      ui_req = c(ui_req,
        "action_set_state_state",
        "action_set_state_value")
    } else if(state[["CTS"]][["ui"]][["rule_type"]] == "manual"){
      rule_desc = paste0("manual code")
      rule_action  = list(
         condition       = state[["CTS"]][["ui"]][["rule_condition"]],
         action = list(
           type     = "manual",
           code     = state[["CTS"]][["ui"]][["action_manual_code"]]))
      ui_req = c(ui_req,
        "action_manual_code")
    } else {
      RULE_IS_GOOD = FALSE
      msgs = c(msgs, paste0("Unknown rule_type: ", state[["CTS"]][["ui"]][["rule_type"]]))
    }


    # We have to check the rule components. This is a simple check to make
    # sure something is there. Because the components can have arbitrary
    # user defined code, we cannot really validate it here.
    if(RULE_IS_GOOD){
      for(tmp_ui in ui_req){
        if(!is.null( state[["CTS"]][["ui"]][[tmp_ui]])){
          if(state[["CTS"]][["ui"]][[tmp_ui]] == ""){
            RULE_IS_GOOD = FALSE
            msgs = c(msgs, paste0(tmp_ui, " is undefined."))
          }
        } else {
          RULE_IS_GOOD = FALSE
          msgs = c(msgs, paste0(tmp_ui, " is undefined."))
        }
      }
    }
  } else {
    RULE_IS_GOOD = FALSE
    msgs = c(msgs, "Unable to determine the rule_type")
  }

  if(RULE_IS_GOOD){
    FM_le(state, "add rule success")

    # Checking to see if the rule name has already been used. If so we just
    # replace it and flag a message. So first we delete it from the
    # components table and the rule list.
    if(!is.null(element[["components_table"]][["Name"]])){
      if(any(element[["components_table"]][["Name"]] == rule_name)){

        # Removing the list element:
        hash_del =
          element[["components_table"]][
            (element[["components_table"]][["Name"]] == rule_name), ][["hash"]]
        element[["components_list"]][[hash_del]] = NULL

        # Removing the row:
        element[["components_table"]] =
          element[["components_table"]][
              !(element[["components_table"]][["Name"]] == rule_name), ]

        msgs = c(msgs, paste0("Rule: ", rule_name, " has been updated."))
        RULE_UPDATED = TRUE
      }
    }

    # By default there are no rules:
    rule_id = 1

    # Now we look at the table tracking rules to see if it exists and if it
    # has at lest one rule
    if(!is.null(element[["components_table"]])){
      if(nrow(element[["components_table"]]) > 0){
        rule_id = max(element[["components_table"]][["rule_id"]]) + 1
      }
    }

    rule_hash = digest::digest(rule_id, algo=c("md5"))
    rule_row = data.frame(
      rule_id   = rule_id,
      Name      = rule_name,
      Condition = state[["CTS"]][["ui"]][["rule_condition"]],
      Action    = rule_desc,
      Delete    = FALSE,
      hash      = rule_hash
    )

    # Adding the rule row
    element[["components_table"]] = rbind(
      element[["components_table"]],
      rule_row
    )

    # Adding rule details to list
    element[["components_list"]][[rule_hash]][[rule_name]] = rule_action

  }


  if(RULE_IS_GOOD){
    if(RULE_UPDATED){
      FM_le(state, "rule updated")
      notify_text = paste0("Rule ", rule_name, " updated.")
      notify_id   = "RULE_UPDATED"
      notify_type = "warning"
    } else {
      notify_text = paste0("Rule ", rule_name, " added.")
      notify_id   = "RULE_IS_GOOD"
      notify_type = "success"
      FM_le(state, "rule added")
    }
  } else {
    notify_text = "Unable to add rule."
    notify_id   = "RULE_IS_BAD"
    notify_type = "failure"
  }

  # Appending results
  element[["rares"]] = list(
    RULE_IS_GOOD = RULE_IS_GOOD,
    RULE_UPDATED = RULE_UPDATED,
    notify_text  = notify_text,
    notify_id    = notify_id,
    notify_type  = notify_type,
    msgs         = msgs
  )


element}

#'@export
#'@title Add Covariate to Elemetnt
#'@description Takes the ui elements in the module state and processes the covariate elements for addition.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param element Element list from \code{CTS_fetch_current_element()}
#'@return Element with the results of adding the covariate. The \code{cares} list
#'element can be used to determine the exit status of the function.
#'\itemize{
#'  \item{COV_IS_GOOD}  If TRUE if the covariate was good and added, and FALSE if
#'  there were any issues.
#' \item{msgs}     Vector of messages.
#'}
#'@details This depends on the following UI values in the state
#'\itemize{
#'  \item{} \code{state[["CTS"]][["ui"]][["covariate_value"]]}
#'  \item{} \code{state[["CTS"]][["ui"]][["covariate_type_selected"]]}
#'  \item{} \code{state[["CTS"]][["ui"]][["selected_covariate"]]}
#'}
#'@example inst/test_apps/CTS_funcs.R
CTS_add_covariate   = function(state, element){

  msgs         = c()
  COV_IS_GOOD = TRUE

  covariate_value    = state[["CTS"]][["ui"]][["covariate_value"]]
  covariate_type     = state[["CTS"]][["ui"]][["covariate_type_selected"]]
  selected_covariate = state[["CTS"]][["ui"]][["selected_covariate"]]
  if(covariate_value == ""){
    tmp_msg = paste0("No value specified for covariate: ", selected_covariate, ".")
    FM_le(state, tmp_msg)
    msgs = c(msgs, tmp_msg)
    COV_IS_GOOD = FALSE
  }else{
    covariate_value = paste0("c(", covariate_value, ")")
    cmd = paste0("cvval = ", covariate_value)
    tcres =
      FM_tc(cmd     = cmd,
            tc_env  = list(),
            capture = "cvval")

    if(tcres[["isgood"]]){
      # Adding the covariate:
      cov_list = list(
          values   = tcres[["capture"]][["cvval"]],
          sampling = state[["MC"]][["covariate_generation"]][["types"]][[covariate_type]][["sampling"]],
          type     = state[["MC"]][["covariate_generation"]][["types"]][[covariate_type]][["type"]])

      element[["covariates"]][[selected_covariate]] = cov_list
    } else {
      tmp_msg = paste0("Unable to evaluate value for covariate: ", selected_covariate, ".")
      FM_le(state, tmp_msg)
      msgs = c(msgs, tmp_msg)
      msgs = c(msgs, paste0("  -> ", covariate_value))
      msgs = c(msgs, tcres[["msgs"]])
      COV_IS_GOOD = FALSE
    }
  }

  # Appending results
  element[["cares"]] = list(
    COV_IS_GOOD  = COV_IS_GOOD,
    msgs         = msgs
  )
element}

#'@export
#'@title Append Report Elements
#'@description Appends report elements to a formods report.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for
#' \code{\link[onbrand]{template_details}}
#'@param rpttype Type of report to generate (supported "xlsx", "pptx", "docx").
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasrptele:} Boolean indicator if the module has any reportable elements.
#'  \item{code:}      Code to generate reporting elements.
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{rpt:}       Report with any additions passed back to the user.
#'}
#'@seealso \code{\link[formods]{FM_generate_report}}
CTS_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The CTS module only supports the following report types:
  supported_rpttypes = c("xlsx", "pptx", "docx")

  if(rpttype %in% supported_rpttypes){
  }

  res = list(
    isgood    = isgood,
    hasrptele = hasrptele,
    code      = code,
    msgs      = msgs,
    rpt       = rpt
  )

res}

#'@export
#'@title Fetch Clinical Trial Simulator Module Datasets
#'@description Fetches the datasets produced by the module. For each cohort
#'this will be the simulation timecourse and the event table
#'@param state CTS state from \code{CTS_fetch_state()}
#'@return Character object vector with the lines of code
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasds:}     Boolean indicator if the module has any datasets
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{ds:}        List with datasets. Each list element has the name of
#'  the R-object for that dataset. Each element has the following structure:
#'  \itemize{
#'    \item{label: Text label for the dataset}
#'    \item{MOD_TYPE: Short name for the type of module.}
#'    \item{id: module ID}
#'    \item{DS: Dataframe containing the actual dataset.}
#'    \item{DSMETA: Metadata describing DS}
#'    \item{code: Complete code to build dataset.}
#'    \item{checksum: Module checksum.}
#'    \item{DSchecksum: Dataset checksum.}
#'  }
#'}
#'@examples
#' # We need a module state:
#' sess_res = CTS_test_mksession(session=list(), full_session=FALSE)
#' state = sess_res$state
#'
#' ds = CTS_fetch_ds(state)
#'
#' ds
CTS_fetch_ds = function(state){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

# # This prevents returning a dataset if this is triggered before data has
# # been loaded
  if(state[["CTS"]][["isgood"]]){
 
    # Fill in the DS creation stuff here
    isgood = FALSE

    # Empty list for new datasets
    NEWDS = list(label      = NULL,
                 MOD_TYPE   = "CTS",
                 id         = NULL,
                 DS         = NULL,
                 DSMETA     = NULL,
                 code       = NULL,
                 checksum   = NULL,
                 DSchecksum = NULL)
    
    # Putting it all into the ds object to be returned
    for(ename in names(state[["CTS"]][["elements"]])){
      current_ele = state[["CTS"]][["elements"]][[ename]] 
      if(current_ele[["isgood"]]){
        if(current_ele[["simres"]][["isgood"]]){
          hasds = TRUE
          sim_tc_obj = current_ele[["sim_tc_object_name"]]
          sim_ev_obj = current_ele[["sim_ev_object_name"]]

          # Timecourse dataset
          ds[[sim_tc_obj]] = NEWDS
          ds[[sim_tc_obj]][["label"]]       = paste0(current_ele[[ename]][["element_name"]])
          ds[[sim_tc_obj]][["DS"]]          =  current_ele[["simres"]][["capture"]][[sim_tc_obj]]
          ds[[sim_tc_obj]][["DSMETA"]]      =  state[["MC"]][["labels"]][["ds_tc"]]
          ds[[sim_tc_obj]][["code"]]        =  current_ele[["code_ele_only"]]
          ds[[sim_tc_obj]][["checksum"]]    =  state[["CTS"]][["checksum"]]
          ds[[sim_tc_obj]][["DSchecksum"]]  =  digest::digest(
            current_ele[["simres"]][["capture"]][[sim_tc_obj]],
            algo=c("md5"))

          # Event table dataset
          ds[[sim_ev_obj]] = NEWDS
          ds[[sim_ev_obj]][["label"]]       = paste0(current_ele[[ename]][["element_name"]])
          ds[[sim_ev_obj]][["DS"]]          =  current_ele[["simres"]][["capture"]][[sim_ev_obj]]
          ds[[sim_ev_obj]][["DSMETA"]]      =  state[["MC"]][["labels"]][["ds_ev"]]
          ds[[sim_ev_obj]][["code"]]        =  current_ele[["code_ele_only"]]
          ds[[sim_ev_obj]][["checksum"]]    =  state[["CTS"]][["checksum"]]
          ds[[sim_ev_obj]][["DSchecksum"]]  =  digest::digest(
            current_ele[["simres"]][["capture"]][[sim_ev_obj]],
            algo=c("md5"))
        }
      }
    }
  } else {
    # Fill in the DS creation stuff here
    isgood = FALSE
  }

  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}

#  #'@export
#  #'@title Fetch Clinical Trial Simulator Module Models
#  #'@description Fetches the models contained in the module.
#  #'@param state CTS state from \code{CTS_fetch_state()}
#  #'@return list containing the following elements
#  #'\itemize{
#  #'  \item{isgood:}    Return status of the function.
#  #'  \item{hasmdl:}    Boolean indicator if the module has any models
#  #'  \item{msgs:}      Messages to be passed back to the user.
#  #'  \item{mdl:}       List with models. Each list element has the name of
#  #'  the R-object for that dataset. Each element has the following structure:
#  #'  \itemize{
#  #'    \item{label:}      Text label for the model (e.g. one-compartment model).
#  #'    \item{MOD_TYPE:}   Type of module.
#  #'    \item{id:}         Module ID.
#  #'    \item{rx_obj:}     The rxode2 object name that holds the model.
#  #'    \item{fcn_def:}    Text to define the model
#  #'    \item{MDLMETA:}    Notes about the model.
#  #'    \item{code:}       Code to generate the model.
#  #'    \item{checksum:}   Module checksum.
#  #'    \item{MDLchecksum:} Model checksum.
#  #'  }
#  #'}
#  #'@examples
#  #' # We need a module state:
#  #' sess_res = CTS_test_mksession(session=list(), full_session=FALSE)
#  #' state = sess_res$state
#  #'
#  #' mdls = CTS_fetch_mdl(state)
#  #'
#  #' names(mdls)
#  CTS_fetch_mdl = function(state){
#
#    # JMH update later
#    hasmdl  = FALSE
#    isgood = TRUE
#    msgs   = c()
#    mdl    = list()
#
#    # This prevents returning a dataset if this is triggered before data has
#    # been loaded
#    if(state[["CTS"]][["isgood"]]){
#
#      # Checksum for the module
#      m_checksum = state[["CTS"]][["checksum"]]
#      elements = names(state[["CTS"]][["elements"]])
#      if(!is.null(elements)){
#        # We have at least 1 model
#        hasmdl = TRUE
#        for(element in elements){
#          # current element
#          ce = state[["CTS"]][["elements"]][[element]]
#          ce_checksum = ce[["checksum"]]
#
#
#          # NOTE: You need to populate teh NULL pieces below:
#          mdl[[ ce[["rx_obj_name"]] ]] =
#            list(label       = ce[["ui"]][["element_name"]],
#                 MOD_TYPE    = "CTS",
#                 id          = state[["id"]],
#                 rx_obj      = NULL, #
#                 fcn_def     = NULL, #
#                 MDLMETA     = NULL, #
#                 code        = NULL, #
#                 checksum    = m_checksum,
#                 MDLchecksum = ce_checksum)
#        }
#      }
#
#    } else {
#      isgood = FALSE
#      msgs = c(msgs, "Bad CTS state")
#    }
#
#    res = list(hasmdl  = hasmdl,
#               isgood = isgood,
#               msgs   = msgs,
#               mdl    = mdl)
#    res}

#'@export
#'@title Updates CTS Module Checksum
#'@description Takes a CTS state and updates the checksum used to trigger
#'downstream updates
#'@param state CTS state from \code{CTS_fetch_state()}
#'@return CTS state object with the checksum updated
#'@example inst/test_apps/CTS_funcs.R
CTS_update_checksum     = function(state){

  # checksum string
  chk_str = ""

  # We'll concatinate all the individual checksums together
  # and create a checksum of those:
  element_ids = names(state[["CTS"]][["elements"]])
  for(element_id in element_ids){
    # We trigger updates when the element changes:
    chk_str = paste0(chk_str, ":", state[["CTS"]][["elements"]][[element_id]][["checksum"]])
    #JMH add element_name here?
  }

  # This prevents messaging when no change has been made to the module.
  old_chk = state[["CTS"]][["checksum"]]
  new_chk = digest::digest(chk_str, algo=c("md5"))

  if(has_updated(old_chk, new_chk)){
    state[["CTS"]][["checksum"]] = new_chk
    FM_le(state, paste0("module checksum updated: ", state[["CTS"]][["checksum"]]))
  }

state}


#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM An ID string that corresponds with the ID used to call the ASM module
#'@param id_MB An ID string that corresponds with the ID used to call the MB module
#'@param full_session  Boolean to indicate if the full test session should be created (default \code{TRUE}).
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:} Boolean indicating the exit status of the function.
#'   \item{session:} The value Shiny session variable (in app) or a list (outside of app) after initialization.
#'   \item{input:} The value of the shiny input at the end of the session initialization.
#'   \item{state:} App state.
#'   \item{rsc:} The \code{react_state} components.
#'}
#'@examples
#'sess_res = CTS_test_mksession(session=list(), full_session=FALSE)
CTS_test_mksession = function(session, id = "CTS", id_ASM="ASM", id_MB = "MB", full_session=TRUE){

  isgood = TRUE
  rsc    = list()
  input  = list()
  state  = list()

  sess_res = MB_test_mksession(session=session, full_session=full_session)
  session = sess_res[["session"]]

  # Configuration files
  FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "ruminate", "templates", "CTS.yaml")

  # Creating an empty state object
  state = CTS_fetch_state(id             = id,
                         id_ASM          = id_ASM,
                         id_MB           = id_MB,
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         react_state     = NULL)


  # Pulling the default first element
  current_ele = CTS_fetch_current_element(state)

  # Defining the source model
  state[["CTS"]][["ui"]][["source_model"]] = "MB_obj_1_rx"
  current_ele = CTS_change_source_model(state, current_ele)

  # Single visit
  current_ele[["ui"]][["visit_times"]]                 = "0"
  current_ele[["ui"]][["cts_config_nsteps"]]           = "5"

  # Creating a dosing rule
  state[["CTS"]][["ui"]][["rule_condition"]]           = "time == 0"
  state[["CTS"]][["ui"]][["rule_type"]]                = "dose"
  state[["CTS"]][["ui"]][["action_dosing_state"]]      = "central"
  state[["CTS"]][["ui"]][["action_dosing_values"]]     = "c(1)"
  state[["CTS"]][["ui"]][["action_dosing_times"]]      = "c(0)"
  state[["CTS"]][["ui"]][["action_dosing_durations"]]  = "c(0)"
  state[["CTS"]][["ui"]][["rule_name"]]                = "Single_Dose"


  # Adding the rule:
  current_ele = CTS_add_rule(state, current_ele)

  # Appending the plotting details as well
  current_ele[["ui"]][["fpage"]]             = "1"
  current_ele[["ui"]][["dvcols"]]            = "Cc"

  # Reducing the number of subjects and steps to speed things up on CRAN
  current_ele[["ui"]][["nsub"]]              = "2"
  current_ele[["ui"]][["cts_config_nsteps"]] = "5"

  # Putting the element back in the state forcing code generation
  state = CTS_set_current_element(
    state   = state,
    element = current_ele)

  # Now we pull out the current element, and simulate it
  current_ele = CTS_fetch_current_element(state)
  current_ele = CTS_simulate_element(state, current_ele)

  # Next we plot the element
  current_ele = CTS_plot_element(state, current_ele)

  # Now we save those results back into the state:
  state = CTS_set_current_element(
    state   = state,
    element = current_ele)

  #-------------------------------------------------------
  if(full_session){
    # Creating a new element, pulling it out, setting the source model,
    # and defining the visit times:
    state       = CTS_new_element(state)
    current_ele = CTS_fetch_current_element(state)
    state[["CTS"]][["ui"]][["source_model"]] = "MB_obj_2_rx"
    current_ele = CTS_change_source_model(state, current_ele)
    current_ele[["ui"]][["visit_times"]]  = "(0:6)*28*2"

    # We must define covariates:
    state[["CTS"]][["ui"]][["covariate_value"]]            = "70, .1"
    state[["CTS"]][["ui"]][["covariate_type_selected"]]    = "cont_lognormal"
    state[["CTS"]][["ui"]][["selected_covariate"]]         = "WT"
    current_ele = CTS_add_covariate(state, current_ele)

    state[["CTS"]][["ui"]][["covariate_value"]]            = "0, 1"
    state[["CTS"]][["ui"]][["covariate_type_selected"]]    = "discrete"
    state[["CTS"]][["ui"]][["selected_covariate"]]         = "SEX_ID"
    current_ele = CTS_add_covariate(state, current_ele)

    state[["CTS"]][["ui"]][["covariate_value"]]            = "1"
    state[["CTS"]][["ui"]][["covariate_type_selected"]]    = "fixed"
    state[["CTS"]][["ui"]][["selected_covariate"]]         = "SUBTYPE_ID"
    current_ele = CTS_add_covariate(state, current_ele)

    # Next we define the rules
    # Initial dose:
    state[["CTS"]][["ui"]][["rule_condition"]]           = "time == 0"
    state[["CTS"]][["ui"]][["rule_type"]]                = "dose"
    state[["CTS"]][["ui"]][["action_dosing_state"]]      = "Ac"
    state[["CTS"]][["ui"]][["action_dosing_values"]]     = "c(0.1,  0.1,  0.1,  0.1)*1e6/MW"
    state[["CTS"]][["ui"]][["action_dosing_times"]]      = "c(0, 14, 28, 42)"
    state[["CTS"]][["ui"]][["action_dosing_durations"]]  = "c(0,  0,  0,  0)"
    state[["CTS"]][["ui"]][["rule_name"]]                = "first_dose"
    current_ele = CTS_add_rule(state, current_ele)

    # Maintain steady state if within rage:
    state[["CTS"]][["ui"]][["rule_condition"]]           = "((BM <=  7e4) & (BM >=5e4)) & (time > 0)"
    state[["CTS"]][["ui"]][["rule_type"]]                = "dose"
    state[["CTS"]][["ui"]][["action_dosing_state"]]      = "Ac"
    state[["CTS"]][["ui"]][["action_dosing_values"]]     = "c( 1.0,  1.00,  1.00,  1.00)*SI_fpd(id=id, state='Ac')"
    state[["CTS"]][["ui"]][["action_dosing_times"]]      = "c(0, 14, 28, 42)"
    state[["CTS"]][["ui"]][["action_dosing_durations"]]  = "c(0,  0,  0,  0)"
    state[["CTS"]][["ui"]][["rule_name"]]                = "keep_dose"
    current_ele = CTS_add_rule(state, current_ele)

    # Decrease dose if over target
    state[["CTS"]][["ui"]][["rule_condition"]]           = "(BM >  7e4) & (time > 0)"
    state[["CTS"]][["ui"]][["rule_type"]]                = "dose"
    state[["CTS"]][["ui"]][["action_dosing_state"]]      = "Ac"
    state[["CTS"]][["ui"]][["action_dosing_values"]]     = "c( .90,   .90,   .90,   .90)*SI_fpd(id=id, state='Ac')"
    state[["CTS"]][["ui"]][["action_dosing_times"]]      = "c(0, 14, 28, 42)"
    state[["CTS"]][["ui"]][["action_dosing_durations"]]  = "c(0,  0,  0,  0)"
    state[["CTS"]][["ui"]][["rule_name"]]                = "decrease_dose"
    current_ele = CTS_add_rule(state, current_ele)

    # Increase dose if below target
    state[["CTS"]][["ui"]][["rule_condition"]]           = "(BM <  5e4) & (time > 0)"
    state[["CTS"]][["ui"]][["rule_type"]]                = "dose"
    state[["CTS"]][["ui"]][["action_dosing_state"]]      = "Ac"
    state[["CTS"]][["ui"]][["action_dosing_values"]]     = "c(1.30,  1.30,  1.30,  1.30)*SI_fpd(id=id, state='Ac')"
    state[["CTS"]][["ui"]][["action_dosing_times"]]      = "c(0, 14, 28, 42)"
    state[["CTS"]][["ui"]][["action_dosing_durations"]]  = "c(0,  0,  0,  0)"
    state[["CTS"]][["ui"]][["rule_name"]]                = "increase_dose"
    current_ele = CTS_add_rule(state, current_ele)

    # Appending the plotting details as well
    current_ele[["ui"]][["fpage"]]                = "1"
    current_ele[["ui"]][["trial_end"]]            = "400"
    current_ele[["ui"]][["dvcols"]]               = c("Cc", "BM")

    # Number of sujbects
    current_ele[["ui"]][["nsub"]]                 = "12"
    current_ele[["ui"]][["cts_config_nsteps"]]    = "50"

    # Putting the element back in the state forcing code generation
    state = CTS_set_current_element( state   = state, element = current_ele)

    # Now we pull out the current element and simulate it:
    current_ele = CTS_fetch_current_element(state)
    current_ele = CTS_simulate_element(state, current_ele)

    # Next we plot the element
    current_ele = CTS_plot_element(state, current_ele)

    # then we save everything:
    state = CTS_set_current_element( state   = state, element = current_ele)
  }

  if(("ShinySession" %in% class(session))){
    FM_set_mod_state(session, id, state)
  } else {
    session = FM_set_mod_state(session, id, state)
  }

  res = list(
    isgood  = isgood,
    session = session,
    input   = input,
    state   = state,
    rsc     = rsc
  )
}

#'@export
#'@title New ===Module_Name=== cohort
#'@description Appends a new empty cohort to the CTS state object
#'and makes this new cohort the active cohort.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@return CTS state object containing a new cohort and that
#'cohort is set as the current active cohort. See the help for
#'\code{CTS_fetch_state()} for ===ELEMENT== format.
#'@example inst/test_apps/CTS_funcs.R
CTS_new_element = function(state){

  # Incrementing the element counter
  state[["CTS"]][["element_cntr"]] = state[["CTS"]][["element_cntr"]] + 1


  # Figuring out the default source model:
  source_model = ""
  if( !is.null(state[["CTS"]][["MDL"]][["hasmdl"]]) ){
  if( state[["CTS"]][["MDL"]][["hasmdl"]] ){
    # This just uses the first one
    source_model = state[["CTS"]][["MDL"]][["catalog"]][["object"]][1]
  }}

  rx_details = NULL
  if(source_model != ""){
    rx_details = fetch_rxinfo(state[["CTS"]][["MDL"]][["mdl"]][[source_model]][["rx_obj"]])
  }

  # Creating a default element ID
  element_id = paste0("element_", state[["CTS"]][["element_cntr"]])

  # Creating the object name for this element
  element_object_name = paste0(state[["MC"]][["element_object_name"]],
                       "_", state[["CTS"]][["element_cntr"]])

  def_evplot = state[["MC"]][["formatting"]][["evplot"]][["choices"]][[
                state[["MC"]][["formatting"]][["evplot"]][["default"]]
                ]][["value"]]

  # Default for a new element:
  element_def =
    list(
         # internal use only
         isgood                 = TRUE,
         # This will hold the ui values for the current element
         ui                     = list(
           element_name  = paste0("cohort ", state[["CTS"]][["element_cntr"]]),
           nsub          =  state[["MC"]][["formatting"]][["nsub"]][["value"]],
           visit_times   =  state[["MC"]][["formatting"]][["visit_times"]][["value"]],
           trial_end     =  state[["MC"]][["formatting"]][["trial_end"]][["value"]],
           fpage         = "1",
           dvcols        = "",
           tc_dim        = state[["MC"]][["formatting"]][["tc_dim"]][["default"]],
           evplot        = def_evplot,
           source_model  = source_model
           ),
         id                     = element_id,
         idx                    = state[["CTS"]][["element_cntr"]],
         element_object_name    = element_object_name,
         cov_object_name        = paste0(element_object_name, "_cov"),
         rules_object_name      = paste0(element_object_name, "_rules"),
         subs_object_name       = paste0(element_object_name, "_subs"),
         rxopts_object_name     = paste0(element_object_name, "_rxopts"),
         simres_object_name     = paste0(element_object_name, "_simres"),
         sim_tc_object_name     = paste0(element_object_name, "_sim_tc"),
         sim_ev_object_name     = paste0(element_object_name, "_sim_ev"),
         ot_object_name         = paste0(element_object_name, "_output_times"),
         fgtc_object_name       = paste0(element_object_name, "_fgtc"),
         fgev_object_name       = paste0(element_object_name, "_fgev"),
         code_previous          = NULL,
         # This is information about the source model from fetch_rxinfo()
         rx_details             = rx_details,
         # user facing
         # This is used if you build the element in a layering method sort of
         # like how the ggplot figures in the FG module builds using different
         # ggplot commands (layers).
         components_table       = NULL,
         components_list        = list(),
         # This will hold definitions for how covariates are to be determined
         # when subjects are created
         covariates             = list(),
         # Generated on save
         checksum               = NULL,
         # code is the code to generate the element by itself. It assumes
         # any other module code, library calls, etc will be present before
         # this is run. It is used to generate the reproducible script on
         # export.
         code                   = NULL,
         # code_ele_only is meant to stand alone and be run to regenerate the
         # element by itself. It should contain any library calls and module
         # components that the current module and element depend on. It is
         # what you see in the code pull down for the current element in the
         # UI
         code_ele_only          = NULL)


  # Creating default values for the simulation configuration options
  for(cname in names(state[["CTS"]][["sc_meta"]][["config"]])){
    element_def[["ui"]][[  state[["CTS"]][["sc_meta"]][["config"]][[cname]][["ui"]]  ]] =
      state[["CTS"]][["sc_meta"]][["config"]][[cname]][["value"]]
  }

  # This contains the code to generate inputs for the current element (e.g.
  # datasets that are needed).
  code_previous = ""
  element_def[["code_previous"]] = code_previous

  # Dropping the new element into the state
  state[["CTS"]][["elements"]][[element_id]] = element_def


  # updating the checksum for the current element
  state[["CTS"]][["elements"]][[element_id]][["checksum"]] = digest::digest(element_def, algo=c("md5"))

  # Setting the new element as current
  state[["CTS"]][["current_element"]]     = element_id

state}


#'@export
#'@title Fetches Current cohort
#'@description Takes a CTS state and returns the current active
#'cohort
#'@param state CTS state from \code{CTS_fetch_state()}
#'@return List containing the details of the active data view. The structure
#'of this list is the same as the structure of \code{state$CTS$elements} in the output of
#'\code{CTS_fetch_state()}.
#'@example inst/test_apps/CTS_funcs.R
CTS_fetch_current_element    = function(state){

  element_id = state[["CTS"]][["current_element"]]

  current_element = state[["CTS"]][["elements"]][[element_id]]

current_element}

#'@export
#'@title Simulates the Specified Element
#'@description Takes a CTS state and element and simulates the current set of
#'rules.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param element Element list from \code{CTS_fetch_current_element()}
#'@return Simulation element with simulation results stored in the
#'\code{"simres"} element.
#'@example inst/test_apps/CTS_funcs.R
CTS_simulate_element    = function(state, element){

  ELE_ISGOOD = TRUE
  capture    = NULL
  msgs       = c()

  source_model = element[["ui"]][["source_model"]]

  # Here we can do some high-level checks before we run the simulation:

  # This makes sure all the covariates have been defined:
  if(!all(element[["rx_details"]][["elements"]][["covariates"]] %in% names(element[["covariates"]]))){
    missing_covars = element[["rx_details"]][["elements"]][["covariates"]][
          !(element[["rx_details"]][["elements"]][["covariates"]] %in% names(element[["covariates"]])) ]

    ELE_ISGOOD = FALSE
    msgs = c("The following covariates have not been defined:",
             paste0("  > ", paste0(missing_covars, collapse = ", ")))

  }


  if(ELE_ISGOOD){
    if(source_model %in% names(state[["CTS"]][["MDL"]][["mdl"]])){
      code_model        = state[["CTS"]][["MDL"]][["mdl"]][[source_model]][["code"]]
      rx_obj_name       = state[["CTS"]][["MDL"]][["mdl"]][[source_model]][["rx_obj_name"]]
      rx_obj            = state[["CTS"]][["MDL"]][["mdl"]][[source_model]][["rx_obj"]]

      # These are all the objects to collect after the simulation
      capture  = c(
        element[["cov_object_name"]],
        element[["subs_object_name"]],
        element[["sim_tc_object_name"]],
        element[["sim_ev_object_name"]],
        element[["simres_object_name"]])


      # THis is the current simulation object in use
      tc_env  = list()
      tc_env[[rx_obj_name]] = rx_obj

      # This will run the simulation only:
      cmd = element[["code_sim_only"]]

      tcres =
        FM_tc(cmd     = cmd,
              tc_env  = tc_env,
              capture = capture)

      capture    = tcres[["capture"]]

      # IF the try catch succeeded we need to insepct the simulation result otherwise
      # we pass the failure back from FM_tc
      if(tcres[["isgood"]]){
        ELE_ISGOOD = tcres[["capture"]][[ element[["simres_object_name"]] ]][["isgood"]]
        msgs = c(msgs, tcres[["capture"]][[ element[["simres_object_name"]] ]][["msgs"]])
      } else {
        ELE_ISGOOD = tcres[["isgood"]]
        msgs       = c(msgs, tcres[["msgs"]])
      }


    } else {
      ELE_ISGOOD = FALSE
      msgs = c(msgs, "Source model not found")
    }
  }

  if(!ELE_ISGOOD){
    FM_le(state, "CTS_simulate_element()")
    FM_le(state, unlist(strsplit(element[["code_sim_only"]], split="\n")))
    FM_le(state, msgs)
  }
  element[["simres"]] = list(
    capture = capture,
    isgood  = ELE_ISGOOD,
    msgs    = msgs)

element}

#'@export
#'@title Plots the Specified Element
#'@description Takes a CTS state and element and simulates the current set of
#'rules.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param element Element list from \code{CTS_fetch_current_element()}
#'@return Simulation element with plot results stored in the '\code{"plotres"} element.
# with the following structure:
#' \itemize{
#'   \item{isgood}   Boolean value indicating the state of the figure generation code.
#'   \item{msgs}     Any messages to be passed to the user.
#'   \item{capture}  Captured figure generation output from \code{plot_sr_tc()}
#'}
#'@example inst/test_apps/CTS_funcs.R
CTS_plot_element    = function(state, element){

  ELE_ISGOOD = TRUE
  capture    = NULL
  msgs       = c()

  if(is.null(element[["simres"]][["isgood"]])){
    ELE_ISGOOD = FALSE
    msgs = c(msgs, state[["MC"]][["errors"]][["no_sim_found"]])
  } else if(!element[["simres"]][["isgood"]]){
    ELE_ISGOOD = FALSE
    msgs = c(msgs, state[["MC"]][["errors"]][["bad_sim"]])
  }



  if(ELE_ISGOOD){
    tcres =
    FM_tc(cmd     = element[["code_figtcev"]],
          tc_env  = element[["simres"]][["capture"]],
          capture = c(element$fgtc_object_name,
                      element$fgev_object_name))

    if(tcres[["isgood"]]){
      capture = tcres[["capture"]]
    } else {
      ELE_ISGOOD = FALSE
      msgs = c(msgs, tcres[["msgs"]])
    }
  }

  if(!ELE_ISGOOD){
    FM_le(state, "CTS_plot_element()")
    FM_le(state, unlist(strsplit(element[["code_figtcev"]], split="\n")))
    FM_le(state, msgs)
  }
  element[["plotres"]] = list(
    capture = capture,
    isgood  = ELE_ISGOOD,
    msgs    = msgs)

element}

#'@export
#'@title Sets the Value for the  Current cohort
#'@description Takes a CTS state and returns the current active
#'cohort
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param element Element list from \code{CTS_fetch_current_element()}
#'@return CTS state object with the current cohort set using the
#'supplied value.
#'@example inst/test_apps/CTS_funcs.R
CTS_set_current_element    = function(state, element){

  element_id = state[["CTS"]][["current_element"]]

  ELE_ISGOOD          = TRUE
  msgs                = c()

  # These are the objects names used in the code
  model_object        = "not_found"
  cov_object_name     = element[["cov_object_name"]]
  subs_object_name    = element[["subs_object_name"]]
  rxopts_object_name  = element[["rxopts_object_name"]]
  simres_object_name  = element[["simres_object_name"]]
  sim_tc_object_name  = element[["sim_tc_object_name"]]
  sim_ev_object_name  = element[["sim_ev_object_name"]]
  rules_object_name   = element[["rules_object_name"]]
  ot_object_name      = element[["ot_object_name"]]
  fgtc_object_name    = element[["fgtc_object_name"]]
  fgev_object_name    = element[["fgev_object_name"]]

  # These are the little code chunks that will be stacked to create the final
  # pieces of code for the element
  code_packages       =  paste0("library(", state[["MC"]][["code"]][["packages"]],")")
  code_model          = c()
  code_cov            = c("",
                          "# Defining covariates",
                          paste0(cov_object_name, " = list()"))
  code_mksubs         = c()
  code_rules          = c("",
                          "# Creating rules",
                          paste0(rules_object_name, " = list()"))
  code_rxopts         = c()
  code_ot             = c()
  code_simrules       = c()


  # updating the code elements
  source_model = element[["ui"]][["source_model"]]
  if(source_model %in% names(state[["CTS"]][["MDL"]][["mdl"]])){
    code_model   = state[["CTS"]][["MDL"]][["mdl"]][[source_model]][["code"]]
    model_object = state[["CTS"]][["MDL"]][["mdl"]][[source_model]][["rx_obj_name"]]
  } else {
    ELE_ISGOOD = FALSE
    msgs = c(msgs, "Source model not found")
  }

  # Code to define covariates
  if(length(element[["rx_details"]][["elements"]][["covariates"]]) > 0){
    for(cname in element[["rx_details"]][["elements"]][["covariates"]]){
      if(cname %in% names(element[["covariates"]])){
        code_cov = c(code_cov, paste0(cov_object_name,'[["', cname, '"]] = ', deparse(element[["covariates"]][[cname]])))
      } else {
        ELE_ISGOOD = FALSE
        msgs = c(msgs, paste0("Covariate not defined:",cname))
      }
    }
  }

  # Code to make subjects
  code_mksubs = c(
    paste0(""),
    paste0("# Generating the subjects"),
    paste0(subs_object_name, " = mk_subjects(object = ", model_object,  ","),
    paste0("  nsub   = ", element[["ui"]][["nsub"]],               ","),
    paste0("  covs   = ", cov_object_name,                         ")"))

  # Code to define rules
  if(!is.null(names(element[["components_list"]]))){
    for(tmp_hash in names(element[["components_list"]]) ){
      tmp_rule_name = names(element[["components_list"]][[tmp_hash]])[1]
      tmp_rule_value = deparse(element[["components_list"]][[tmp_hash]][[tmp_rule_name]])
      code_rules = c(code_rules,
        "",
        paste0(rules_object_name, '[["',tmp_rule_name ,'"]] = ') ,
        tmp_rule_value
        )
    }
  }
  code_rules = c(code_rules, "")

  # Code to define rx_options

  if("rxSolve" %in% state[["CTS"]][["sc_meta"]][["cfg_summary"]][["use"]]){
    code_rxopts = c("# rxSolve options",
                    paste0(rxopts_object_name, ' = list('))
    opt_names = state[["CTS"]][["sc_meta"]][["cfg_summary"]][
            "rxSolve" == state[["CTS"]][["sc_meta"]][["cfg_summary"]][["use"]], ][["name"]]

    optidx = 1
    for(opt_name in opt_names){
      opt_type  = state[["CTS"]][["sc_meta"]][["config"]][[opt_name]][["type"]]
      opt_ui    = state[["CTS"]][["sc_meta"]][["config"]][[opt_name]][["ui"]]
      opt_value = element[["ui"]][[opt_ui]]

      # This will contain the parameter definition for the list element
      opt_str = "  "

      if(opt_type == "numeric" |
         opt_type == "logical"   ){
        opt_str = paste0(opt_str, opt_name, " = ", opt_value )
      } else if ( opt_type == "character"   ){
        opt_str = paste0(opt_str, opt_name, ' = "', opt_value, '"')
      } else {
        FM_le(state, paste0("Unknown option type: ", opt_type, ", for option: ", opt_name), entry_type="danger")
      }

      if(optidx < length(opt_names)){
        opt_str = paste0(opt_str, ",")
      } else {
        opt_str = paste0(opt_str, ")")
      }

      code_rxopts = c(code_rxopts, opt_str)

      optidx = optidx + 1
    }
    code_rxopts = c(code_rxopts, "")

  } else {
    code_rxopts = paste0(rxopts_object_name, ' = list()'  )
  }

  # JMH todo
  # Code to define output_times
  # We check the trial_end input
  tmp_trial_end = autocast(element[["ui"]][["trial_end"]])
  TEND_ISGOOD = TRUE
  if(is.numeric(tmp_trial_end)){
    if(tmp_trial_end <=0){
      TEND_ISGOOD = FALSE
      ELE_ISGOOD = FALSE
      msgs = c(msgs, "Trial end must be > 0.")
    }
  } else {
    ELE_ISGOOD = FALSE
    TEND_ISGOOD = FALSE
    msgs = c(msgs, "Trial end is not numeric.")
  }


  # We check the nsteps input
  tmp_nsteps    = autocast(element[["ui"]][["cts_config_nsteps"]])
  NSTEPS_ISGOOD = TRUE
  if(is.numeric(tmp_nsteps)){
    if(tmp_nsteps <=1){
      STEPS_ISGOOD = FALSE
      msgs = c(msgs, "Number of steps must be > 1.")
    }
    if( (tmp_nsteps %% 1) != 0){
      STEPS_ISGOOD = FALSE
      msgs = c(msgs, "Number of steps must be an integer.")
    }
  } else {
    NSTEPS_ISGOOD = FALSE
    msgs = c(msgs, "Number of steps is not numeric.")
  }


  # This actually creates the output times code
  if(NSTEPS_ISGOOD & TEND_ISGOOD){
    code_ot   = "# Output times"
    code_ot   = c(code_ot,
                  paste0( ot_object_name, " = formods::linspace(0,", tmp_trial_end, ",", tmp_nsteps, ')'),
                  "")
  } else {
    ELE_ISGOOD = FALSE
    code_ot = c("# Unable to create output times",
                 paste0( ot_object_name, " = c()"),
                 "")
  }


  # Code to define eval_times
  if(is.null(element[["ui"]][["visit_times"]])){
    ELE_ISGOOD = FALSE
    msgs = c(msgs, "Visit times not specified")
    visit_str = "NULL"
  } else {
    visit_str = paste0("c(", element[["ui"]][["visit_times"]], ")")

  }

  # Code to run the simulation
  code_simrules =
  c(
    paste0('# Running simulation'),
    paste0(simres_object_name, ' =                                 '  ),
    paste0(' simulate_rules(object        = ', model_object,       ','),
    paste0('                subjects      = ', subs_object_name,   '[["subjects"]],'),
    paste0('                eval_times    = ', visit_str,          ','),
    paste0('                output_times  = ', ot_object_name,     ','),
    paste0('                rules         = ', rules_object_name,  ','),
    paste0('                rx_options    = ', rxopts_object_name, ')'),
    "# Collecting the simulation and event history values",
    paste0(sim_tc_object_name, ' = ', simres_object_name, '[["simall"]]'),
    paste0(sim_ev_object_name, ' = ', simres_object_name, '[["ev_history"]]'),
    ""
   )


  # By default we use the dvcols from the element:
  dvcols    = element[["ui"]][["dvcols"]]
  evplot    = as.numeric(as.character(element[["ui"]][["evplot"]]))

  fnrow=state[["MC"]][["formatting"]][["tc_dim"]][["choices"]][[   element[["ui"]][["tc_dim"]]  ]][["nrow"]]
  fncol=state[["MC"]][["formatting"]][["tc_dim"]][["choices"]][[   element[["ui"]][["tc_dim"]]  ]][["ncol"]]

  # Code to make the timecourse and event figures
  code_figtcev =
  c(
    paste0('# Plotting timecourse'),
    paste0(fgtc_object_name, ' =                                 '),
    paste0('  plot_sr_tc(sro = ', simres_object_name, ','         ),
    paste0('    fncol        = ', fncol,',                       '),
    paste0('    fnrow        = ', fnrow,',                       '),
    paste0('    dvcols       = ', deparse(dvcols),','          ),
    paste0('    fpage        = ', element[["ui"]][["fpage"]],  ")"),
    "",
    paste0('# Plotting events'),
    paste0(fgev_object_name, ' =                                 '),
    paste0('  plot_sr_ev(sro = ', simres_object_name, ','         ),
    paste0('    fncol        = ', fncol,',                       '),
    paste0('    fnrow        = ', fnrow,',                       '),
    paste0('    evplot       = ', deparse(evplot),','             ),
    paste0('    fpage        = ', element[["ui"]][["fpage"]],  ")"),
    ""
   )

  element[["code_figtcev"]] = paste0(code_figtcev , collapse = "\n")

  # Stand alone code to make the element
  element[["code"]]          = paste0(c(code_packages,
                                        "",
                                        "# Creating the model",
                                        code_model,
                                        code_cov,
                                        code_mksubs,
                                        code_rules,
                                        code_rxopts,
                                        code_ot,
                                        code_simrules,
                                        code_figtcev
                                        ), collapse="\n")


  # code to do everything upto the simulation
  element[["code_sim_only"]] = paste0(c(code_cov,
                                        code_mksubs,
                                        code_rules,
                                        code_rxopts,
                                        code_ot,
                                        code_simrules),
                                        collapse="\n")


  # Code to make the element only assuming all the goodies it needs are
  # already defined
  element[["code_ele_only"]] = paste0(c(code_cov,
                                        code_mksubs,
                                        code_rules,
                                        code_rxopts,
                                        code_ot,
                                        code_simrules
                                        ),
                                        collapse="\n")

  # Saving the element status
  element[["isgood"]]        = ELE_ISGOOD
  element[["msgs"]]          = msgs

  # updating the checksum for the current element
  tmp_ele = element

  # These are the components that are not necessary to signal a change
  tmp_ele[["checksum"]]                        = ""
 #tmp_ele[["ui"]][["rule_condition"]]          = ""
 #tmp_ele[["ui"]][["action_dosing_state"]]     = ""
 #tmp_ele[["ui"]][["action_dosing_values"]]    = ""
 #tmp_ele[["ui"]][["action_dosing_times"]]     = ""
 #tmp_ele[["ui"]][["action_dosing_durations"]] = ""
 #tmp_ele[["ui"]][["action_set_state_state"]]  = ""
 #tmp_ele[["ui"]][["action_set_state_value"]]  = ""
 #tmp_ele[["ui"]][["action_manual_code"]]      = ""

  tmp_checksum  = digest::digest(tmp_ele, algo=c("md5"))
  if(has_updated(element[["checksum"]], tmp_checksum)){
    FM_le(state, paste0("cohort checksum updated: ", tmp_checksum))
    element[["checksum"]]  = tmp_checksum
  }

  # this updates the current element
  state[["CTS"]][["elements"]][[element_id]] = element

  # This will update the checksum for the module
  state = CTS_update_checksum(state)

state}

#'@export
#'@title Deletes Current cohort
#'@description Takes a CTS state and deletes the current cohort.
#'If that is the last element, then a new default will be added.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@return CTS state object with the current cohort deleted.
#'@example inst/test_apps/CTS_funcs.R
CTS_del_current_element    = function(state){

  # We need the current element and corresponding ID
  current_element = CTS_fetch_current_element(state)
  element_id = current_element[["id"]]

  # This deletes the current element ID
  state[["CTS"]][["elements"]][[element_id]] = NULL

  if(length(names(state[["CTS"]][["elements"]])) == 0){
    # This is triggered when we've deleted the last element,
    # So now we will create a new one that will be active:
    state = CTS_new_element(state)
  } else {
    # If there is at least one left, we pull off the first
    # one and make that active:
    element_id = names(state[["CTS"]][["elements"]])[1]
    state[["CTS"]][["current_element"]] = element_id
  }

state}





#'@export
#'@title Fetches Simulation Parameter Meta Information
#'@description This provides meta information about simulatino options. This
#'includes option names, text descriptions, ui_names used, etc.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@return List with the following elements:
#' \itemize{
#'   \item{config} List from the YAML->MC->sim_config.
#'   \item{summary:} Dataframe with elements of config in tabular format.
#'   \item{ui_config} Vector of  all the ui_ids for configuration options.
#'}
#'@examples
#' CTS_fetch_sc_meta()
CTS_fetch_sc_meta = function(
  MOD_yaml_file = system.file(package="ruminate","templates","CTS.yaml")){

  ui_config = c()

  cfg_summary = NULL

  # Reading in the yaml file
  MOD_config = yaml::read_yaml(MOD_yaml_file)
  sim_config = MOD_config[["MC"]][["sim_config"]]
  for(cname in names(sim_config)){
    tmp_ui    = paste0("cts_config_", cname)
    cfg_summary=
      rbind(cfg_summary,
        data.frame(
          name   = cname,
          ui     = tmp_ui,
          use    = sim_config[[cname]][["use"]],
          type   = sim_config[[cname]][["type"]],
          group  = sim_config[[cname]][["group"]]
        )
      )
    ui_config = c(ui_config, tmp_ui)
    sim_config[[cname]][["ui"]] = tmp_ui
  }

  res = list(
      config      = sim_config,
      cfg_summary = cfg_summary,
      ui_config   = ui_config
    )
res}


#'@export
#'@title Change the Source Model
#'@description Takes the ui elements in the state and element and processes any changes to the source model and updates the element accordingly.
#'@param state CTS state from \code{CTS_fetch_state()}
#'@param element Element list from \code{CTS_fetch_current_element()}
#'@return Element with the necessary changes to the source model.
#'@details This depends on the following UI values in the state.
#'\itemize{
#'  \item{}  \code{state[["CTS"]][["ui"]][["source_model"]]}
#'}
#'@example inst/test_apps/CTS_funcs.R
CTS_change_source_model   = function(state, element){

    tmp_source_model = state[["CTS"]][["ui"]][["source_model"]]

    # If the source model has actually changed we zero out the covariates as
    # well.
    if(state[["CTS"]][["ui"]][["source_model"]] != element[["ui"]][["source_model"]]){
      FM_le(state, "source model change detected")
      FM_le(state, " > covariates reset")
      element[["covariates"]] = list()
    }

    # Saving the source model
    element[["ui"]][["source_model"]] = tmp_source_model

    # updating the rx_details
    element[["rx_details"]] = fetch_rxinfo(state[["CTS"]][["MDL"]][["mdl"]][[tmp_source_model]][["rx_obj"]])

element}
