#' ruminate: Shiny app and module to facilitate pharamacometrics analysis
#'
#' This is done by creating a Shiny interface to different tools for data
#' transformation (`dplyr` and `tidyr`), plotting (`ggplot2`), and
#' noncompartmental analysis (`PKNCA`). These results can be reported in Excel,
#' Word or PowerPoint. The state of the app can be saved and loaded at a later
#' date. When saved, a script is generated to reproduce the different actions in
#' the Shiny interface.
#'
#' @seealso \url{https://ruminate.ubiquity.tools/}
#' @docType package
#' @name ruminate
"_PACKAGE"

#'@import rhandsontable
#'@import dplyr
#'@import tidyr
#'@import tidyr
#'@import formods
#'@import shiny
#'@import officer
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor
#'@importFrom stringr str_replace str_detect str_split
#'@importFrom utils head

# JMH
# Notes:
# - Add reporting elements
# - Create NCA_test_mksession()
# - Add NCA tabular results
# - Add NCA graphic results

#'@export
#'@title Fetch Non-Compartmental Analysis State
#'@description Merges default app options with the changes made in the UI
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param id_UD ID string for the upload data module used to save and load app states
#'@param id_DW ID string for the data wrangling module used to save and load app states
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The list
#'has the following structure:
#' \itemize{
#' \item{yaml:} Full contents of the supplied yaml file.
#' \item{MC:} Module components of the yaml file.
#' \item{NCA:}
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the state.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"NCA"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
#'@example inst/test_apps/nca_app.R
NCA_Server <- function(id,
                      FM_yaml_file  = system.file(package = "formods",   "templates", "formods.yaml"),
                      MOD_yaml_file = system.file(package = "ruminate",  "templates", "NCA.yaml"),
                      id_ASM        = "ASM",
                      id_UD         = "UD",
                      id_DW         = "DW",
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Generated data reading code
    observe({
      # Reacting to file changes
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["button_fg_ind_obs_save"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      if(is.null(current_ana[["code"]])){
        uiele = "# Run analysis to see code."
      } else {
        uiele = current_ana[["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_nca_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # User messages:
    output$ui_nca_msg = renderText({
      input[["button_ana_add_int"]]
      input[["button_ana_use_scenario"]]
      input[["button_fg_ind_obs_save"]]

      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_fig"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = state[["NCA"]][["ui_msg"]]

      uiele})
    #------------------------------------
    # Side buttons:
    # new
    output$ui_nca_new_ana = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_new"),
                label   = state[["MC"]][["labels"]][["new_ana"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_new"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_new"]][["block"]],
                color   = "success",
                icon    = icon("plus"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip             = state[["MC"]][["formatting"]][["button_ana_new"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_ana_new"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # save
    output$ui_nca_save_ana = renderUI({

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_save"),
                label   = state[["MC"]][["labels"]][["save_ana"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_save"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_save"]][["block"]],
                color   = "primary",
                icon    = icon("arrow-down"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip             = state[["MC"]][["formatting"]][["button_ana_save"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_ana_save"]][["tooltip_position"]])

      uiele})
    #------------------------------------
    # clip code
    output$ui_nca_clip_code = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      uiele = NULL
      uiele = NULL
      if((system.file(package="clipr") != "") &
         !state[["yaml"]][["FM"]][["deployed"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_ana_clip"),
                  label   = state[["MC"]][["labels"]][["clip_ana"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_ana_clip"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_ana_clip"]][["block"]],
                  color   = "royal",
                  icon    = icon("clipboard", lib="font-awesome"))
        # Optinally adding the tooltip:
        uiele = formods::FM_add_ui_tooltip(state, uiele,
                 tooltip             = state[["MC"]][["formatting"]][["button_ana_clip"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["button_ana_clip"]][["tooltip_position"]])
      }
      uiele})
    #------------------------------------
    # delete
    output$ui_nca_del_ana   = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_del"),
                label   = state[["MC"]][["labels"]][["del_ana"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_del"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_del"]][["block"]],
                color   = "danger",
                icon    = icon("minus"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip             = state[["MC"]][["formatting"]][["button_ana_del"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_ana_del"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    # copy
    output$ui_nca_copy_ana   = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_copy"),
                label   = state[["MC"]][["labels"]][["copy_ana"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_copy"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_copy"]][["block"]],
                color   = "royal",
                icon    = icon("copy"))

      # Optinally adding the tooltip:
      uiele = formods::FM_add_ui_tooltip(state, uiele,
               tooltip             = state[["MC"]][["formatting"]][["button_ana_copy"]][["tooltip"]],
               position    = state[["MC"]][["formatting"]][["button_ana_copy"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    # Captions
    # name
    output$ui_nca_ana_name       = renderUI({
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)

      value       = current_ana[["key"]]

      uiele = textInput(inputId      = NS(id, "text_ana_key"),
                        label        = NULL,
                        value        = value,
                        placeholder  = state[["MC"]][["tooltips"]][["ph"]][["ana_key"]])

      uiele})
    #------------------------------------
    # notes
    output$ui_nca_ana_notes      = renderUI({
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)


      value       = current_ana[["notes"]]
      uiele = textAreaInput(inputId     = NS(id, "text_ana_notes"),
                           width        = state[["MC"]][["formatting"]][["notes"]][["width"]],
                           height       = state[["MC"]][["formatting"]][["notes"]][["height"]],
                           label        = NULL,
                           value        = value,
                           placeholder  = state[["MC"]][["tooltips"]][["ph"]][["notes"]])


      uiele})
    #------------------------------------
    output$ui_nca_curr_anas = renderUI({
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      if(is.null(names(state[["NCA"]][["anas"]]))){
        uiele  = tags$em(state[["MC"]][["labels"]][["curr_anas_none"]])
      } else {

        # This is the current analysis ID
        current_ana_id = state[["NCA"]][["current_ana"]]

        choices     = c()
        cnames      = c()
        subtext     = c()

        for(ana_id in names(state[["NCA"]][["anas"]])){
           tmp_ana = state[["NCA"]][["anas"]][[ana_id]]
           # Creating the select subtext from the caption
           if(is.null(tmp_ana[["notes"]])){
             subtext = c(subtext, "")
           } else {
             subtext = c(subtext, strtrim(tmp_ana[["notes"]], 20))
           }

           choices = c(choices, ana_id)
           cnames  = c(cnames,  tmp_ana[["key"]])
        }

        choicesOpt = list( subtext = subtext)
        names(choices) = cnames

        uiele =
        shinyWidgets::pickerInput(
          selected   = current_ana_id,
          inputId    = NS(id, "select_current_ana"),
          label      = state[["MC"]][["labels"]][["select_current_ana"]],
          choices    = choices,
          width      = state[["MC"]][["formatting"]][["select_current_ana"]][["width"]],
          choicesOpt = choicesOpt)
      }

      uiele})
    # Data Sources
    output$ui_nca_curr_views    = renderUI({
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      # This creates the selection but it is updated in the observe() below
      choicesOpt = NULL
      uiele =
        shinyWidgets::pickerInput(
          selected   = "PH",
          inputId    = NS(id, "select_current_view"),
          label      = state[["MC"]][["labels"]][["select_current_view"]],
          choices    = c("PH"),
          width      = state[["MC"]][["formatting"]][["select_current_view"]][["width"]],
          choicesOpt = choicesOpt)

      uiele})
    #------------------------------------
    # This forces the dataset view selection to update
    observe({
      input$button_ana_new
      input$button_ana_save
      input$button_ana_copy
      input$button_ana_del
      input$select_current_ana
      #req(input$select_current_view)


      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]
      # Forcing a reaction to changes in other modules
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      if(state[["NCA"]][["isgood"]]){
        # If this is triggered before datasets have been loaded the state will
        # be bad:
        # Pulling out the data set views catalog
        ds_catalog = state[["NCA"]][["DSV"]][["catalog"]]

        if(current_ana[["ana_dsview"]] %in% ds_catalog[["object"]]){
          current_view_id= current_ana[["ana_dsview"]]
        } else {
          current_view_id = ds_catalog[["object"]][1]
          formods::FM_le(state, paste0("ui_nca_curr_views: dataset view missing."   ))
          formods::FM_le(state, paste0("ana_key: ",     current_ana[["key"]]       ))
          formods::FM_le(state, paste0("ana_dsview: ",  current_ana[["ana_dsview"]]))
          formods::FM_le(state, paste0("switching to view:", current_view_id ))
        }

        choices        = ds_catalog[["object"]]
        names(choices) = ds_catalog[["label"]]

        choicesOpt = NULL
        shinyWidgets::updatePickerInput(
          session    = session,
          selected   = current_view_id,
          inputId    = "select_current_view",
          choices    = choices,
          choicesOpt = choicesOpt)
      }
    })
    #------------------------------------
    # interval range
    output$ui_nca_ana_int_range = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      # We need the time column defined to use this element
      req(input[["select_ana_col_time"]])


      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)



      # Pulling out the current analysis to get the column information
      uiele = NULL
      current_ana = NCA_fetch_current_ana(state)
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){
          value_start = current_ana[["interval_range"]][1]
          value_stop  = current_ana[["interval_range"]][2]


          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]
          # This is all the times in the dataset:
          time_choices = unique(sort(ds[["DS"]][[current_ana[["col_time"]] ]]))

          # Now we need to convert it to text and add infinity as an option
          time_choices = c(as.character(time_choices), "Inf")

          # Now we're going figure out if the interval in the current analysis
          # exists in the current choices:
          if((value_start %in% time_choices)){
             idx_start = which(value_start == time_choices)
          } else {
             idx_start = 1
          }

          if((value_stop %in% time_choices)){
             idx_stop  = which(value_stop  == time_choices)
          } else {
             idx_stop  = length(time_choices)
          }

          uiele =
            shinyWidgets::sliderTextInput(
            inputId   = NS(id, "select_ana_interval_range"),
            label     = state[["MC"]][["labels"]][["select_ana_interval_range"]],
            width     = state[["MC"]][["formatting"]][["select_ana_interval_range"]][["width"]],
            choices   = time_choices,
            selected  = time_choices[c(idx_start, idx_stop)]
           )
        }
      }

      uiele})
    #------------------------------------
    # NCA parameters to compute
    output$ui_nca_ana_params = renderUI({
      input$button_ana_use_scenario

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      # Currently selected parameters:
      value   = current_ana[["nca_parameters"]]

      # Choice list
      choices = state[["NCA"]][["nca_parameters"]][["choices"]]

      uiele =
      shinyWidgets::pickerInput(
        inputId    = NS(id, "select_ana_nca_parameters"),
        choices    = choices,
        label      = state[["MC"]][["labels"]][["select_ana_nca_parameters"]],
        selected   = value,
        multiple   = TRUE,
        width      = state[["MC"]][["formatting"]][["select_ana_nca_parameters"]][["width"]],
        options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
        inline     = TRUE)
      uiele})
    #------------------------------------
    # Data source sampling (sparse or serail)
    output$ui_nca_ana_source_sampling = renderUI({
      input$button_ana_use_scenario

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)
      value       = current_ana[["sampling"]]

      choices = list("Serial"="serial", "Sparse"="sparse")

      uiele =
      shinyWidgets::pickerInput(
        inputId    = NS(id, "select_ana_source_sampling"),
        choices    = choices,
        label      = state[["MC"]][["labels"]][["select_ana_source_sampling"]],
        selected   = value,
        width      = state[["MC"]][["formatting"]][["select_ana_source_sampling"]][["width"]],
        inline     = TRUE)

      uiele})
    #------------------------------------
    # Data source sampling (sparse or serail)
    output$ui_nca_ana_run             = renderUI({
      input$button_ana_use_scenario

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_run"),
                label   = state[["MC"]][["labels"]][["run_ana"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_run"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_run"]][["block"]],
                color   = "primary",
                icon    = icon("arrow-down"))

      uiele})
    #------------------------------------
    # Figure Results
    output$ui_nca_ana_results_fig             = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]


      uiele = tagList(
             htmlOutput(  NS(id, "ui_nca_ana_results_fig_general")),
             htmlOutput(  NS(id, "ui_nca_ana_results_fig_controls")),
             htmlOutput(  NS(id, "ui_nca_ana_results_fig_plot")))



      uiele})
    #------------------------------------
    # Table Results (show table)
    output$ui_nca_ana_results_tab_table              = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_tab"]]

      # Update on save
      input[["button_tb_ind_obs_save"]]
      input[["button_tb_ind_params_save"]]
      input[["button_tb_sum_params_save"]]

      # Don't build out the table until the tab_view has been created
      req(input[["select_ana_tab_view"]])

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL
      if(current_ana[["isgood"]]){
        fobj = NCA_fetch_current_obj(state, "table")

       #pvw          = state[["MC"]][["formatting"]][["preview"]][["width"]]
       #pvh          = state[["MC"]][["formatting"]][["preview"]][["height"]]
       #pv_div_style = paste0("height:",pvh,";width:",pvw,";display:inline-block;vertical-align:top")
        # If we fetched everything then we return the table
        if(fobj[["isgood"]]){
          if(current_ana[["tab_type"]] == "report"){
            uiele = flextable::htmltools_value(fobj[["ft"]])
          } else {
            uiele = DT::DTOutput(NS(id, "ui_nca_ana_results_tab_table_dt"))
          }
        } else {
          # Otherwise we return messages to the user
          uiele = fobj[["msgs"]]
        }
      }

    uiele})
    #------------------------------------
    # Table Results (show table data table DT output)
    output$ui_nca_ana_results_tab_table_dt           = DT::renderDT({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_tab"]]

      # Update on save
      input[["button_tb_ind_obs_save"]]
      input[["button_tb_ind_params_save"]]
      input[["button_tb_sum_params_save"]]

      # Don't build out the table until the tab_view has been created
      req(input[["select_ana_tab_view"]])

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL
      if(current_ana[["isgood"]]){
        fobj = NCA_fetch_current_obj(state, "table")
        uiele = fobj[["df"]]
      }

    uiele})
    #------------------------------------
    # Figure Results (show plot)
    output$ui_nca_ana_results_fig_plot               = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_fig"]]
      input[["button_fg_ind_obs_save"]]


      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      # pvh          = state[["MC"]][["formatting"]][["preview"]][["height"]]
      # pvw          = state[["MC"]][["formatting"]][["preview"]][["width"]]
      # pv_div_style = paste0("height:",pvh,"px;width:",pvw,"px;display:inline-block;vertical-align:top")

      pvw          = state[["MC"]][["formatting"]][["preview"]][["width"]]
      pvh          = state[["MC"]][["formatting"]][["preview"]][["height"]]
      pv_div_style = paste0("height:",pvh,";width:",pvw,";display:inline-block;vertical-align:top")
      if(current_ana[["fig_type"]] == "interactive"){
        uiele = tagList(
           div(style=pv_div_style, plotly::plotlyOutput(NS(id, "ui_nca_ana_results_fig_plotly"), width=pvw, height=pvh)))
      } else {
        uiele = tagList(
           div(style=pv_div_style, plotOutput(  NS(id, "ui_nca_ana_results_fig_ggplot"), width=pvw, height=pvh)))
      }
    uiele})
    #------------------------------------
    # Figure Results (select figure)
    output$ui_nca_ana_results_fig_general            = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL

      # We only generate the ui element if the
      # analysis is in a good state
      if(current_ana[["isgood"]]){

        choices = c()
        cnames  = c()
        subtext = c()

        for(fig_id in names(state[["MC"]][["figures"]])){
          choices = c(choices, fig_id)
          cnames  = c(cnames,  state[["MC"]][["figures"]][[fig_id]][["choice"]])
          subtext = c(subtext, state[["MC"]][["figures"]][[fig_id]][["subtext"]])
        }

        # Applying names
        names(choices) = cnames

        # Adding the subtext
        choicesOpt = list(
          subtext = subtext)

        # If the current_ana figure view exists we default to that
        # otherwise we just choose the first choice.
        if(current_ana[["fig_view"]] %in% choices){
          selected = current_ana[["fig_view"]]
        } else {
          selected = choices[1]
        }

        uiele_picker =  shinyWidgets::pickerInput(
             inputId    = NS(id, "select_ana_fig_view"),
             choices    = choices,
             selected   = selected,
             width      = state[["MC"]][["formatting"]][["select_ana_fig_view"]][["width"]],
             inline     = TRUE,
             choicesOpt = choicesOpt)

        choiceValues = c("report", "interactive")
        choiceNames  = c(state[["MC"]][["labels"]][["switch_ana_fig_report"]],
                         state[["MC"]][["labels"]][["switch_ana_fig_interactive"]])

        uiele_switch =
          shinyWidgets::radioGroupButtons(
           inputId       = NS(id, "switch_ana_fig"),
           label         = state[["MC"]][["labels"]][["switch_ana_fig"]],
           selected      = current_ana[["fig_type"]],
           choiceValues  = choiceValues,
           choiceNames   = choiceNames ,
           status        = "primary")

        pv_div_style = paste0("display:inline-block") #;vertical-align:top")

        uiele = tagList(div(style=pv_div_style, uiele_picker),
                        div(style=pv_div_style, uiele_switch))
      } else {
        uiele = state[["MC"]][["errors"]][["nca_no_fig"]]
      }

      uiele})
    #------------------------------------
    # Figure Results ggplot
    output$ui_nca_ana_results_fig_ggplot             = renderPlot({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_fig"]]

      input[["button_fg_ind_obs_save"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL
      # We only return a non-NULL result if the current analysis is good
      # This should only be called if an interactive as _not_ been selected
      # so we just need to return the ggplot object.
      if(current_ana[["isgood"]]){
        # Pulling out the current figure
        fobj = NCA_fetch_current_obj(state, "figure")
        if(!is.null(fobj)){
         uiele = fobj[["ggplot"]]
        }
      }
      uiele})

    #------------------------------------
    # Figure Results plotly
    output$ui_nca_ana_results_fig_plotly             = plotly::renderPlotly({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_fig"]]

      input[["button_fg_ind_obs_save"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL
      # We only return a non-NULL result if the current analysis is good
      # and this should only be called if an interactive figure has been
      # selected. For that we need the ggplotly output of the ggplot object:
      if(current_ana[["isgood"]]){
         fobj = NCA_fetch_current_obj(state, "figure")
        if(!is.null(fobj)){
          uiele = plotly::ggplotly(fobj[["ggplot"]])
        }
      }
      uiele})
    #------------------------------------
    # Figure Results (plot controls)
    output$ui_nca_ana_results_fig_controls           = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_fig"]]
      # This is dependent on the figure view so
      # shouldn't build out until that.
      req(input[["select_ana_fig_view"]])

      #input[["button_fg_ind_obs_save"]]
      state = NCA_fetch_state(id             = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = NULL

      current_ana = NCA_fetch_current_ana(state)

      # If the current analysis state is bad then we return NULL and wait for
      # the user to fix it.
      if(current_ana[["isgood"]]){
        if(current_ana[["fig_view"]] == "fg_ind_obs"){

          uiele_save =
          shinyWidgets::actionBttn(
            inputId = NS(id, "button_fg_ind_obs_save"),
            label   = state[["MC"]][["labels"]][["fg_save"]],
            style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
            size    = state[["MC"]][["formatting"]][["button_fg_save"]][["size"]],
            block   = state[["MC"]][["formatting"]][["button_fg_save"]][["block"]],
            color   = "primary",
            icon    = icon("arrow-down"))

          uiele_nrow =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_fg_ind_obs_nrow"),
            choices    = state[["MC"]][["formatting"]][["preview"]][["nrow_opt"]],
            label      = state[["MC"]][["labels"]][["select_fg_ind_obs_nrow"]],
            selected   = current_ana[["fg_ind_obs_nrow"]],
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_fg_ind_obs_nrow"]][["width"]],
            )
          uiele_ncol =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_fg_ind_obs_ncol"),
            choices    = state[["MC"]][["formatting"]][["preview"]][["ncol_opt"]],
            label      = state[["MC"]][["labels"]][["select_fg_ind_obs_ncol"]],
            selected   = current_ana[["fg_ind_obs_ncol"]],
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_fg_ind_obs_ncol"]][["width"]]
            )

          uiele_page = htmlOutput(NS(id, "ui_nca_ana_results_fig_pages"))

          uiele_logy = shinyWidgets::awesomeCheckbox(
                   inputId  = NS(id, "check_fg_ind_obs_logy"),
                   label    = state[["MC"]][["labels"]][["check_fg_ind_obs_logy"]],
                   width    = state[["MC"]][["formatting"]][["check_fg_ind_obs"]][["width"]],
                   value    = current_ana[["fg_ind_obs_logy"]])
          dvstyle = "display:inline-block"
          uiele = tagList(div(style=dvstyle, uiele_save),
                          div(style=dvstyle, uiele_nrow),
                          div(style=dvstyle, uiele_ncol),
                          div(style=dvstyle, uiele_logy),
                          div(style=dvstyle, uiele_page))
        }
      }

      uiele})
    #------------------------------------
    # Table page ui element
    output$ui_nca_ana_results_tab_pages              = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      # This is dependent on the table view so
      # shouldn't build out until that.
      req(input[["select_ana_tab_view"]])

      input[["button_tb_ind_obs_save"]]
      input[["button_tb_ind_params_save"]]
      input[["button_tb_sum_params_save"]]
      state = NCA_fetch_state(id             = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = NULL

      current_ana = NCA_fetch_current_ana(state)

      # JMH here: Generalize for all tables and add
      #    - tb_ind_params
      #    - tb_sum_params

      if(current_ana[["isgood"]]){
        if(current_ana[["tab_view"]] == "tb_ind_obs"){
          page_values = names(current_ana[["objs"]][["tb_ind_obs"]][["value"]][["tables"]])
          if(current_ana[["curr_tb_ind_obs"]] %in% page_values){
            selected_page = current_ana[["curr_tb_ind_obs"]]
          } else {
            selected_page = page_values[1]
          }
          names(page_values) = 1:length(page_values)

          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_tb_ind_obs_page"),
            choices    = page_values,
            label      = state[["MC"]][["labels"]][["select_tb_page"]],
            selected   = selected_page,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_tb_page"]][["width"]]
            )
        }
        if(current_ana[["tab_view"]] == "tb_ind_params"){
          page_values = names(current_ana[["objs"]][["tb_ind_params"]][["value"]][["tables"]])
          if(current_ana[["curr_tb_ind_params"]] %in% page_values){
            selected_page = current_ana[["curr_tb_ind_params"]]
          } else {
            selected_page = page_values[1]
          }
          names(page_values) = 1:length(page_values)

          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_tb_ind_params_page"),
            choices    = page_values,
            label      = state[["MC"]][["labels"]][["select_tb_page"]],
            selected   = selected_page,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_tb_page"]][["width"]]
            )
        }
        if(current_ana[["tab_view"]] == "tb_sum_params"){
        }
      }
    uiele})
    #------------------------------------
    # Figure page ui element
    output$ui_nca_ana_results_fig_pages              = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      input[["switch_ana_fig"]]
      # This is dependent on the figure view so
      # shouldn't build out until that.
      req(input[["select_ana_fig_view"]])

      input[["button_fg_ind_obs_save"]]
      state = NCA_fetch_state(id             = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = NULL

      current_ana = NCA_fetch_current_ana(state)

      if(current_ana[["isgood"]]){
        page_values = names(current_ana[["objs"]][["fg_ind_obs"]][["value"]][["figures"]])
        if(current_ana[["curr_fg_ind_obs"]] %in% page_values){
          selected_page = current_ana[["curr_fg_ind_obs"]]
        } else {
          selected_page = page_values[1]
        }
        names(page_values) = 1:length(page_values)

        uiele =
        shinyWidgets::pickerInput(
          inputId    = NS(id, "select_fg_ind_obs_page"),
          choices    = page_values,
          label      = state[["MC"]][["labels"]][["select_fg_ind_obs_page"]],
          selected   = selected_page,
          options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
          width      = state[["MC"]][["formatting"]][["select_fg_ind_obs_page"]][["width"]]
          )
      }




    uiele})
    #------------------------------------
    # Tabular Results
    output$ui_nca_ana_results_tab             = renderUI({
      input$button_ana_run

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = tagList(
             htmlOutput(NS(id, "ui_nca_ana_results_tab_general")),
             htmlOutput(NS(id, "ui_nca_ana_results_tab_controls")),
             htmlOutput(NS(id, "ui_nca_ana_results_tab_table"))
        )
      uiele})

    #------------------------------------
    # Table Results (table controls)
    output$ui_nca_ana_results_tab_controls           = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      # This is dependent on the table view so
      # shouldn't build out until that.
      req(input[["select_ana_tab_view"]])

      state = NCA_fetch_state(id             = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = NULL

      current_ana = NCA_fetch_current_ana(state)

      # this is the common ui to select pages that's used below
      uiele_page = htmlOutput(NS(id, "ui_nca_ana_results_tab_pages"))

      # If the current analysis state is bad then we return NULL and wait for
      # the user to fix it.
      if(current_ana[["isgood"]]){
        if(current_ana[["tab_view"]] == "tb_ind_obs"){
          uiele_save =
          shinyWidgets::actionBttn(
            inputId = NS(id, "button_tb_ind_obs_save"),
            label   = state[["MC"]][["labels"]][["tb_save"]],
            style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
            size    = state[["MC"]][["formatting"]][["button_tb_save"]][["size"]],
            block   = state[["MC"]][["formatting"]][["button_tb_save"]][["block"]],
            color   = "primary",
            icon    = icon("arrow-down"))

          dvstyle = "display:inline-block"
          uiele = tagList(div(style=dvstyle, uiele_save),
                          div(style=dvstyle, uiele_page))
        }
        if(current_ana[["tab_view"]] == "tb_nca_raw"){
          # Currently there are no controls for the raw NCA output
        }
        if(current_ana[["tab_view"]] == "tb_ind_params"){
          uiele_save =
          shinyWidgets::actionBttn(
            inputId = NS(id, "button_tb_ind_params_save"),
            label   = state[["MC"]][["labels"]][["tb_save"]],
            style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
            size    = state[["MC"]][["formatting"]][["button_tb_save"]][["size"]],
            block   = state[["MC"]][["formatting"]][["button_tb_save"]][["block"]],
            color   = "primary",
            icon    = icon("arrow-down"))

          dvstyle = "display:inline-block"
          uiele = tagList(div(style=dvstyle, uiele_save),
                          div(style=dvstyle, uiele_page))
        }
        if(current_ana[["tab_view"]] == "tb_sum_params"){
          # JMH add controls
          uiele_save =
          shinyWidgets::actionBttn(
            inputId = NS(id, "button_tb_sum_params_save"),
            label   = state[["MC"]][["labels"]][["tb_save"]],
            style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
            size    = state[["MC"]][["formatting"]][["button_tb_save"]][["size"]],
            block   = state[["MC"]][["formatting"]][["button_tb_save"]][["block"]],
            color   = "primary",
            icon    = icon("arrow-down"))

          dvstyle = "display:inline-block"
          uiele = tagList(div(style=dvstyle, uiele_save),
                          div(style=dvstyle, uiele_page))
        }
      }

      uiele})
    #------------------------------------
    # Table Results (select table)
    output$ui_nca_ana_results_tab_general            = renderUI({
      input[["button_ana_run"]]
      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL

      # We only generate the ui element if the
      # analysis is in a good state
      if(current_ana[["isgood"]]){

        choices = c()
        cnames  = c()
        subtext = c()

        for(fig_id in names(state[["MC"]][["tables"]])){
          choices = c(choices, fig_id)
          cnames  = c(cnames,  state[["MC"]][["tables"]][[fig_id]][["choice"]])
          subtext = c(subtext, state[["MC"]][["tables"]][[fig_id]][["subtext"]])
        }

        # Applying names
        names(choices) = cnames

        # Adding the subtext
        choicesOpt = list(
          subtext = subtext)

        # If the current_ana figure view exists we default to that
        # otherwise we just choose the first choice.
        if(current_ana[["tab_view"]] %in% choices){
          selected = current_ana[["tab_view"]]
        } else {
          selected = choices[1]
        }

        uiele_picker =  shinyWidgets::pickerInput(
             inputId    = NS(id, "select_ana_tab_view"),
             choices    = choices,
             selected   = selected,
             width      = state[["MC"]][["formatting"]][["select_ana_fig_view"]][["width"]],
             inline     = TRUE,
             choicesOpt = choicesOpt)

      # uiele_switch =
      # shinyWidgets::materialSwitch(
      #    inputId = NS(id, "switch_ana_tab_type"),
      #    label   = state[["MC"]][["labels"]][["switch_ana_tab_type"]],
      #    value   = current_ana[["tab_type_rpt"]],
      #    inline  = TRUE,
      #    status  = "success"
      # )
      #
      # uiele = tagList(uiele_picker, uiele_switch)

        choiceValues = c("report", "interactive")
        choiceNames  = c(state[["MC"]][["labels"]][["switch_ana_tab_report"]],
                         state[["MC"]][["labels"]][["switch_ana_tab_interactive"]])

        uiele_switch =
          shinyWidgets::radioGroupButtons(
           inputId       = NS(id, "switch_ana_tab"),
           label         = state[["MC"]][["labels"]][["switch_ana_tab"]],
           selected      = current_ana[["tab_type"]],
           choiceValues  = choiceValues,
           choiceNames   = choiceNames ,
           status        = "primary")

        pv_div_style = paste0("display:inline-block") #;vertical-align:top")

        uiele = tagList(div(style=pv_div_style, uiele_picker),
                        div(style=pv_div_style, uiele_switch))




      } else {
        uiele = state[["MC"]][["errors"]][["nca_no_tab"]]
      }

      uiele})
    #------------------------------------
    # Column Mapping
    # id column
    output$ui_nca_ana_col_id = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])


          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_id"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_id"]],
            patterns = state[["MC"]][["detect_col"]][["id"]],
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_id"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_id"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_id"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # nominal time column
    output$ui_nca_ana_col_ntime = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_ntime"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_ntime"]],
            patterns = state[["MC"]][["detect_col"]][["ntime"]],
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_ntime"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_ntime"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_ntime"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # time column
    output$ui_nca_ana_col_time = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_time"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_time"]],
            patterns = state[["MC"]][["detect_col"]][["time"]],
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_time"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_time"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_time"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # dose column
    output$ui_nca_ana_col_dose = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_dose"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_dose"]],
            patterns = state[["MC"]][["detect_col"]][["dose"]],
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_dose"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_dose"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_dose"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # dur  column
    output$ui_nca_ana_col_dur  = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_dur"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_dur"]],
            patterns = state[["MC"]][["detect_col"]][["dur"]],
            null_ok  = TRUE,
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_dur"),
            choices    = c("N/A", dscols),
            label      = state[["MC"]][["labels"]][["select_ana_col_dur"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_dur"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # rotue column
    output$ui_nca_ana_col_route = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_route"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_route"]],
            patterns = state[["MC"]][["detect_col"]][["route"]],
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_route"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_route"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_route"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # conc column
    output$ui_nca_ana_col_conc = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_conc"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_conc"]],
            patterns = state[["MC"]][["detect_col"]][["conc"]],
            dscols   = dscols)

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_conc"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_conc"]],
            selected   = value,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_conc"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # group column
    output$ui_nca_ana_col_group = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL

      # This only generates the UI element if there is a data set
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_group"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_group"]],
            patterns = state[["MC"]][["detect_col"]][["group"]],
            null_ok  = TRUE,
            dscols   = dscols)

          #JMH add multiple option to NCA_find_col
          # react to dose, id and time and remove those options

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_group"),
            choices    = dscols,
            label      = state[["MC"]][["labels"]][["select_ana_col_group"]],
            selected   = value,
            multiple   = TRUE,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_group"]][["width"]],
            inline     = TRUE)
        }
      }

      uiele})
    #------------------------------------
    # group column
    output$ui_nca_ana_col_analyte = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["button_ana_save"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)
      uiele = NULL
      if(!is.null(current_ana[["ana_dsview"]])){
        if(!is.null(state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]])){

          # Pulling out the dataset list
          ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

          # These are the columns in the dataset:
          dscols = names(ds[["DS"]])

          # Finding the value to use:
          value = NCA_find_col(
            curr_ana = current_ana[["col_analyte"]],
            curr_ui  = state[["NCA"]][["ui"]][["select_ana_col_analyte"]],
            patterns = state[["MC"]][["detect_col"]][["analyte"]],
            null_ok  = TRUE,
            dscols   = dscols)

          #JMH add multiple option to NCA_find_col
          # react to dose, id and time and remove those options

          # Creating the selection input
          uiele =
          shinyWidgets::pickerInput(
            inputId    = NS(id, "select_ana_col_analyte"),
            choices    = c("N/A", dscols),
            label      = state[["MC"]][["labels"]][["select_ana_col_analyte"]],
            selected   = value,
            multiple   = FALSE,
            options    = list(size = state[["yaml"]][["FM"]][["ui"]][["select_size"]]),
            width      = state[["MC"]][["formatting"]][["select_ana_col_analyte"]][["width"]],
            inline     = TRUE)
        }
      }
      uiele})
    #------------------------------------
    # Specifying Units
    # include units checkbox
    output$ui_nca_ana_check_units = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)


      current_ana = NCA_fetch_current_ana(state)

      uiele =
      shinyWidgets::materialSwitch(
         inputId = NS(id, "switch_ana_include_units"),
         label   = state[["MC"]][["labels"]][["switch_ana_include_units"]],
         value   = current_ana[["include_units"]],
         status  = "success"
      )
      uiele})
    #------------------------------------
    # amt units
    output$ui_nca_ana_units_amt = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      req(input$switch_ana_include_units)

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

     current_ana = NCA_fetch_current_ana(state)

     uiele = NULL

     # We only show the selection if include units is checked.
     if(input$switch_ana_include_units){
       uiele =
       shinyWidgets::pickerInput(
         inputId    = NS(id, "select_ana_units_amt"),
         choices    = state[["MC"]][["units"]][["amt"]],
         label      = state[["MC"]][["labels"]][["select_ana_units_amt"]],
         selected   = current_ana[["units_amt"]],
         multiple   = FALSE,
         width      = state[["MC"]][["formatting"]][["select_ana_units_amt"]][["width"]],
         inline     = TRUE)
     }
      uiele})
    #------------------------------------
    #  units
    output$ui_nca_ana_units_dose = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      req(input$switch_ana_include_units)

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

     current_ana = NCA_fetch_current_ana(state)

     uiele = NULL

     # We only show the selection if include units is checked.
     if(input$switch_ana_include_units){
       uiele =
       shinyWidgets::pickerInput(
         inputId    = NS(id, "select_ana_units_dose"),
         choices    = state[["MC"]][["units"]][["dose"]],
         label      = state[["MC"]][["labels"]][["select_ana_units_dose"]],
         selected   = current_ana[["units_dose"]],
         multiple   = FALSE,
         width      = state[["MC"]][["formatting"]][["select_ana_units_dose"]][["width"]],
         inline     = TRUE)
     }
      uiele})
    #------------------------------------
    # conc units
    output$ui_nca_ana_units_conc = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      req(input$switch_ana_include_units)

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

     current_ana = NCA_fetch_current_ana(state)

     uiele = NULL

     # We only show the selection if include units is checked.
     if(input$switch_ana_include_units){
       uiele =
       shinyWidgets::pickerInput(
         inputId    = NS(id, "select_ana_units_conc"),
         choices    = state[["MC"]][["units"]][["conc"]],
         label      = state[["MC"]][["labels"]][["select_ana_units_conc"]],
         selected   = current_ana[["units_conc"]],
         multiple   = FALSE,
         width      = state[["MC"]][["formatting"]][["select_ana_units_conc"]][["width"]],
         inline     = TRUE)
     }
      uiele})
    #------------------------------------
    # time units
    output$ui_nca_ana_units_time = renderUI({

      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      req(input$switch_ana_include_units)

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

     current_ana = NCA_fetch_current_ana(state)

     uiele = NULL

     # We only show the selection if include units is checked.
     if(input$switch_ana_include_units){
       uiele =
       shinyWidgets::pickerInput(
         inputId    = NS(id, "select_ana_units_time"),
         choices    = state[["MC"]][["units"]][["time"]],
         label      = state[["MC"]][["labels"]][["select_ana_units_time"]],
         selected   = current_ana[["units_time"]],
         multiple   = FALSE,
         width      = state[["MC"]][["formatting"]][["select_ana_units_time"]][["width"]],
         inline     = TRUE)
     }
      uiele})
    #------------------------------------
    # Analysis Scenarios
    # Scenario selector
    output$ui_nca_ana_scenario = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      # Pulling out the current analysis
      current_ana = NCA_fetch_current_ana(state)

      # Pulling out the scenario for the current analysis
      ana_scenario = current_ana[["ana_scenario"]]

      # Building the selector from the scenarios in the yaml file:
      choices = list()
      for(choice in names(state[["MC"]][["ana_scenarios"]])){
        choices[[ state[["MC"]][["ana_scenarios"]][[choice]][["description"]] ]] = choice
      }

      uiele =
      shinyWidgets::pickerInput(
        selected   = ana_scenario,
        inputId    = NS(id, "select_ana_scenario"),
        label      = state[["MC"]][["labels"]][["select_ana_scenario"]],
        choices    = choices,
        width      = state[["MC"]][["formatting"]][["select_ana_scenario"]][["width"]])

      uiele})
    #------------------------------------
    # Add interval button
    output$ui_nca_ana_add_int      = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_add_int"),
                label   = state[["MC"]][["labels"]][["ana_add_int"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_add_int"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_add_int"]][["block"]],
                color   = "success",
                icon    = icon("plus-sign", lib="glyphicon"))
      uiele})
    #------------------------------------
    # Scenario button
    output$ui_nca_ana_scenario_use = renderUI({
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      uiele = shinyWidgets::actionBttn(
                inputId = NS(id, "button_ana_use_scenario"),
                label   = state[["MC"]][["labels"]][["ana_use_scenario"]],
                style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                size    = state[["MC"]][["formatting"]][["button_ana_use_scenario"]][["size"]],
                block   = state[["MC"]][["formatting"]][["button_ana_use_scenario"]][["block"]],
                color   = "primary",
                icon    = icon("arrow-down"))
      uiele})
    #------------------------------------
    # Analysis Options
    output$ui_nca_ana_options    = renderUI({

      input[["button_ana_new"]]
      input[["button_ana_save"]]
      input[["button_ana_del"]]
      input[["button_ana_run"]]
      input[["button_ana_copy"]]
      input[["select_current_ana"]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      # Pulling out the current analysis
      current_ana = NCA_fetch_current_ana(state)

      # NCA configuration  for the current analysis
      nca_config = current_ana[["nca_config"]]

      # NCA configuration summary table
      nc_summary = state[["NCA"]][["nca_config"]][["summary"]]


      # This will hold all of the tabs:
      uiele = tagList()

      # Pulling the option width from the config file
      opt_width =  state[["MC"]][["formatting"]][["nca_config_option"]][["width"]]
      for(tmpgroup in unique(nc_summary[["group"]])){
        group_ele = tagList()
        for(cfg_ele in nc_summary[nc_summary[["group"]] == tmpgroup, ]$key){
          if(cfg_ele %in% names(nca_config)){


          # Determining if we need to add a tool tip
          ADD_TOOL_TIP = FALSE
          if(state[["MC"]][["tooltips"]][["include"]]){
            # If a tooltip is defined for this element then we attach it
            if(!is.null((nca_config[[cfg_ele]][["tooltip"]]))){
              if(system.file(package="prompter") != ""){
                ADD_TOOL_TIP = TRUE
              }
            }
          }

            tmp_tt = NULL

            if(is.null(nca_config[[cfg_ele]][["options"]])){
              # If no options are specified we build out a text box
              tmp_tI = textInput(
                    inputId = NS(id, nca_config[[cfg_ele]][["ui_id"]]),
                    width   = opt_width,
                    label   = nca_config[[cfg_ele]][["label"]],
                    value   = nca_config[[cfg_ele]][["value"]]
                    )

              # Adding tooltip if necessary
              if(ADD_TOOL_TIP){
                tmp_tI = prompter::add_prompt(
                  tmp_tI,
                  position = "right",
                  size     = "medium",
                  message  = nca_config[[cfg_ele]][["tooltip"]]
                )
              }

              # Adding the text input
              group_ele = tagList(group_ele,
                div(style="display:inline-block;vertical-align:top", #align-items:bottom;justify-content:center",
                    tmp_tI
                  )
                )
            } else {
              # If there are options then we create a select box
              tmp_sI =
                  selectInput(inputId  = NS(id, nca_config[[cfg_ele]][["ui_id"]]),
                              width    = opt_width,
                              label    = nca_config[[cfg_ele]][["label"]],
                              selected = nca_config[[cfg_ele]][["value"]],
                              choices  = nca_config[[cfg_ele]][["options"]]
                  )

              # Adding tooltip if necessary
              if(ADD_TOOL_TIP){
                tmp_sI = prompter::add_prompt(
                  tmp_sI,
                  position = "right",
                  size     = "medium",
                  message  = nca_config[[cfg_ele]][["tooltip"]]
                )
              }

              # Adding the select input
              group_ele = tagList(group_ele,
                div(style="display:inline-block;vertical-align:top", #align-items:bottom;justify-content:center",
                  tmp_sI
                )
              )
            }
          }
        }

        # Adding the current group as a tab:
        uiele = tagList(uiele,tags$h3(tmpgroup), group_ele)
      }


      uiele})
    #------------------------------------
    # Current DW elements
    output$hot_nca_intervals = rhandsontable::renderRHandsontable({
      input[["button_ana_add_int"]]
      input[["button_ana_use_scenario"]]
      input[["button_ana_new"]]
      input[["button_ana_del"]]
      input[["button_ana_copy"]]

      # This forces reaction to the delete button
      input$hot_nca_intervals

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)
      # Pulling out the current analysis
      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL

      # Pulling out the widths to make it easier to deal with them below
      w_start     = state[["MC"]][["formatting"]][["intervals_start"]][["width"]]
      w_stop      = state[["MC"]][["formatting"]][["intervals_stop"]][["width"]]
      w_np_text   = state[["MC"]][["formatting"]][["intervals_np_text"]][["width"]]
      w_delete    = state[["MC"]][["formatting"]][["intervals_delete"]][["width"]]

      # Total for the no intervals table
      w_total = w_start + w_stop + w_np_text + w_delete

      # By default intervals here is NULL and when new intervals are added it
      # will become an dataframe:
      if(is.null(current_ana[["intervals"]])){
        df = data.frame("Intervals"= state[["MC"]][["labels"]][["no_intervals"]])
        uiele = rhandsontable::rhandsontable(
          df,
          width  = state[["MC"]][["formatting"]][["intervals"]][["width"]],
          height = state[["MC"]][["formatting"]][["intervals"]][["height"]],
          rowHeaders = NULL
          ) |>
          hot_cols(colWidths = c(w_total))
      } else {
        # The user only sees np_text (pretty names) and not np_actual (actual
        # names)
        df =   current_ana[["intervals"]]


        # This will force things like 0 to be 0 instead of 0.0 and Inf to show
        # up correctly.
        df[["start"]] = as.character(df[["start"]])
        df[["stop"]]  = as.character(df[["stop"]])

        df = dplyr::select(df, "start", "stop", "np_text", "delete")
        df = dplyr::rename(df, "Start"          = "start")
        df = dplyr::rename(df, "Stop"           = "stop")
        df = dplyr::rename(df, "NCA Parameter"  = "np_text")
        df = dplyr::rename(df, "Delete"         = "delete")

        uiele = rhandsontable::rhandsontable(
          df,
          width  = state[["MC"]][["formatting"]][["intervals"]][["width"]],
          height = state[["MC"]][["formatting"]][["intervals"]][["height"]],
          rowHeaders = NULL
          ) |>
          hot_col("Start" ,           readOnly = TRUE) |>
          hot_col("Stop" ,            readOnly = TRUE) |>
          hot_col("NCA Parameter" ,   readOnly = TRUE) |>
          hot_cols(colWidths = c(w_start, w_stop, w_np_text, w_delete))


      }
    })
    #------------------------------------
    # This can be used to trigger notifications
    toNotify <- reactive({
      list(input$button_ana_add_int,
           input$button_ana_save,
           input$button_ana_copy,
           input$button_ana_del,
           input$button_ana_new,
           input$button_ana_use_scenario)
    })
    observeEvent(toNotify(), {
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      # Triggering optional notifications
      notify_res = formods::FM_notify(
        state   = state,
        session = session)
    })
    # Creates the ui for the compact view of the module
    #------------------------------------
    output$NCA_ui_compact  =  renderUI({
      # Forcing a reaction to changes in other modules
      react_state[[id_UD]]
      react_state[[id_DW]]
      react_state[[id_ASM]]

      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      current_ana = NCA_fetch_current_ana(state)

      uiele = NULL
      if(is.null(current_ana)){
        uiele = state[["MC"]][["labels"]][["no_dataset"]]
      } else {
        uiele_code_button = NULL
        # Generating code button if enabled
        if( state[["MC"]][["compact"]][["code"]]){
          uiele_code = tagList(shinyAce::aceEditor(
            NS(id, "ui_nca_code"),
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
             status  = "danger btn-custom-nca",
             icon    = icon("code", lib="font-awesome"),
             tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
          )

        # Button with FG elements table
        uiele_nca_intervals = rhandsontable::rHandsontableOutput(NS(id, "hot_nca_intervals"))
        uiele_nca_intervals_button = tagList(
         shinyWidgets::dropdownButton(
           uiele_nca_intervals,
           inline  = FALSE,
           right   = TRUE ,
           size    = "sm",
           circle  = FALSE,
           status  = "primary btn-custom-nca",
           icon    = icon("layer-group", lib="font-awesome"),
           tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["nca_intervals"]]))
        )

        }


        # We only show the clip button if it's enabled
        uiele_clip_button = NULL
        if(state[["MC"]][["compact"]][["clip"]]){
          uiele_clip_button = htmlOutput(NS(id, "ui_nca_clip_code"))
        }


        uiele_buttons_right = tagList(
                 tags$style(".btn-custom-nca {width: 100px;}"),
                 div(style="display:inline-block;vertical-align:top",
                 uiele_nca_intervals_button,
                 uiele_code_button,
                 uiele_clip_button,
                 htmlOutput(NS(id, "ui_nca_save_ana")),
                 htmlOutput(NS(id, "ui_nca_copy_ana")),
                 htmlOutput(NS(id, "ui_nca_del_ana")),
                 htmlOutput(NS(id, "ui_nca_new_ana"))
                 ))


        # Main NCA options tab
        uiele_nca_options = tagList(
          tags$h3(state[["MC"]][["labels"]][["head_analysis_template"]]),
          div(style="display:inline-block;vertical-align:bottom",
            htmlOutput(NS(id, "ui_nca_ana_scenario"))
            ),
          div(style="display:inline-block;vertical-align:bottom",
            htmlOutput(NS(id, "ui_nca_ana_scenario_use")), tags$br()
            ),
          tags$br(),
          tags$h3(state[["MC"]][["labels"]][["head_intervals"]]),
          tags$h4(state[["MC"]][["labels"]][["head_intervals_create"]]),
          div(style="display:inline-block;vertical-align:bottom",
            htmlOutput(NS(id, "ui_nca_ana_int_range"))
            ),
          div(style="display:inline-block;vertical-align:bottom",
            htmlOutput(NS(id, "ui_nca_ana_params"))
            ),
          div(style="display:inline-block;vertical-align:bottom",
            htmlOutput(NS(id, "ui_nca_ana_add_int")), tags$br()
            ),
          #tags$br(),
          #tags$h4(state[["MC"]][["labels"]][["head_intervals_current"]]),
          #tagList(rhandsontable::rHandsontableOutput(NS(id, "hot_nca_intervals"))),
          tags$br(),
          tags$h3(state[["MC"]][["labels"]][["head_col_mapping"]]),
          tags$h4(state[["MC"]][["labels"]][["head_col_mapping_required"]]),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_id"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_time"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_ntime"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_conc"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_dose"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_route"))),
          tags$br(),
          tags$h4(state[["MC"]][["labels"]][["head_col_mapping_optional"]]),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_group"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_analyte"))),
           div(style="display:inline-block",
             htmlOutput(NS("NCA", "ui_nca_ana_col_dur"))),
          tags$br(),
          tags$h3(state[["MC"]][["labels"]][["head_units"]]),
          div(style="display:inline-block",
            htmlOutput(NS(id, "ui_nca_ana_check_units"))
            ),
          tags$br(),
          div(style="display:inline-block",
            htmlOutput(NS(id, "ui_nca_ana_units_time"))
            ),
          div(style="display:inline-block",
            htmlOutput(NS(id, "ui_nca_ana_units_dose"))
            ),
          div(style="display:inline-block",
            htmlOutput(NS(id, "ui_nca_ana_units_conc"))
            ),
          div(style="display:inline-block",
            htmlOutput(NS(id, "ui_nca_ana_units_amt"))
            ),
          tags$br(),
          tags$h3(state[["MC"]][["labels"]][["head_run_analysis"]]),
          div(style="display:inline-block;vertical-align:bottom",
             htmlOutput(NS(id, "ui_nca_ana_source_sampling"))
             ),
          div(style="display:inline-block;vertical-align:bottom",
             htmlOutput(NS(id, "ui_nca_ana_run")), tags$br()
             )
        )


        uiele_preview = tagList(
          div(
            shinydashboard::tabBox(
              width = 10,
              title = NULL,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = NS(id, "tabset_nca_optons"), # height = "250px",
              shiny::tabPanel(id=NS(id, "panel_analysis_opts"),
                       title=tagList(shiny::icon("magnifying-glass-chart"),
                                     state[["MC"]][["labels"]][["panel_analysis_opts"]]),
                tagList(
                uiele_nca_options
                # htmlOutput(NS(id, "ui_nca_ana_results_fig"))
                )
              ),
              shiny::tabPanel(id=NS(id, "panel_figure"),
                       title=tagList(shiny::icon("chart-line"),
                                     state[["MC"]][["labels"]][["panel_figures"]]),
                htmlOutput(NS(id, "ui_nca_ana_results_fig"))
              ),
              shiny::tabPanel(id=NS(id, "panel_tables"),
                       title=tagList(shiny::icon("table"),
                                     state[["MC"]][["labels"]][["panel_tables"]]),
                htmlOutput(NS(id, "ui_nca_ana_results_tab"))
              ),
              shiny::tabPanel(id=NS(id, "panel_nca_config"),
                       title=tagList(shiny::icon("gear"),
                                     state[["MC"]][["labels"]][["panel_nca_config"]]),
                htmlOutput(NS(id, "ui_nca_ana_options"))
              )
            )
          )
        )

        uiele = tagList(

          div(style="display:inline-block", htmlOutput(NS(id, "ui_nca_curr_anas"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_nca_ana_name"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_nca_curr_views"))),
          tags$br(),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_nca_ana_notes"))),
          tags$br(),
          verbatimTextOutput(NS(id, "ui_nca_msg")),
          #uiele_buttons_left,
          uiele_preview,
          uiele_buttons_right
          )


      }

    uiele})
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
             input$button_ana_run,
             input$button_ana_new,
             input$button_ana_save,
             input$button_ana_copy,
             input$button_ana_del
            )
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

        formods::FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        state_list = list(
         checksum = state[["NCA"]][["checksum"]],
         ui       = state[["NCA"]][["ui"]]
        )
        react_state[[id]][["NCA"]] = state_list
      })
    }
    #------------------------------------
    # Copying code to clipboard
    observeEvent(input$button_ana_clip, {
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      formods::FM_le(state, "clipping code")
      # This is all conditional on the whether clipr is installed $
      # and if the app isn't deployed
      if((system.file(package="clipr") != "") &
         !state[["yaml"]][["FM"]][["deployed"]]){

        current_ana = NCA_fetch_current_ana(state)

        if(is.null(current_ana[["code"]])){
          uiele = "# Run analysis to see code."
        } else {
          uiele = current_ana[["code"]]
        }

        clipr::write_clip(uiele)

      }

    })
    #------------------------------------
    # Removing holds
    remove_hold_listen  <- reactive({
      list(input$select_current_ana,
           input$select_current_view)
    })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      formods::FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["NCA"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)
    #------------------------------------

  })
}

#'@title Fetch ruminate State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id_ASM ID string for the app state management module used to save and load app states
#'@param id_UD ID string for the upload data module used to save and load app states
#'@param id_DW ID string for the data wrangling module used to save and load app states
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The list
#'has the following structure:
#' \itemize{
#' \item{yaml:} Full contents of the supplied yaml file.
#' \item{MC:} Module components of the yaml file.
#' \item{NCA:}
#' \itemize{
#'   \item{ana_cntr:}       Analysis counter.
#'   \item{anas:}                    List of analyses: Each analysis has the following  structure:
#'      \itemize{
#'        \item{ana_dsview:}       Dataset view/ID (name from DSV) selected as a data source for this analysis.
#'        \item{ana_scenario:}     Analysis scenario selected in the UI
#'        \item{checksum:}         checksum of the analysis (used to detect changes in the analysis).
#'        \item{code:}             Code to generate analysis from start to finish or error messages if code generation/analysis failed.
#'        \item{code_components:}  List containing the different components from code
#'        \item{col_conc:}         Column from ana_dsview containing the concentration data.
#'        \item{col_dose:}         Column from ana_dsview containing the dose amount.
#'        \item{col_dur:}          Column from ana_dsview containing the infusion duration or N/A if unused.
#'        \item{col_group:}        Columns from ana_dsview containing other grouping variables.
#'        \item{col_id:}           Column from ana_dsview containing the subject IDs.
#'        \item{col_ntime:}        Column from ana_dsview containing the nominal time values
#'        \item{col_route:}        Column from ana_dsview containing the dosing route.
#'        \item{col_time:}         Column from ana_dsview containing the time values.
#'        \item{id:}               Character id (\code{ana_idx}).
#'        \item{idx:}              Numeric id (\code{1}).
#'        \item{include_units:}    Boolean variable indicating in units should included in the analysis.
#'        \item{interval_range:}   Vector with the first element representing he beginning of the interval
#'                                 and the second element containing the end of the interval.
#'        \item{intervals:}        List of the intervals to include.
#'        \item{isgood:}           Current status of the analysis.
#'        \item{key:}              Analysis key acts as a title/caption (user editable)
#'        \item{msgs:}             Messages generated when checking configuration and analysis options.
#'        \item{nca_config:}       List of NCA configuration options for this analysis.
#'        \item{nca_object_name:}  Prefix for NCA objects associated with this analyis.
#'        \item{nca_parameters:}   NCA parameters selected for calculation in the UI.
#'        \item{notes:}            Analysis notes  (user editable)
#'        \item{objs:}             List of names and values for objects created with generated code.
#'        \item{sampling:}         Sampling method either "sparse" or "serial"
#'        \item{units_amt:}        Amount units.
#'        \item{units_conc:}       Concentration units.
#'        \item{units_dose:}       Dosing units.
#'        \item{units_time:}       Time units.
#'   }
# '  \item{button_counters:}       List of counters to detect button clicks.
#'   \item{current_ana:}           Currently selected analysis (list name element from anas).
#'   \item{DSV:}                   Available data source views (see \code{\link[formods]{FM_fetch_ds}})
#'   \item{checksum:}              This is an MD5 sum of the module (checksum of the analysis checksums).
#'   \item{nca_config:}            List of PKNCA configuration options for this analysis.
#'   \item{nca_parameters:}        List with two elements
#'      \itemize{
#'        \item{choices:}          List consisting of "Common Parameters" and
#'                                 "Other" (used for grouping in the UI).
#'                                 Each of these is a list of text parameter
#'                                 names with a value of the PKNCA parameter
#'                                 name.
#'        \item{summary:}          Summary table with the following columns:
#'        \itemize{
#'          \item{parameter:}      PKNCA Paramter name.
#'          \item{text:}           Name used in text output.
#'          \item{md:}             Name used markdown output.
#'          \item{latex:}          Name used in latex output.
#'          \item{description:}    Verbose textual description of the parameter.
#'        }
#'      }
#'   \item{ui:}                    Current value of form elements in the UI.
#'   \item{ui_ana_map:}            Map between UI element names and analysis in the object you get from \code{\link{NCA_fetch_current_ana}}
#'   \item{ui_ids:}                Vector of UI elements for the module.
#'   \item{ui_hold:}               List of hold elements to disable updates before a full ui referesh is complete.
#' }
#' \item{MOD_TYPE:} Character data containing the type of module \code{"NCA"}
#' \item{id:} Character data containing the module id module in the session variable.
#' \item{FM_yaml_file:} App configuration file with FM as main section.
#' \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
NCA_fetch_state = function(id, input, session, 
                           FM_yaml_file, MOD_yaml_file, 
                           id_ASM, id_UD, id_DW, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = formods::FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = NCA_init_state(FM_yaml_file, MOD_yaml_file, id, id_UD, id_DW, session)
  }

  # Detecting changes in the datasets
  # JMH Test this update in full app. Test the following:
  #  - new uploaded dataset
  #  - data view in use changes
  #  - data view in use is deleted
  UPDATE_DS = FALSE
  # The main uploaded dataset
  if("checksum" %in% names(isolate(react_state[[id_UD]][["UD"]]))){
    if(!is.null(isolate(react_state[[id_UD]][["UD"]][["checksum"]]))){
      if(is.null(state[["NCA"]][["DSV"]][["modules"]][["UD"]][[id_UD]])){
        # If the UD checksum isn't NULL but the stored value in DSV is then we
        # need to update the dataset
        UPDATE_DS = TRUE
      } else if(isolate(react_state[[id_UD]][["UD"]][["checksum"]]) !=
                state[["NCA"]][["DSV"]][["modules"]][["UD"]][[id_UD]]){
        # If the stored checksum in DSV is different than the currently
        # uploaded dataset in UD then we force a reset as well:
        UPDATE_DS = TRUE
      }
    }
  }

  # This is changes in the wrangled data views
  if("checksum" %in% names(isolate(react_state[[id_DW]][["DW"]]))){
    if(!is.null(isolate(react_state[[id_DW]][["DW"]][["checksum"]]))){
      if(is.null(state[["NCA"]][["DSV"]][["modules"]][["DW"]][[id_DW]])){
        # If the DW checksum isn't NULL but the stored value in DSV is then we
        # need to update the dataset
        UPDATE_DS = TRUE
      } else if(isolate(react_state[[id_DW]][["DW"]][["checksum"]]) !=
                state[["NCA"]][["DSV"]][["modules"]][["DW"]][[id_DW]]){
        # If the stored checksum in DSV is different than the currently
        # uploaded dataset in DW then we force a reset as well:
        UPDATE_DS = TRUE
      }
    }
  }

  if(UPDATE_DS){
    formods::FM_le(state, "Updating DS")
    # updating the "DSV" components
    if(state[["NCA"]][["isgood"]]){
      state[["NCA"]][["DSV"]] = formods::FM_fetch_ds(state, session, c(id_UD, id_DW))
    } else {
      state = NCA_init_state(FM_yaml_file,
                             MOD_yaml_file,
                             id,
                             id_UD,
                             id_DW,
                             session)
    }
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["NCA"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["NCA"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       state[["NCA"]][["ui"]][[ui_name]] = ""
     }
   }
   msgs = c()

  #---------------------------------------------
  # This will sync the analysis options in the UI to the values in the UI
  # JMH probably need to put some logic here if the analysis has changed using
  # the selector
  for(ui_name in names(state[["NCA"]][["ui_ana_map"]])){
    # We only update if there are no holds set:
    # for the current ui_name
    if(!fetch_hold(state, ui_name)){
      # Pulling out the current analysis
      current_ana = NCA_fetch_current_ana(state)

      # This prevents updating analysis elements if
      # the ui_element has not yet been created:
      SAVE_ANA_NAME = FALSE
      if(length(state[["NCA"]][["ui"]][[ui_name]])>1){
        SAVE_ANA_NAME = TRUE
      }else if(state[["NCA"]][["ui"]][[ui_name]] != "" ){
        SAVE_ANA_NAME = TRUE
      }

      if(SAVE_ANA_NAME){
        ana_name = state[["NCA"]][["ui_ana_map"]][[ui_name]]
        # Messaging detected change
        if(has_changed(ui_val  = state[["NCA"]][["ui"]][[ui_name]],
                       old_val = current_ana[[ana_name]])){
          formods::FM_le(state, paste0("setting analysis: ", ana_name, " = ", paste(state[["NCA"]][["ui"]][[ui_name]], collapse=", ")))
        }

        current_ana[[ana_name]] = state[["NCA"]][["ui"]][[ui_name]]
      }

      # Storing any changes here:
      state = NCA_set_current_ana(state, current_ana)
    }
  }
  #---------------------------------------------
  #---------------------------------------------
  # This will sync the nca options in the UI to the values in the state
  current_ana = NCA_fetch_current_ana(state)
  for(nca_opt in names(current_ana[["nca_config"]])){
    # Getting the ui_id for the current option:
    ui_name = current_ana[["nca_config"]][[nca_opt]][["ui_id"]]

    # We only update the nca option if it exists in the UI
    if(!is.null(state[["NCA"]][["ui"]][[ui_name]])){
      # We only update the nca option of there is no hold
      if(!fetch_hold(state, ui_name)){
        if(state[["NCA"]][["ui"]][[ui_name]] != ""){

          # We compare the values in the ui to the nca_config and if they are
          # different we assign them
          if(as.character(state[["NCA"]][["ui"]][[ui_name]]) !=
             as.character(current_ana[["nca_config"]][[nca_opt]][["value"]])){

             # Updating the current analysis with the ui from the state
             current_ana[["nca_config"]][[nca_opt]][["value"]] =
               state[["NCA"]][["ui"]][[ui_name]]
            formods::FM_le(state, paste0("setting NCA option: ", nca_opt, " = ", state[["NCA"]][["ui"]][[ui_name]]))
          }
        }
      }
    }
  }
  # Storing any changes here:
  state = NCA_set_current_ana(state, current_ana)
  #---------------------------------------------
  # Here we're processing any element delete requests
  # - first we only do this if the hot_nca_intervals has been defined
  if(!fetch_hold(state,"hot_nca_intervals")){
    if(is.list(state[["NCA"]][["ui"]][["hot_nca_intervals"]])){
      # - If that's the case we get the data frame for it:
      hot_df = rhandsontable::hot_to_r(state[["NCA"]][["ui"]][["hot_nca_intervals"]])
      # - Because the UI initialzes to a "no intervals" message we need
      # to make sure there is a delete column
      if("Delete" %in% names(hot_df)){
        # - lastly we check to see if any have been selected for deletion:
        if(any(hot_df$Delete == TRUE)){
          # Pulling out the current analysis
          current_ana = NCA_fetch_current_ana(state)

          # Just keeping the rows that are _not_ marked for deletion:
          current_ana[["intervals"]] = current_ana[["intervals"]][!hot_df$Delete, ]

          # If we delete the last entry we set it to NULL so it will display
          # the empty intervals message:
          if(nrow(current_ana[["intervals"]]) == 0){
            current_ana[["intervals"]] = NULL
          }

          # Storing any changes here:
          state = NCA_set_current_ana(state, current_ana)

        }
      }
    }
  }
  #---------------------------------------------
  # Process option changes to the fg_ind_obs figure
  # the current selection in the UI:
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_fg_ind_obs_save"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_fg_ind_obs_save"]])){

    formods::FM_le(state, "updating fg_ind_obs")

    # updating just the fg_ind_obs figure
    state = run_nca_components(state, "fg_ind_obs")

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_fg_ind_obs_save"]] =
      state[["NCA"]][["ui"]][["button_fg_ind_obs_save"]]
  }
  #---------------------------------------------
  # Process scenario button selection here to overwrite
  # the current selection in the UI:
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_use_scenario"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_use_scenario"]])){
    # Empty messages:
    msgs = c()


    # Current scenario:
    ana_scenario = current_ana[["ana_scenario"]]

    # Loading the scenario:
    state = NCA_load_scenario(state, ana_scenario)

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_use_scenario"]] =
      state[["NCA"]][["ui"]][["button_ana_use_scenario"]]

  }
  #---------------------------------------------
  # Process scenario button selection here to overwrite
  # the current selection in the UI:
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_add_int"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_add_int"]])){

    # Default to adding the interval
    ADD_INTERVAL = TRUE

    # Pulling the interval specifications from the ui elements:
    if(length(state[["NCA"]][["ui"]][["select_ana_interval_range"]])==2){
      interval_start = state[["NCA"]][["ui"]][["select_ana_interval_range"]][1]
      interval_stop  = state[["NCA"]][["ui"]][["select_ana_interval_range"]][2]
    } else {
      ADD_INTERVAL = FALSE
      interval_start = NA
      interval_stop  = NA
      msgs = c(msgs, "Interval range should be of lenght 2")
    }
    interval_start = as.numeric(as.character(interval_start))
    interval_stop  = as.numeric(as.character(interval_stop))

    # Pulling out the NCA parameters
    nca_parameters = state[["NCA"]][["ui"]][["select_ana_nca_parameters"]]

    # Some basic error checking
    if(is.na(interval_start)){
      ADD_INTERVAL = FALSE
      msgs = c(msgs, "Unknown interval start time. Must be a number, 0, or Inf")
    }
    if(is.na(interval_stop)){
      ADD_INTERVAL = FALSE
      msgs = c(msgs, "Unknown interval stop time. Must be a number, 0, or Inf")
    }

    if(!is.na(interval_start) & !is.na(interval_stop)){
      if(interval_start > interval_stop){
        ADD_INTERVAL = FALSE
        msgs  = c(msgs, paste0("Interval start (", interval_start, ") should be less than the interval end (", interval_stop, ")"))
      }
    }

    if(length(nca_parameters) == 1){
      if(nca_parameters ==""){
        ADD_INTERVAL = FALSE
        msgs = c(msgs, "You must select at least one NCA parameter per interval.")
      }
    }

    # If everything is good up top we add the interval
    if(ADD_INTERVAL){
      state = NCA_add_int(state=state,
        interval_start = interval_start,
        interval_stop  = interval_stop,
        nca_parameters = nca_parameters)

      details = paste0("[", interval_start, ", ",
                            interval_stop, "] ",
                            paste0(nca_parameters, collapse=", "))

      # Adding a notification
      notify_text = state[["MC"]][["notifications"]][["ana_add_int_success"]]
      notify_text = stringr::str_replace(notify_text, "===DETAILS===", details)
      notify_type = "success"
      formods::FM_le(state, notify_text)
      state = FM_set_notification(state, notify_text, "Interval Added", "success")
    } else {
      notify_text = paste(msgs, collapse="\n")
      notify_type = "failure"
      state = FM_set_notification(state, notify_text, "Interval Not Added", "failure")
      formods::FM_le(state, "interval was not added")
    }

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_add_int"]] =
      state[["NCA"]][["ui"]][["button_ana_add_int"]]

    # Updating any messages
    state = formods::FM_set_ui_msg(state, msgs)
  }
  #---------------------------------------------
  # Run Analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_run"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_run"]])){

    formods::FM_le(state, "running nca and generating subsequent figures and tables")

    # Pausing access to the screen
    formods:: FM_pause_screen(
      state   = state,
      message = state[["MC"]][["labels"]][["busy"]][["run_nca"]],
      session = session)

    # This will build the code and run the different components:
    state = run_nca_components(state)

    # Removing the pause
    FM_resume_screen(state   = state,
                     session = session)

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_run"]] =
      state[["NCA"]][["ui"]][["button_ana_run"]]
  }
  #---------------------------------------------
  # Here we react to changes between the UI and the current state
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["select_current_ana"]],
                 old_val  = state[["NCA"]][["current_ana"]]) &
      (!fetch_hold(state, "select_current_ana"))){

    # Changing the current view to the one selected in the UI
    # JMH create NCA_mkactive_ana here to set active
    state[["NCA"]][["current_ana"]]  =  state[["NCA"]][["ui"]][["select_current_ana"]]
  }
  #---------------------------------------------
  # Copy Analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_copy"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_copy"]])){

    formods::FM_le(state, "copying current analysis")

    # Pausing access to the screen because the rebuilding portion below can
    # take a while and we don't want the user mucking around with things while
    # that is happening.
    formods::FM_pause_screen(
      state   = state,
      message = state[["MC"]][["labels"]][["busy"]][["run_nca"]],
      session = session)

    # This creates a copy of the current analysis which will become the old
    # one :).
    old_ana = NCA_fetch_current_ana(state)

    # Creating a new analysis and pulling it out of the
    # state object:
    state = NCA_new_ana(state)
    current_ana = NCA_fetch_current_ana(state)


    # Now we take the current (new one we just created) analysis and
    # we populate it with all the individual elements except
    # the following:
    exnames    = c("id", "key", "idx", "nca_object_name", "objs")
    # This is all the names
    copy_names = names(old_ana)
    # And this removes the exclusions:
    copy_names = copy_names[!(copy_names %in% exnames)]

    for(ana_name in copy_names){
      current_ana[[ana_name]] = old_ana[[ana_name]]
    }

    # Now we save the copy of the new analysis that has been updated with
    # the elements of the old analysis. This has to be done before we rerun
    # the components below:
    state = NCA_set_current_ana(state, current_ana)

    # Lastly we need to rebuild the analysis elements so all the id in
    # the generated code are correct. It's a little slow but it ensures the
    # objs list element is correct
    state = run_nca_components(state)

    # Setting hold for all the elements
    state = set_hold(state, inputId = NULL)

    # Removing the pause
    FM_resume_screen(state   = state,
                     session = session)

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_copy"]] =
      state[["NCA"]][["ui"]][["button_ana_copy"]]

  }
  #---------------------------------------------
  # New Analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_new"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_new"]])){

    formods::FM_le(state, "creating new analysis")
    msgs = c()

    # Creating a new analysis
    state = NCA_new_ana(state)

    # Setting hold for all the elements
    state = set_hold(state, inputId = NULL)
    #state = set_hold(state, inputId = "select_current_ana")
    #state = set_hold(state, inputId = "select_current_view")

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_new"]] =
      state[["NCA"]][["ui"]][["button_ana_new"]]

    # Updating any messages
    state = formods::FM_set_ui_msg(state, msgs)
  }
  #---------------------------------------------
  # Delete analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_del"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_del"]])){

    formods::FM_le(state, "deleting analysis")
    msgs = c()

    # Getting the current analysis
    current_ana = NCA_fetch_current_ana(state)

    # Deleting the analysis
    state[["NCA"]][["anas"]][[current_ana[["id"]]]] = NULL

    # If there are no analysis left then we create an empty one
    if( length(state[["NCA"]][["anas"]])  == 0){
      state = NCA_new_ana(state)
    } else {
      # If there are analysis then we set the first one as active
      state[["NCA"]][["current_ana"]] = names(state[["NCA"]][["anas"]])[1]
    }

    # Setting hold for analysis select
    state = set_hold(state, inputId = "select_current_ana")
    state = set_hold(state, inputId = "select_current_view")

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_del"]] =
      state[["NCA"]][["ui"]][["button_ana_del"]]

    # Updating any messages
    state = formods::FM_set_ui_msg(state, msgs)
  }
  #---------------------------------------------
  # Save analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_save"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_save"]])){

    formods::FM_le(state, "saving changes to current analysis")

    # Getting the current analysis
    current_ana = NCA_fetch_current_ana(state)

    if(state[["NCA"]][["ui"]][["text_ana_key"]] != ""){
      # Resetting the key
      current_ana[["key"]] = state[["NCA"]][["ui"]][["text_ana_key"]]
    } else {
      # returning an error
      msgs = c(msgs,
          state[["MC"]][["errors"]][["current_key_empty"]])
    }

    # Saving the caption as well
    current_ana[["notes"]] = state[["NCA"]][["ui"]][["text_ana_notes"]]

    # updating the view id
    current_ana[["ana_dsview"]] = state[["NCA"]][["ui"]][["select_current_view"]]

    # Saving changes to the current analysis
    state = NCA_set_current_ana(state, current_ana)

    notify_text = paste(
           tagList(paste0("Caption: ", current_ana[["key"]], ", " ),
             paste0("Source Data: ", state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]][["label"]], ", "),
             paste0("Notes: ", current_ana[["notes"]]  )),
                        collapse="\n")


    state = FM_set_notification(state, notify_text, "Interval Added", "info")


    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_save"]] =
      state[["NCA"]][["ui"]][["button_ana_save"]]

    # Updating any messages
    state = formods::FM_set_ui_msg(state, msgs)

  }
  #---------------------------------------------
  # Passing any messages back to the user
  #state = formods::FM_set_ui_msg(state, msgs, append=TRUE)

  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@title Initialize NCA Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@return list containing an empty NCA state
NCA_init_state = function(FM_yaml_file, MOD_yaml_file,  id, id_UD, id_DW,  session){

  button_counters = c("button_ana_new",
                      "button_ana_del",
                      "button_ana_run",
                      "button_ana_save",
                      "button_ana_copy",
                      "button_ana_add_int",
                      "button_ana_use_scenario",
                      "button_fg_ind_obs_save",
                      "switch_ana_include_units")

  # mapping name in UI to name used in the analysis
  ui_ana_map = list(
    "switch_ana_include_units"       = "include_units",
    "select_ana_interval_range"      = "interval_range",
    "select_fg_ind_obs_ncol"         = "fg_ind_obs_ncol",
    "select_fg_ind_obs_nrow"         = "fg_ind_obs_nrow",
    "select_fg_ind_obs_page"         = "curr_fg_ind_obs",
    "select_tb_ind_obs_page"         = "curr_tb_ind_obs",
    "select_tb_ind_params_page"      = "curr_tb_ind_params",
    "select_tb_sum_params_page"      = "curr_tb_sum_params",
    "fg_ind_obs_rpt"                 = "fg_ind_obs_rpt",
    "tb_ind_obs_rpt"                 = "tb_ind_obs_rpt",
    "tb_ind_params_rpt"              = "tb_ind_params_rpt",
    "tb_sum_params_rpt"              = "tb_sum_params_rpt",
    "check_fg_ind_obs_logy"          = "fg_ind_obs_logy",
    "switch_ana_source_sampling"     = "sampling",
    "switch_ana_fig"                 = "fig_type",
    "switch_ana_tab"                 = "tab_type",
    "text_ana_key"                   = "key",
    "select_ana_nca_parameters"      = "nca_parameters",
    "select_ana_scenario"            = "ana_scenario",
    "select_ana_units_time"          = "units_time",
    "select_ana_units_conc"          = "units_conc",
    "select_ana_units_dose"          = "units_dose",
    "select_ana_units_amt"           = "units_amt",
    "select_ana_col_id"              = "col_id"   ,
    "select_ana_col_time"            = "col_time" ,
    "select_ana_col_ntime"           = "col_ntime" ,
    "select_ana_col_dose"            = "col_dose" ,
    "select_ana_col_dur"             = "col_dur" ,
    "select_ana_col_conc"            = "col_conc" ,
    "select_ana_col_route"           = "col_route" ,
    "select_ana_col_group"           = "col_group",
    "select_ana_col_analyte"         = "col_analyte",
    "select_ana_fig_view"            = "fig_view",
    "select_ana_tab_view"            = "tab_view"
  )

  # We add all of the button counters as well as
  # the ui inputs specified in ui_ana_map above.
  # Then we append the other ui elements we want to
  # use in the state:
  ui_ids          = c(button_counters,
                      names(ui_ana_map),
                      "hot_nca_intervals",
                      "select_current_ana",
                      "select_current_view",
                      "text_ana_notes")


  ui_hold         = c()

  isgood          = TRUE

  state = formods::FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    dep_mod_ids     = c(id_UD, id_DW),
    MT              = "NCA",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  # These will be the default options
  nca_config = state[["MC"]][["nca_config"]]

  # This table summarizes the different options
  nc_summary = NULL
  for(nca_opt in  names(nca_config)){
    nca_config_ui_id =  paste0("nca_opt_",nca_opt)
    # Storing the ui_id for the option in the list
    nca_config[[nca_opt]][["ui_id"]] = nca_config_ui_id
    nc_summary =
    rbind(nc_summary,
          data.frame(key          = nca_opt,
                     ui_id        = nca_config_ui_id,
                     group        = state[["MC"]][["nca_config"]][[nca_opt]][["group"]],
                     label        = state[["MC"]][["nca_config"]][[nca_opt]][["label"]],
                     pknca_option = state[["MC"]][["nca_config"]][[nca_opt]][["pknca_option"]])
          )
  }

  # This table summarizes the NCA parameters
  np_summary = NULL
  # This list contains the NCA parameter
  # choices in the selection menu
  np_choices = list()

  # Default descriptors from PKNCA:
  PKNCA_def = PKNCA::get.interval.cols()

  for(nca_param in names(state[["MC"]][["nca_parameters"]])){
    np_comps = state[["MC"]][["nca_parameters"]][[nca_param]]

    # This should catch any typos in the YAML file:
    if(nca_param %in% names(PKNCA_def)){
      # Null values for description and text indicate that we want
      # to use the default values from PKNCA
      if(is.null(np_comps[["description"]])){
        np_comps[["description"]] = PKNCA_def[[nca_param]][["desc"]]
      }
      if(is.null(np_comps[["text"]])){
        np_comps[["text"]] = PKNCA_def[[nca_param]][["pretty_name"]]
      }


      # Updating the choices list
      np_choices[[ np_comps[["group"]] ]][[ np_comps[["text"]] ]] = nca_param

      # Updating the summary table
      np_summary =
        rbind(np_summary,
          data.frame(
            parameter   = nca_param,
            text        = np_comps[["text"]],
            md          = np_comps[["md"]],
            latex       = PKNCA_def[[nca_param]][["pretty_name"]],
            description =  np_comps[["description"]])
         )
    } else {
      cli::cli_alert_danger(paste0("NCA: Parameter specified in YAML is not a valid PKNCA parameter: ", nca_param))
    }
  }

  # IDs for NCA options start with nca_opt_ (see data.frame command above)
  # We add those to the list of the  ui_ids:
  ui_ids   = c(ui_ids, nc_summary[["ui_id"]])

# ui_hold         = c(nc_summary[["ui_id"]],
#                     "select_current_ana",
#                     "hot_nca_intervals",
#                     "select_current_view")
#

  # Adding all of the ui_ids to the list of ui_hold
  ui_hold = ui_ids

  # Now we recreate the initialized state with all of the ui_ids
  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
    dep_mod_ids     = c(id_UD, id_DW),
    MT              = "NCA",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  # This table summarizes the options
  state[["NCA"]][["nca_config"]][["summary"]] = nc_summary
  state[["NCA"]][["nca_config"]][["default"]] = nca_config

  # This summarizes the parameters
  state[["NCA"]][["nca_parameters"]][["summary"]]  = np_summary
  state[["NCA"]][["nca_parameters"]][["choices"]]  = np_choices

  # Storing the analysis map
  state[["NCA"]][["ui_ana_map"]]              = ui_ana_map

  # Creating an empty docx report. It has the style information
  # needed for building tables
  init_cmd = state[["yaml"]][["FM"]][["reporting"]][["content_init"]][["docx"]]
  tcres = formods::FM_tc(cmd = init_cmd, tc_env = list(), capture="rpt")
  state[["NCA"]][["docx_rpt"]] = tcres[["capture"]][["rpt"]]

  # Finding the dataset
  DSV = formods::FM_fetch_ds(state, session, c(id_UD, id_DW))

  # If the dataset isn't good then we need to
  # flag the whole module as not being good
  if(!DSV[["isgood"]]){
    isgood = FALSE
  }

  # Module-specific elements
  state[["NCA"]][["isgood"]]        = isgood
  state[["NCA"]][["DSV"]]           = DSV
  state[["NCA"]][["anas"]]          = NULL
  state[["NCA"]][["ana_cntr"]]      = 0
  state[["NCA"]][["current_ana"]]   = NULL

  formods::FM_le(state, "State initialized")

  if(isgood){
    # Initializing an empty analysis
    state = NCA_new_ana(state)
  }


  formods::FM_le(state, "State initialized")
state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state NCA state from \code{NCA_fetch_state()}
#'@return Character object vector with the lines of code
NCA_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Append Report Elements
#'@description Takes an NCA state object and appends any reportable elements
#'for the specified report type. On NCA analyses that are in a "good" state
#'will be reported. Those not in a good state will be ignored. 
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link[formods]{FM_generate_report}}.
#'@param rpttype Type of report to generate (supported "xlsx", "pptx", "docx").
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasrptele:} Boolean indicator if the module has any reportable elements.
#'  \item{code:}      Code to create report elements. 
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{rpt:}       Report with any additions passed back to the user.
#'}
#'@seealso \code{\link[formods]{FM_generate_report}}
#'@examples
#'
#'# We need a state object to use below
#'sess_res = NCA_test_mksession(session=list())
#'state = sess_res$state
#'
#'rpt = list(summary = list(), sheets=list())
#'
#'rpt_res = NCA_append_report(state,
#'  rpt     = rpt,
#'  rpttype = "xlsx")
#'
#'# Shows if report elements are present
#'rpt_res$hasrptele
#'
#'# Code chunk to generate report element
#'cat(paste(rpt_res$code, collapse="\n"))
#'
#'# Tabular summary of data views
#'rpt_res$rpt$summary
#'@seealso \code{\link{FM_generate_report}}
#'@examples
#' # We need a state object to use below
#' sess_res = NCA_test_mksession(session=list())
#' state = sess_res$state
#' 
#' # here we need an empty report object for tabular data
#' rpt = list(summary = list(), sheets=list())
#' 
#' # Now we append the report indicating we want 
#' # Excel output:
#' rpt_res = NCA_append_report(state,
#'   rpt     = rpt,
#'   rpttype = "xlsx")
#' 
#' # Shows if report elements are present
#' rpt_res$hasrptele
#' 
#' # Code chunk to generate report element
#' cat(paste(rpt_res$code, collapse="\n"))
#' 
#' # Tabular results from the first NCA analysis
#' rpt_res$rpt$sheets$NCA_1_RES
NCA_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The NCA module supports the following report types:
  supported_rpttypes = c("xlsx", "pptx", "docx")

  if(rpttype %in% supported_rpttypes){
    for(ana in names(state[["NCA"]][["anas"]])){
      # Only the analyses that are in a good state are reported
      tmp_ana = state[["NCA"]][["anas"]][[ana]] 
      if(tmp_ana[["isgood"]]){
        # We're creating a list for to pass into the try/catch enviornment.
        # This will create all of the NCA-related objects so that they can be
        # used for reporting.
        tc_env = list()
        for(obj_key in names(tmp_ana[["objs"]])){
          tc_env[[ tmp_ana[["objs"]][[obj_key]][["name"]] ]] = 
                   tmp_ana[["objs"]][[obj_key]][["value"]]
        }
          
        # Here we process the analyses based on the report type selected:
        #----------------------------------------------------------
        # Excel
        if(rpttype == "xlsx"){
          hasrptele = TRUE
          #----------------------------------------------------------
          # Table of final PKNCA results
          ind_sheet_name = paste0(tmp_ana[["id"]], "_RES")
        
          # This appends the data frame to the report list
          code_chunk = paste0('rpt[["sheets"]][["',
                              ind_sheet_name,
                              '"]]=',
                              tmp_ana[["objs"]][["tb_ind_params"]][["name"]],
                              '[["one_table"]]')
          # Evaluating the code
          if(!gen_code_only){
            # We have to make sure the try/catch environment is using the
            # latest rpt object, so before we cal FM_tc, we need to update the
            # rpt object. This is done below for each FM_tc call:
            tc_env[["rpt"]] = rpt
            tc_res = formods::FM_tc(capture="rpt", cmd=code_chunk, tc_env = tc_env)
            if(tc_res[["isgood"]]){
              rpt = tc_res[["capture"]][["rpt"]]
            } else {
              formods::FM_le(state, "Failed to add report element: ")
              formods::FM_le(state, paste0("  Analysis: ", ana))
              formods::FM_le(state, paste0("  Table:    ", ind_sheet_name))
              if(!is.null(tc_res[["msgs"]])){
                formods::FM_le(state, tc_res[["msgs"]])
              }
            }
          }

          # Appending to returned code
          code = c(code, code_chunk)
        
          # Appends the mapping between sheet name and description:
          code_chunk = c('rpt[["summary"]] = rbind(rpt[["summary"]],',
                         "  data.frame(",
                  paste0('    Sheet_Name="',  ind_sheet_name, '",'),
                  paste0('    Description="', tmp_ana[["key"]], ' (indiviudal results)"'),
                         "  )",
                         ')')
          # Evaluating the code
          if(!gen_code_only){
            tc_env[["rpt"]] = rpt
            tc_res = formods::FM_tc(capture="rpt", cmd=code_chunk, tc_env = tc_env)
            if(tc_res[["isgood"]]){
              rpt = tc_res[["capture"]][["rpt"]]
            } else {
              formods::FM_le(state, "Failed to add report element: ")
              formods::FM_le(state, paste0("  Analysis: ", ana))
              formods::FM_le(state, paste0("  Table:    ", ind_sheet_name))
              if(!is.null(tc_res[["msgs"]])){
                formods::FM_le(state, tc_res[["msgs"]])
              }
            }
          }
          # Appending to returned code
          code = c(code, code_chunk)
        
          #----------------------------------------------------------
          # Table of raw PKNCA results
          raw_sheet_name = paste0(tmp_ana[["id"]], "_RAW")
        
          # This appends the data frame to the report list
          code_chunk = paste0('rpt[["sheets"]][["',
                              raw_sheet_name,
                              '"]]=as.data.frame(',
                              tmp_ana[["objs"]][["res"]][["name"]],
                              ")")
          # Evaluating the code
          if(!gen_code_only){
            tc_env[["rpt"]] = rpt
            tc_res = formods::FM_tc(capture="rpt", cmd=code_chunk, tc_env = tc_env)
            if(tc_res[["isgood"]]){
              rpt = tc_res[["capture"]][["rpt"]]
            } else {
              formods::FM_le(state, "Failed to add report element: ")
              formods::FM_le(state, paste0("  Analysis: ", ana))
              formods::FM_le(state, paste0("  Table:    ", ind_sheet_name))
              if(!is.null(tc_res[["msgs"]])){
                formods::FM_le(state, tc_res[["msgs"]])
              }
            }
          }
          # Appending to returned code
          code = c(code, code_chunk)
        
          # Appends the mapping between sheet name and description:
          code_chunk = c('rpt[["summary"]] = rbind(rpt[["summary"]],',
                         "  data.frame(",
                  paste0('    Sheet_Name="',  raw_sheet_name, '",'),
                  paste0('    Description="', tmp_ana[["key"]], ' (raw NCA results)"'),
                         "  )",
                         ')')
          # Evaluating the code
          if(!gen_code_only){
            tc_env[["rpt"]] = rpt
            tc_res = formods::FM_tc(capture="rpt", cmd=code_chunk, tc_env = tc_env)
            if(tc_res[["isgood"]]){
              rpt = tc_res[["capture"]][["rpt"]]
            } else {
              formods::FM_le(state, "Failed to add report element: ")
              formods::FM_le(state, paste0("  Analysis: ", ana))
              formods::FM_le(state, paste0("  Table:    ", ind_sheet_name))
              if(!is.null(tc_res[["msgs"]])){
                formods::FM_le(state, tc_res[["msgs"]])
              }
            }
          }
          # Appending to returned code
          code = c(code, code_chunk)
          #----------------------------------------------------------
        }
        #----------------------------------------------------------
        # Word or PowerPoint
        if(rpttype %in% c("docx", "pptx")){
          #----------------------------------------------------------
          # Reporting figures:
          for(fid in names(state[["MC"]][["reporting"]][["figures"]])){
            # This is the list name contiaing the selected report outputs for
            # this figure
            fid_rpt = paste0(fid, "_rpt")
         
            if(rpttype %in% tmp_ana[[fid_rpt]]){
        
              # This is the key that can be used in referencing the figure in
              # Word
              fig_id      = paste0("fig_", tmp_ana[["id"]], "_", fid)
        
              figisgood = TRUE
              hasrptele = TRUE
              figmsgs   = c()
              if(fid %in% names(tmp_ana[["objs"]])){
                if(!is.null(names(tmp_ana[["objs"]][[fid]][["value"]][["figures"]]))){
        
                  # This is the pages of figures associated with the current
                  # figure:
                  tmp_figs = tmp_ana[["objs"]][[fid]][["value"]][["figures"]]
                  notes_str = NULL
        
                  # This is the counter used for creating captions
                  fig_idx = 1
                  for(tmp_fig in names(tmp_figs)){
        
                    # This is a character string that references the current
                    # ggplot object.
                    fg_object_name = paste0(tmp_ana[["objs"]][[fid]][["name"]],
                                    '[["figures"]]',
                                    '[["', tmp_fig, '"]][["gg"]]')
                    
                    # Here we create the code chunk for either a powerpoint
                    # or docx whichever the report type
                    code_chunk = NULL
                    if(rpttype == "pptx"){
                       # Here we construct the title for the figure based on the
                       # specification in the configuration file:
                       if(length(names(tmp_figs)) > 1){
                         fig_title      = state[["MC"]][["reporting"]][["figures"]][[fid]][["title_multiple"]]
                         fig_sub_title   = state[["MC"]][["reporting"]][["figures"]][[fid]][["sub_title_multiple"]]
                       } else {
                         fig_title      = state[["MC"]][["reporting"]][["figures"]][[fid]][["title_single"]]
                         fig_sub_title   = state[["MC"]][["reporting"]][["figures"]][[fid]][["sub_title_single"]]
                       }
                       
                      if(is.null(fig_title)){
                        title_code_chunk = NULL
                      }else{
                        fig_title    = stringr::str_replace_all(fig_title, "===NCADESC===",  tmp_ana[["key"]])
                        fig_title    = stringr::str_replace_all(fig_title, "===FIGNUM===",   as.character(fig_idx))
                        fig_title    = stringr::str_replace_all(fig_title, "===FIGTOT===",   as.character(length(names(tmp_figs))))
                        title_code_chunk = c(
                          paste0('            title        = list( content = "', fig_title,'",'),
                                 '                                 type    = "text"),                ')
                      }
                      if(is.null(fig_sub_title)){
                        sub_title_code_chunk = NULL
                      }else{
                        sub_title_code_chunk = NULL
                        fig_sub_title = stringr::str_replace_all(fig_sub_title, "===NCADESC===",  tmp_ana[["key"]])
                        fig_sub_title = stringr::str_replace_all(fig_sub_title, "===FIGNUM===",   as.character(fig_idx))
                        fig_sub_title = stringr::str_replace_all(fig_sub_title, "===FIGTOT===",   as.character(length(names(tmp_figs))))
                        sub_title_code_chunk = c(
                           paste0('            sub_title    = list( content = "', fig_sub_title,'",'),
                                  '                                 type    = "text"),                ')
                      }
        
                      code_chunk = c(
                      paste0('# Inserting figure: ', fig_id),
                             'rpt  = onbrand::report_add_slide(rpt,                              ',
                             '          template = "content_list",                               ',
                             '          elements = list(                                         ',
                             title_code_chunk,
                             sub_title_code_chunk,
                      paste0('            content_body = list( content = ', fg_object_name, ',   '),
                             '                                 type    = "ggplot")))               '
                      )
                    } 
                    if(rpttype == "docx"){
                       # Here we construct the caption for the figure based on the
                       # specification in the configuration file:
                       if(length(names(tmp_figs)) > 1){
                         fig_caption = state[["MC"]][["reporting"]][["figures"]][[fid]][["caption_multiple"]]
                       } else {
                         fig_caption = state[["MC"]][["reporting"]][["figures"]][[fid]][["caption_single"]]
                       }
                       
                       fig_caption = stringr::str_replace_all(fig_caption, "===NCADESC===",  tmp_ana[["key"]])
                       fig_caption = stringr::str_replace_all(fig_caption, "===FIGNUM===",   as.character(fig_idx))
                       fig_caption = stringr::str_replace_all(fig_caption, "===FIGTOT===",   as.character(length(names(tmp_figs))))
        
                       code_chunk = c(
                       paste0('# Inserting figure: ', fig_id),
                              'rpt = onbrand::report_add_doc_content(rpt,',
                              '        type     = "ggplot",',
                              '        content  = list(',
                                         notes_str,
                       paste0('          image           =  ', fg_object_name, ','),
                       paste0('          key             = "', fig_id,  '",'),
                       paste0('          caption_format  = "',state[["MC"]][["reporting"]][["text_format"]],'",'),
                       paste0('          caption         = "',fig_caption,'"))'),
                              '# adding a page break',
                              'rpt = onbrand::report_add_doc_content(rpt,',
                              '        type    = "break",',
                              '        content = NULL)',
                              ' '
                       )
                    }
        
                    # Now we evaluate the code chunk
                    if(!is.null(code_chunk)){
                      # Evaluating the code
                      if(!gen_code_only){
                        tc_env[["rpt"]] = rpt
                        tc_res = formods::FM_tc(capture="rpt", cmd=code_chunk, tc_env = tc_env)
                        if(tc_res[["isgood"]]){
                          fig_idx = fig_idx + 1 
                          rpt     = tc_res[["capture"]][["rpt"]]
                        } else {
                          formods::FM_le(state, "Failed to add report element: ")
                          formods::FM_le(state, paste0("  Analysis: ", ana))
                          formods::FM_le(state, paste0("  fig_id:   ", fig_id))
                          formods::FM_le(state, paste0("  figure:   ", tmp_fig))
                          if(!is.null(tc_res[["msgs"]])){
                            formods::FM_le(state, tc_res[["msgs"]])
                          }
                        }
                      }
                      # Appending to returned code
                      code = c(code, code_chunk)
                     }
                   }
                 } else {
                   figisgood = FALSE
                   figmsgs = c(figmsgs, paste0("  Figure id (", fid,") has no figures."))
                 }
              } else {
                figisgood = FALSE
                figmsgs = c(figmsgs, paste0("  Figure id (", fid,") was not found."))
              } 

              if(!figisgood){
                formods::FM_le(state, "Failed to add report element: ")
                formods::FM_le(state, paste0("  Analysis: ", ana))
                formods::FM_le(state, paste0("  Figure:   ", fid_rpt))
                formods::FM_le(state, figmsgs)
              }
            }
          }
          #----------------------------------------------------------
          # Reporting tables:
          for(tid in names(state[["MC"]][["reporting"]][["tables"]])){
            # This is the list name contiaing the selected report outputs for
            # this table  
            tid_rpt = paste0(tid, "_rpt")
         
            if(rpttype %in% tmp_ana[[tid_rpt]]){
              # This is the key that can be used in referencing the table in
              # Word
              tab_id      = paste0("tab_", tmp_ana[["id"]], "_", tid)
        
              tabisgood = TRUE
              hasrptele = TRUE
              tabmsgs   = c()

              if(tid %in% names(tmp_ana[["objs"]])){
                if(!is.null(names(tmp_ana[["objs"]][[tid]][["value"]][["tables"]]))){
                  # This is the pages of tables  associated with the current
                  # table:  
                  tmp_tabs = tmp_ana[["objs"]][[tid]][["value"]][["tables"]]
                  # This is the counter used for creating captions
                  tab_idx = 1
                  for(tmp_tab in names(tmp_tabs)){
                    notes_str = NULL
                    # This is a character string that references the current
                    # ggplot object.
                    tb_object_name = paste0(tmp_ana[["objs"]][[tid]][["name"]],
                                    '[["tables"]]',
                                    '[["', tmp_tab, '"]][["ft"]]')

                    # These are the notes for the current table
                    tb_notes = tmp_ana[["objs"]][[tid]][["value"]][["tables"]][[tmp_tab]][["notes"]]
                    if(!is.null(tb_notes)){
                      tb_notes_full = c()
                      for(tb_note in tb_notes){
                        tb_note_full = NULL
                        if(tb_note %in% names(state[["MC"]][["reporting"]][["notes"]])){
                          tb_note_full = state[["MC"]][["reporting"]][["notes"]][[tb_note]]
                        }
                        # If we didn't find the full note we flag that
                        # here. This probably shouldn't happen but it's not
                        # important enough to stop everything.
                        if(is.null(tb_note_full)){
                          formods::FM_le(state, "Note not added. Note string: ", tb_note)
                          formods::FM_le(state, paste0("  Analysis: ", ana))
                          formods::FM_le(state, paste0("  tab_id:   ", tab_id))
                          formods::FM_le(state, paste0("  table:    ", tmp_tab))
                        } else {
                          tb_notes_full = c(tb_notes_full, paste0(tb_note, ": ", tb_note_full))
                        }
                      }
                      # If we made it this far and we've found some
                      # notes then we create the actual notes. 
                      if(!is.null(tb_notes_full)){
                        notes_str = c(
                          paste0('          notes           =  "', paste0(tb_notes_full, collapse=", "),'",'),
                          paste0('          notes_format    = "',state[["MC"]][["reporting"]][["text_format"]],'",')
                        )
                      }
                    }


                    # Here we create the code chunk for either a powerpoint
                    # or docx whichever the report type
                    code_chunk = NULL
                    if(rpttype == "pptx"){
                    } 
                    if(rpttype == "docx"){
                       # Here we construct the caption for the table based on the
                       # specification in the configuration file:
                       if(length(names(tmp_tabs)) > 1){
                         tab_caption = state[["MC"]][["reporting"]][["tables"]][[tid]][["caption_multiple"]]
                       } else {
                         tab_caption = state[["MC"]][["reporting"]][["tables"]][[tid]][["caption_single"]]
                       }
                       
                      tab_caption = stringr::str_replace_all(tab_caption, "===NCADESC===",  tmp_ana[["key"]])
                      tab_caption = stringr::str_replace_all(tab_caption, "===TABNUM===",   as.character(tab_idx))
                      tab_caption = stringr::str_replace_all(tab_caption, "===TABTOT===",   as.character(length(names(tmp_tabs))))

                      code_chunk = c(
                      paste0('# Inserting table: ', fig_id),
                             'rpt = onbrand::report_add_doc_content(rpt,',
                             '        type     = "flextable_object",',
                             '        content  = list(',
                                        notes_str,
                      paste0('          ft              =  ', tb_object_name, ','),
                      paste0('          key             = "', tab_id,  '",'),
                      paste0('          caption_format  = "',state[["MC"]][["reporting"]][["text_format"]],'",'),
                      paste0('          caption         = "',tab_caption,'"))'),
                             '# adding a page break',
                             'rpt = onbrand::report_add_doc_content(rpt,',
                             '        type    = "break",',
                             '        content = NULL)',
                             ' '
                      )
                    } 

                    # JMH
                    #browser()
                    # Now we evaluate the code chunk
                    if(!is.null(code_chunk)){
                      # Evaluating the code
                      if(!gen_code_only){
                        tc_env[["rpt"]] = rpt
                        tc_res = formods::FM_tc(capture="rpt", cmd=code_chunk, tc_env = tc_env)
                        if(tc_res[["isgood"]]){
                          tab_idx = tab_idx + 1 
                          rpt     = tc_res[["capture"]][["rpt"]]
                        } else {
                          formods::FM_le(state, "Failed to add report element: ")
                          formods::FM_le(state, paste0("  Analysis: ", ana))
                          formods::FM_le(state, paste0("  tab_id:   ", tab_id))
                          formods::FM_le(state, paste0("  Table:    ", tmp_tab))
                          if(!is.null(tc_res[["msgs"]])){
                            formods::FM_le(state, tc_res[["msgs"]])
                          }
                        }
                      }
                      # Appending to returned code
                      code = c(code, code_chunk)
                     }


                   }
                 } else {
                   tabisgood = FALSE
                   tabmsgs = c(tabmsgs, paste0("  Table id (", tid,") has no tables."))
                 }
              } else {
                tabisgood = FALSE
                tabmsgs = c(tabmsgs, paste0("  Table id (", tid,") was not found."))
              } 
              if(!tabisgood){
                formods::FM_le(state, "Failed to add report element: ")
                formods::FM_le(state, paste0("  Analysis: ", ana))
                formods::FM_le(state, paste0("  Table:    ", tid_rpt))
                formods::FM_le(state, tabmsgs)
              }
            }
          }
        }
      }
    }
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
#'@title Fetch Module Datasets
#'@description Fetches the datasets contained in the module
#'@param state NCA state from \code{NCA_fetch_state()}
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
#' # We need a state object to use below
#' sess_res = NCA_test_mksession(session=list())
#' state = sess_res$state
#'
#' myDs = NCA_fetch_ds(state)
NCA_fetch_ds = function(state){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = "NCA",
               id         = state[["ID"]],
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = NULL,
               DSchecksum = NULL)

  if(length(names(state[["NCA"]][["anas"]]))>0){
    for(ana_name in names(state[["NCA"]][["anas"]])){
      # This shoudl contain the current analys, and we only 
      # process it if it's in a good state.
      curr_ana = state[["NCA"]][["anas"]][[ana_name]]
      if(curr_ana[["isgood"]]){
        # Flipping the hasds switch:
        hasds = TRUE

        #-----------------------------------------------------------
        # Adding the raw NCA data
        tb_name  = paste0(curr_ana[["id"]], "_raw")
        tb_label = paste0(curr_ana[["key"]], " (Raw)")

        # This will add only columns that are present in the dataset
        # to the metadata
        DSMETA = list()
        for(cname in names(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["raw_nca"]])){
          if(cname %in% names(state[["MC"]][["labels"]][["PKNCA_raw"]])){
            DSMETA[[cname]] = state[["MC"]][["labels"]][["PKNCA_raw"]][[cname]]
          } else {
            DSMETA[[cname]] = "Not defined"
            formods::FM_le(state, paste0("No metadata for PKNCA_raw: ", cname))
          }
        }

        # Creating an emtpy dataset
        ds[[tb_name]]                  = NEWDS
        ds[[tb_name]][["label"]]       = tb_label
        ds[[tb_name]][["DS"]]          = curr_ana[["objs"]][["tb_ind_params"]][["value"]][["raw_nca"]]
        ds[[tb_name]][["DSMETA"]]      = DSMETA
        ds[[tb_name]][["code"]]        = curr_ana[["code"]]
        ds[[tb_name]][["checksum"]]    = state[["NCA"]][["checksum"]]
        ds[[tb_name]][["DSchecksum"]]  = digest::digest(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["raw_nca"]], 
                                                        algo=c("md5"))
        #-----------------------------------------------------------
        # Adding the indiviudal parameters
        tb_name  = paste0(curr_ana[["id"]], "_ind")
        tb_label = paste0(curr_ana[["key"]], " (Individual Parameters)")

        # This will add only columns that are present in the dataset
        # to the metadata
        DSMETA = list()
        if(is.null(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["one_header"]])){
          formods::FM_le(state, paste0("No headers individual NCA parameter header: ", tb_name,":", tb_label))
        } else {
          for(cname in names(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["one_table"]])){
            if(cname %in% names(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["one_header"]])){
              DSMETA[[cname]] = paste(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["one_header"]][[cname]], collapse=" ")
            } else {
              DSMETA[[cname]] = "Not defined"
              formods::FM_le(state, paste0("No metadata for individual NCA parameter header: ", cname))
            }
          }
        }
        # Creating an emtpy dataset
        ds[[tb_name]]                  = NEWDS
        ds[[tb_name]][["label"]]       = tb_label
        ds[[tb_name]][["DS"]]          = curr_ana[["objs"]][["tb_ind_params"]][["value"]][["one_table"]]
        ds[[tb_name]][["DSMETA"]]      = DSMETA
        ds[[tb_name]][["code"]]        = curr_ana[["code"]]
        ds[[tb_name]][["checksum"]]    = state[["NCA"]][["checksum"]]
        ds[[tb_name]][["DSchecksum"]]  = digest::digest(curr_ana[["objs"]][["tb_ind_params"]][["value"]][["one_table"]], 
                                                        algo=c("md5"))
        #-----------------------------------------------------------
      }
    }
  }
  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}


#'@title Initialize New Analysis
#'@description Creates a new NCA analysis in an NCA module
#'@param state NCA state from \code{NCA_fetch_state()}
#'@return NCA state object containing a new empty analysis and that analysis
#'is set as the current active analyisis
NCA_new_ana    = function(state){

  # Incrementing the analysis counter
  state[["NCA"]][["ana_cntr"]] = state[["NCA"]][["ana_cntr"]] + 1

  # Creating a default NCA ID
  nca_id = paste0("NCA_", state[["NCA"]][["ana_cntr"]])

  # Pulling out the dataset views
  DSV = state[["NCA"]][["DSV"]]

  # Using the default dsview for the new analysis
  ana_dsview = names(DSV[["ds"]])[1]

  nca_object_name = paste0("NCA_", state[["MC"]][["nca_object_name"]], "_", state[["NCA"]][["ana_cntr"]])

  # This is the object that contains the different components of
  # the analysis list:
  nca_def =
    list(key                     = nca_id,
         id                      = nca_id,
         idx                     = state[["NCA"]][["ana_cntr"]],
         nca_object_name         = nca_object_name,
         objs                    = list(),
         msgs                    = c("New analysis"),
         ana_dsview              = ana_dsview,
         nca_res                 = NULL,    # JMH is this even used?
         nca_config              = state[["NCA"]][["nca_config"]][["default"]],
         checksum                = digest::digest(NULL, algo=c("md5")),
         ana_scenario            = "",
         fig_view                = "fg_ind_obs",
         fig_type                = "",
         fg_ind_obs_nrow         = "",
         fg_ind_obs_ncol         = "",
         fg_ind_obs_logy         = TRUE,
         fg_ind_obs_rpt          = c("docx", "pptx"),
         tb_ind_obs_rpt          = c("docx"),
         tb_ind_params_rpt       = c("docx"),
         tb_sum_params_rpt       = c("pptx"),
         tab_view                = "tb_ind_obs",
         tab_type                = "",
         code_components         = NULL,
         code                    = NULL,
         col_id                  = "",          # The col_* values will be populated later
         col_conc                = "",
         col_dose                = "",
         col_dur                 = "",
         col_route               = "",
         col_time                = "",
         col_ntime               = "",
         col_group               = "",
         curr_fg_ind_obs         = "",               # Current figure page
         curr_tb_ind_obs         = "",               # Current table page
         curr_tb_ind_params      = "",               #
         curr_tb_sum_params      = "",               #
         include_units           = "",
         intervals               = NULL,             # holds intervals added to the analysis
         interval_range          = c("0", "Inf"),    # Current interval in the interface.
         nca_parameters          = "",
         sampling                = "",
         tab_view                = "",
         units_conc              = "",
         units_dose              = "",
         units_amt               = "",
         units_time              = "",
         notes                   = "",
         isgood                  = FALSE)


# I think these were really only needed for the figure
#        page            = 1,
#        num_pages       = 1,
#        add_isgood      = TRUE,
#        elements_table  = NULL)


  # Default ds
  ds =  state[["NCA"]][["DSV"]][["ds"]][[ana_dsview]]
  # These are the columns in the dataset:
  dscols = names(ds[["DS"]])

  # Initializing the columns
  nca_def[["col_id"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["id"]],
    dscols   = dscols)
  nca_def[["col_time"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["time"]],
    dscols   = dscols)
  nca_def[["col_ntime"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["ntime"]],
    dscols   = dscols)
  nca_def[["col_dose"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["dose"]],
    dscols   = dscols)
  nca_def[["col_dur"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["dur"]],
    dscols   = dscols)
  nca_def[["col_analyte"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["analyte"]],
    dscols   = dscols)
  nca_def[["col_conc"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["conc"]],
    dscols   = dscols)
  nca_def[["col_route"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["route"]],
    dscols   = dscols)

  # Setting the units switch for the analysis
  nca_def[["include_units"]]= state[["MC"]][["units"]][["include_units"]]

  # and also for the counters:
  state[["NCA"]][["button_counters"]][["switch_ana_include_units"]] =
      state[["MC"]][["units"]][["include_units"]]

  #state[["NCA"]][["button_counters"]][["switch_ana_fig"]] =
      #state[["MC"]][["units"]][["fig_interactive"]]

  # add the default units
  nca_def[["units_conc"]] = state[["MC"]][["units"]][["conc_def"]]
  nca_def[["units_time"]] = state[["MC"]][["units"]][["time_def"]]
  nca_def[["units_dose"]] = state[["MC"]][["units"]][["dose_def"]]
  nca_def[["units_amt"]]  = state[["MC"]][["units"]][["amt_def"]]

  # Setting the new analysis id as the current analysis
  state[["NCA"]][["current_ana"]]    = nca_id


  # Setting default values for figures and tables
  nca_def[["fg_ind_obs_nrow"]]  = state[["MC"]][["formatting"]][["preview"]][["nrow"]]
  nca_def[["fg_ind_obs_ncol"]]  = state[["MC"]][["formatting"]][["preview"]][["ncol"]]
  nca_def[["fg_ind_obs_logy"]]  = state[["MC"]][["formatting"]][["preview"]][["logy"]]
  nca_def[["fig_type"]]         = state[["MC"]][["formatting"]][["preview"]][["fig_type"]]
  nca_def[["tab_type"]]         = state[["MC"]][["formatting"]][["preview"]][["tab_type"]]

  # Setting scenario specific options for the default scenario
  ana_scenarios    = state[["MC"]][["ana_scenarios"]]
  ana_scenario_def = state[["MC"]][["ana_scenario_def"]]

  nca_def[["ana_scenario"]]   = ana_scenario_def
  nca_def[["nca_parameters"]] = ana_scenarios[[ana_scenario_def]][["nca_parameters"]]
  nca_def[["sampling"]]       = ana_scenarios[[ana_scenario_def]][["sampling"]]


  # Storing the empty analysis object in the state
  state = NCA_set_current_ana(state, nca_def)

state}


#'@title Sets Current Analysis
#'@description Takes an NCA state and an analysis list and sets that figure list
#'as the value for the active figure
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param ana Analysis list from \code{NCA_fetch_current_ana}
#'@return State with the current analysis updated
NCA_set_current_ana    = function(state, ana){

  # Current analysis ID
  ana_id = state[["NCA"]][["current_ana"]]

  # Current analysis
  state[["NCA"]][["anas"]][[ana_id]] = ana

state}


#'@export
#'@title Determines Default Column Name
#'@description Based on the current analysis, value from the UI, an optional
#'list of patterns to search, and column names from a dataset this function
#'tries to find a default value for a column in the analysis (e.g. subject id,
#'dose, concentration, etc).
#'
#'Generally the following is done:
#'
#' - If curr_ui has a non-NULL, non-"" value it is compared to dscols. If it
#'   is found there that value is returned.
#'
#' - If not then the patterns are considered. If the patterns from the YAML
#'   file are not NULL they are compared sequentially to the columns names.
#'   The first match found is returned.
#'
#' - If nothing is found then the first value of dscols is returned.
#'
#'@param curr_ana Current value in the analysis
#'@param curr_ui  Current value in UI
#'@param patterns List of regular expression patterns to consider.
#'@param dscols   Columns from the dataset.
#'@param null_ok  Logical value indicating if a null result (nothing found) is
#'       OK (default: \code{FALSE})
#'@return Name of column found based on the rules above.
NCA_find_col             = function(curr_ana = NULL,
                                    curr_ui  = NULL,
                                    patterns = NULL,
                                    dscols,
                                    null_ok = FALSE){

  value     = NULL
  COL_FOUND = FALSE


  # JMH add curr_ana parsing here

  if(!COL_FOUND){
    if(!is.null(curr_ui)){
      CURR_UI_HAS_DATA = FALSE
      # Now we see if it has been defined already:
      if(length(curr_ui)>1){
        CURR_UI_HAS_DATA = TRUE
      } else if(curr_ui != ""){
        CURR_UI_HAS_DATA = TRUE
      }

      if(CURR_UI_HAS_DATA){
        # If it has we see if it's in the dataset columns. If we switched
        # views we may have a view where this column doesnt exist
        if(any(curr_ui %in% dscols)){
          COL_FOUND = TRUE
          value = curr_ui[curr_ui %in% dscols]
        }
      }
    }
  }

  # If it's not found then we need to see if there is a detection
  # pattern:
  if(!COL_FOUND){
    if(!is.null(patterns)){
      for(pattern in patterns){
        if(!COL_FOUND){
          if(any(stringr::str_detect(string=dscols, pattern=pattern))){
            COL_FOUND = TRUE
            value = dscols[stringr::str_detect(string=dscols, pattern=pattern)][1]
          }
        }
      }
    }
  }


  # If we made it this far and have not found the column we
  # default to the first column in the dataset:
  if(!COL_FOUND & !null_ok){
    value = dscols[1]
  }

value}


#'@title Fetches Current Analysis
#'@description Takes an NCA state and returns the current active analysis
#'@param state NCA state from \code{NCA_fetch_state()}
#'@return List containing the details of the current analysis. The structure
#'of this list is the same as the structure of \code{state$NCA$anas} in the output of
#'\code{NCA_fetch_state()}.
#'@examples 
#'# We need a state object to use below
#'sess_res = NCA_test_mksession(session=list())
#'state = sess_res$state
#' current_ana = NCA_fetch_current_ana(state)
NCA_fetch_current_ana    = function(state){

  # Current analysis ID
  ana_id = state[["NCA"]][["current_ana"]]

  # Current analysis
  ana = state[["NCA"]][["anas"]][[ana_id]]

ana}


#'@export
#'@title Fetches PKNCA Metadata
#'@description Compiles Metadata from PKNCA
#'@return Dataframe containing PKCNA metadata for NCA parameters.
#'@examples
#' PKNCA_meta = NCA_fetch_PKNCA_meta()
#' head(PKNCA_meta)
NCA_fetch_PKNCA_meta    = function(){

  res = list()

  param_spec = PKNCA::get.interval.cols()

  param_df = NULL
  for(pname in names(param_spec)){
    param_df = rbind(param_df,
      data.frame(parameter   = pname,
                 unit_type   = param_spec[[pname]]$unit_type,
                 pretty_name = param_spec[[pname]]$pretty_name,
                 data_type   = param_spec[[pname]]$datatype,
                 desc        = param_spec[[pname]]$desc)
   )
  }

  res = list(parameters = param_df)

res}


#'@export
#'@title Applies Route Mapping to Dataset
#'@description Used to convert nonstandard dose route values (i.e. "IV") to
#'standard values ("intravascular"). 
#'@param route_map List with names corresponding to the route replacement and
#'       a vector of regular expressions to match.
#'@param route_col Column name with the route data.
#'@param DS        Dataframe containing the dataset.
#'@return Dataset with the route mapping applied.
#'@examples
#' library(readxl)
#' #loading a dataset
#' data_file =  system.file(package="formods","test_data","TEST_DATA.xlsx")
#' myDS = readxl::read_excel(path=data_file, sheet="DATA") 
#'
#'  route_map = list(
#'    intravascular = c("^(?i)iv$"),
#'    extravascular = c("^(?i)sc$", "^(?i)oral")
#'  )
#'
#' head(myDS[["ROUTE"]])
#'
#' myDS = apply_route_map(route_map = route_map, 
#'                        route_col = "ROUTE", 
#'                        DS        = myDS)
#'
#' head(myDS[["ROUTE"]])
#'
apply_route_map  = function(route_map  = list(),
                             route_col = NULL,
                             DS        = NULL){
  for(new_route in names(route_map)){
    evalstr =
    paste0(
      "DS = ",
      "dplyr::mutate(DS, ",route_col,"=",
      "             ifelse(stringr::str_detect(pattern = paste0(route_map[[new_route]], collapse='|'), ",
      "                                        string  = ", route_col,"),",
      "                    new_route,",  route_col,"))"
    )
    eval(parse(text=evalstr))
  }


DS}


#'@export
#'@title Fetches Details About Data Requirements
#'@description Use this to get information about data formats.
#'@return list with details about the data formats
#'@examples
#' NCA_fetch_data_format()
NCA_fetch_data_format = function(){

  yaml_file = system.file(package="ruminate","templates","NCA.yaml")

  MC = yaml::read_yaml(yaml_file)

  res = NULL

  for(ridx in 1:length(MC[["MC"]][["data_format"]][["columns"]])){
    entry = MC[["MC"]][["data_format"]][["columns"]][[ridx]][["entry"]]
    res = rbind(res, as.data.frame(entry))
  }
res}


#'@title Adds Analysis Interval to Current Analysis
#'@description Takes the start time, stop time, and NCA parameters and adds
#'them to the intervals table
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param interval_start Interval start time (numeric).
#'@param interval_stop  Interval stop time (numeric).
#'@param nca_parameters list of NCA parameters in the interval
#'@return State with interval added to the current analysis.
NCA_add_int = function(state, interval_start, interval_stop, nca_parameters){

  # This is a summary table of the nca parameters with the
  # PKNCA parameter name ("parameter") and a textual description ("text")
  np_summary = state[["NCA"]][["nca_parameters"]][["summary"]]

  # Pulling out the rows for the currently selected parameters:
  np_summary = np_summary[np_summary[["parameter"]] %in% nca_parameters, ]

  params_string = paste0(nca_parameters,       collapse = ",")
  names_string  = paste0(np_summary[["text"]], collapse = ", ")

  # Pulling out the current analysis
  current_ana = NCA_fetch_current_ana(state)


  # This adds the row
  current_ana[["intervals"]] =
  rbind( current_ana[["intervals"]],
    data.frame("start"        = interval_start,
               "stop"         = interval_stop,
               "np_actual"    = params_string,
               "np_text"      = names_string,
               "delete"       = FALSE))
  # Saving the current analysis with the interval added
  state = NCA_set_current_ana(state, current_ana)


state}

#'@title Processes Current Analysis to be Run
#'@description Takes the current analysis and checks different aspects to for
#'any issues to make sure it's good to go.
#'@param state NCA state from \code{NCA_fetch_state()}
#'@return Current analysis list with isgood and msgs set
NCA_process_current_ana = function(state){

  omsgs  = c()
  amsgs  = c()
  isgood = TRUE
  current_ana = NCA_fetch_current_ana(state)

  # Analysis Dataset
  ds =  state[["NCA"]][["DSV"]][["ds"]][[current_ana[["ana_dsview"]]]]

  #---------------------------------------------
  # Checking the NCA options
  for(nca_opt in names(current_ana[["nca_config"]])){
    opt_value  = current_ana[["nca_config"]][[nca_opt]][["value"]]
    opt_type   = current_ana[["nca_config"]][[nca_opt]][["type"]]
    opt_label  = current_ana[["nca_config"]][[nca_opt]][["label"]]
    opt_pknca  = current_ana[["nca_config"]][[nca_opt]][["pknca_option"]]

    # First we check all of the numeric types
    if(opt_type == "numeric"){
      opt_value_proc = as.numeric(as.character(opt_value))
      if(is.na(opt_value_proc)){
        isgood = FALSE
        omsgs = c(omsgs, paste0(opt_label, " (", nca_opt, ") should be numeric (found:", opt_value, ")." ))
      } else {
        # Storing the pocessed value as _the_ value. This will ensure that
        # numbers stored as text are actually numbers now:
        current_ana[["nca_config"]][[nca_opt]][["value"]] = opt_value_proc


        # Checking specific numeric boundaries: positive numbers
        if(opt_pknca %in% c("adj.r.squared.factor", "min.span.ratio")){
          if(opt_value_proc < 0 ){
            isgood = FALSE
            omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, "] should be a positive number (found:", opt_value, ")." ))
          }
        }
        # Checking specific numeric boundaries: positive integers
        if(opt_pknca %in% c("min.hl.points")){
          if(opt_value_proc < 0  | (as.integer(opt_value_proc)-opt_value_proc) != 0){
            isgood = FALSE
            omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, "] should be a positive integer (found:", opt_value, ")" ))
          }
        }

        # Checking specific numeric boundaries: [0,1]
        if(opt_pknca %in% c("max.missing", "min.hl.r.squared")){
          # max_missing is a fraction and should be between 0 and 1
          if(opt_value_proc < 0 | opt_value_proc > 1){
            isgood = FALSE
            omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, "] should be between 0 and 1 (found:", opt_value, ")." ))
          }
        }
        # Checking specific numeric boundaries: percentage
        if(opt_pknca %in% c("max.aucinf.pext")){
          if(opt_value_proc < 0 | opt_value_proc > 100){
            isgood = FALSE
            omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, "] should be a percentage 0 and 100 (found:", opt_value, ")." ))
          }
        }
      }
    } else if(opt_type == "mixed"){

      # Mixed options can be strings or numbers so we calculate the
      # numeric value here to use below
      opt_value_num  = as.numeric(as.character(opt_value))
      opt_value_str  =            as.character(opt_value)
      if(opt_pknca %in% c("conc.na")){
        OPT_ERR = FALSE
        # If it's not a valid character value we check the numeric value
        if(!(opt_value_str %in% c("drop"))){
          # Here we check to make sure it's not NA and then if it's positive
          if(is.na(opt_value_num)){
            OPT_ERR = TRUE
          } else if(opt_value_num < 0){
            OPT_ERR = TRUE
          }
          if(OPT_ERR){
            isgood = FALSE
            omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, '] should be "drop" or a positive numeric value (found:', opt_value, ")." ))
          } else {
            # Storing the processed numeric value as _the_ value.
            current_ana[["nca_config"]][[nca_opt]][["value"]] = opt_value_num
          }
        } else {
          # Storing the processed character value as _the_ value.
          current_ana[["nca_config"]][[nca_opt]][["value"]] = opt_value_str
        }
      }
      if(opt_pknca %in% c("conc.blq$first", "conc.blq$middle", "conc.blq$last")){
        OPT_ERR = FALSE
        # If it's not a valid character value we check the numeric value
        if(!(opt_value_str %in% c("drop", "keep"))){
          # Here we check to make sure it's not NA and then if it's positive
          if(is.na(opt_value_num)){
            OPT_ERR = TRUE
          } else if(opt_value_num < 0){
            OPT_ERR = TRUE
          }
          if(OPT_ERR){
            isgood = FALSE
            omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, '] should be "drop", "keep" or a positive numeric value (found:', opt_value, ")." ))
          } else {
            # Storing the processed numeric value as _the_ value.
            current_ana[["nca_config"]][[nca_opt]][["value"]] = opt_value_num
          }
        } else {
          # Storing the processed character value as _the_ value.
          current_ana[["nca_config"]][[nca_opt]][["value"]] = opt_value_str
        }
      }
    } else if(opt_type == "character"){
      if(opt_pknca %in% c("auc.method")){
        if(!(opt_value %in% c("lin up/log down", "linear"))){
          isgood = FALSE
          omsgs = c(omsgs, paste0(opt_label, " [", nca_opt, '] should be either "lin up/log down" or "linear" (found:', opt_value, ")." ))
        }
      }
    }
  }

  #---------------------------------------------
  # Checking the analysis components

  # Check interval start/stop
  # JMH, BILL should I check for multiple instances of the same interval
  # here?
  if(is.null(current_ana[["intervals"]])){
    isgood = FALSE
    amsgs  = c(amsgs, paste0("No intervals have been defined."))
  }

  # First we look at the numeric columns
  num_cols = c("col_conc", "col_time", "col_ntime", "col_dose")
  if(current_ana[["col_dur"]] != "N/A"){
    num_cols = c(num_cols, "col_dur")
  }
  for(col_name in num_cols){
    col_vals = ds[["DS"]][[ current_ana[[col_name]] ]]
    if(is.numeric(col_vals)){
      # I'm assuming that the duration column shouldn't have any NA values.
      if(col_name %in% c("col_dur")){
        if(any(is.na(current_ana[[col_name]]))){
          isgood = FALSE
          amsgs = c(amsgs, paste0("Column: ", current_ana[[col_name]], " should not have NA values."))
        }
      }
    } else {
      isgood = FALSE
      amsgs = c(amsgs, paste0("Column: ", current_ana[[col_name]], " is not numeric."))
    }
  }

  # Checking route information:
  # Applying the route mapping
  route_map = state[["MC"]][["detect_route"]]
  DS_routed = apply_route_map(route_map=route_map, route_col=current_ana[["col_route"]], DS = ds[["DS"]])

  # These are acceptable routes
  OK_ROUTES = c("intravascular", "extravascular")

  # Checking for broken routes
  if( any(!(DS_routed[[current_ana[["col_route"]]]] %in% OK_ROUTES))){
    isgood = FALSE
    amsgs = c(amsgs, paste0("Route  column, ", current_ana[["col_route"]], ", should be either: ", paste0(OK_ROUTES, collapse = ", ")))
  }

  # Checking column specifications for uniqueness. We need
  # only 1 observation for each TIME, GROUP, ANALYTE combination
  unique_cols = c()
  # The time, cycle and id columns are required:
  if(current_ana[["col_time"]] != ""){
    unique_cols = c(unique_cols,  current_ana[["col_time"]]  )
  }
  if(current_ana[["col_id"]] != ""){
    unique_cols = c(unique_cols,  current_ana[["col_id"]]  )
  }
  # Group can be empty as well
  if(current_ana[["col_group"]] != ""){
    unique_cols = c(unique_cols,  current_ana[["col_group"]]  )
  }
  # analyte is select 1 so if it's not used it will be N/A
  if(current_ana[["col_analyte"]] != "N/A"){
    unique_cols = c(unique_cols,  current_ana[["col_analyte"]]  )
  }

  tmp_ds =  ds[["DS"]] |>
    dplyr::group_by_at(unique_cols) |>
    dplyr::mutate(NUNIQUE = length(!!as.name(current_ana[["col_id"]])))

  if(any(tmp_ds[["NUNIQUE"]]> 1)){
    isgood = FALSE
    amsgs = c(amsgs, paste0("The combination of ",
                            state[["MC"]][["labels"]][["select_ana_col_id"]],      ", ",
                            state[["MC"]][["labels"]][["select_ana_col_time"]],    ", ",
                            state[["MC"]][["labels"]][["select_ana_col_analyte"]], ", and ",
                            state[["MC"]][["labels"]][["select_ana_col_group"]],
                            " columns must be unique."))
    amsgs = c(amsgs, paste0("This can commonly happen when you have multiple analytes and forgot to specify the analyte column. "))
    amsgs = c(amsgs, paste0("Current values are:"))
    amsgs = c(amsgs, paste0("  ",  state[["MC"]][["labels"]][["select_ana_col_id"]],      ": ", current_ana[["col_id"]]       ))
    amsgs = c(amsgs, paste0("  ",  state[["MC"]][["labels"]][["select_ana_col_time"]],    ": ", current_ana[["col_time"]]     ))
    amsgs = c(amsgs, paste0("  ",  state[["MC"]][["labels"]][["select_ana_col_analyte"]], ": ", current_ana[["col_analyte"]]  ))
    amsgs = c(amsgs, paste0("  ",  state[["MC"]][["labels"]][["select_ana_col_group"]],   ": ", paste0(current_ana[["col_group"]], collapse="\n")    ))
  }

  #---------------------------------------------
  # Saving the status and messages
  current_ana[["isgood"]] = isgood
  current_ana[["msgs"]]   = c(amsgs, omsgs)

current_ana}

#'@title Builds NCA Code from ui Elements
#'@description Takes the current analysis in the state object and creates the
#'code to run the analysis
#'@param state NCA state from \code{NCA_fetch_state()}
#' JMH update the return list below
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}           Return status of the function.
#'  \item{msgs:}             Messages to be passed back to the user.
#'  \item{code_previous:}    Code to generate the dataset.
#'  \item{code_ana_only:}    Code for the analysis.
#'  \item{code_fg_ind_obs:}  Code to generate figure(s) of individual observations
#'  \item{code_tb_ind_obs:}  Code to generate table(s) of individual observations
#'  \item{code:}             Complete code to run the analysis.
#'  \item{obj:}              List with names of R objects used in then generated code.
#'}
nca_builder = function(state){

  isgood             = TRUE
  cmd                = c()
  msgs               = c()
  objs               = list()
  code               = ""
  code_ana_only      = ""
  code_previous      = ""
  code_tb_ind_obs    = ""
  code_tb_ind_params = ""
  code_fg_ind_obs    = ""

  # Checking and retrieving the current analysis
  current_ana  = NCA_process_current_ana(state)

  # These are the object names that will be generated with the code
  nca_ds_object_name             = paste0(current_ana[["id"]], "_DS")
  nca_rm_object_name             = paste0(current_ana[["id"]], "_route_map")
  nca_drec_object_name           = paste0(current_ana[["id"]], "_dose_rec")
  nca_ints_object_name           = paste0(current_ana[["id"]], "_intervals")
  nca_dose_object_name           = paste0(current_ana[["id"]], "_dose")
  nca_data_object_name           = paste0(current_ana[["id"]], "_data")
  nca_conc_object_name           = paste0(current_ana[["id"]], "_conc")
  nca_res_object_name            = paste0(current_ana[["id"]], "_res")
  nca_units_object_name          = paste0(current_ana[["id"]], "_units")
  nca_fg_ind_obs_object_name     = paste0(current_ana[["id"]], "_figure_ind_obs")
  nca_tb_ind_obs_object_name     = paste0(current_ana[["id"]], "_table_ind_obs")
  nca_tb_ind_params_object_name  = paste0(current_ana[["id"]], "_table_ind_params")

  # We only proceed if were able to process the
  # current analysis successfully
  if(current_ana[["isgood"]]){

    # Pulling out all the column information to
    # use in the command construction below
    col_id        = current_ana[["col_id"]]
    col_conc      = current_ana[["col_conc"]]
    col_dose      = current_ana[["col_dose"]]
    col_dur       = current_ana[["col_dur"]]
    col_analyte   = current_ana[["col_analyte"]]
    col_route     = current_ana[["col_route"]]
    col_time      = current_ana[["col_time"]]
    col_ntime     = current_ana[["col_ntime"]]
    col_group     = current_ana[["col_group"]]

    #--------------------------
    # plus columns
    plus_cols = c()
    # Adding the optional grouping columns
    if(col_group != ""){
      plus_cols = c(plus_cols,
                    col_group)
    }
    # ID is last
    plus_cols = c(plus_cols, col_id)
    #--------------------------
    # These are the columns to keep in the
    # dosing records below:
    dose_select_cols = c(
      col_id,
      col_time,
      col_ntime,
      col_dose,
      col_route)
    if(col_group != ""){
      dose_select_cols = c(dose_select_cols, col_group)
    }
    if(col_dur != "N/A"){
      dose_select_cols = c(dose_select_cols, col_dur)
    }
    if(col_analyte != "N/A"){
      dose_select_cols = c(dose_select_cols, col_analyte)
    }
    #--------------------------
    # Converting sampling into boolean
    if(current_ana[["sampling"]] == "sparse"){
      is_sparse = TRUE
    }else{
      is_sparse = FALSE
    }
    #--------------------------

    # Pulling out the dataset for this analysis
    ds_object_name = current_ana[["ana_dsview"]]
    ds =  state[["NCA"]][["DSV"]][["ds"]][[ ds_object_name ]]

    #--------------------------
    # Adding the text description of the analysis
    cmd = c(cmd, "",  paste0("# ", current_ana[["key"]]))


    #--------------------------
    # Defining the pknca options.
    # These are the options that control PKNCA
    blq_cmd = c(
    "  conc.blq = list(",
    paste0("    first  = ", autocast(current_ana[["nca_config"]][["conc_blq_first"]] [["value"]]), ","),
    paste0("    middle = ", autocast(current_ana[["nca_config"]][["conc_blq_middle"]][["value"]]), ","),
    paste0("    last   = ", autocast(current_ana[["nca_config"]][["conc_blq_last"]]  [["value"]])),
    "    )"
    )

    cmd = c(cmd,
    "# Setting the NCA options",
    "PKNCA::PKNCA.options(")
    for(nca_opt in names( current_ana[["nca_config"]])){
      # We process all of the options except the blq values
      # because they are added separately because
      # BILL MADE THEM BEHAVE DIFFERENTLY THAN THE OTHERS! :)
      if(!(nca_opt %in%
          c("conc_blq_first",
            "conc_blq_middle",
            "conc_blq_last"))){
        pknca_option  = current_ana[["nca_config"]][[nca_opt]][["pknca_option"]]
        nca_opt_value = current_ana[["nca_config"]][[nca_opt]] [["value"]]
        nca_opt_type  = current_ana[["nca_config"]][[nca_opt]] [["type"]]
        if(nca_opt_type %in% c("character", "mixed")){
          nca_opt_value = autocast(nca_opt_value)
        }
        cmd = c(cmd,
          paste0("  ", pknca_option, " = ", nca_opt_value, ",")
        )
      }
    }

    # lastly we append the blq options.
    cmd = c(cmd,
    blq_cmd,
    ")"
    )

    #--------------------------
    # Defining the dataset
    cmd = c(cmd, "# Creating a copy of the source dataset to use below")
    cmd = c(cmd, paste0(nca_ds_object_name, " = ", ds_object_name))

    # If we have a route mapping in the module yaml file
    # we construct the code here to apply that.
    if(is.list(state[["MC"]][["detect_route"]])){
      cmd = c(cmd, "")
      cmd = c(cmd, "# Applying route mapping")
      cmd = c(cmd, paste0(nca_rm_object_name, " = list("))
      rmaps = names(state[["MC"]][["detect_route"]])
      for(rmap_idx in 1:length(rmaps)){
        rmap_txt =paste0("  ",rmaps[[rmap_idx]],'=c("',
                          paste0(state[["MC"]][["detect_route"]][[ rmaps[[rmap_idx]] ]] , collapse = '", "'),
                          '")')
        # we have to add commas to each list element
        # definition except for the last one.
        if(rmap_idx < length(rmaps)){
          rmap_txt =paste0(rmap_txt, ",")
        }
        cmd = c(cmd, rmap_txt)
      }
      cmd = c(cmd, "  )")
      cmd = c(cmd, "")
      cmd = c(cmd, paste0(nca_ds_object_name,
                  " = apply_route_map(route_map=", nca_rm_object_name, ", ",
                  ' route_col = "', current_ana[["col_route"]], '", ',
                  ' DS  = ', nca_ds_object_name, ")"))
    }

    # Creating dosing records
    cmd=c(cmd,
      "",
      "# Creating dosing records by reducing the dataset to one row for each unique:",
      "# Subject, Dose number, Grouping (optional) combination",
      paste0(nca_drec_object_name, " = "),
      paste0("  dplyr::group_by(", nca_ds_object_name,",", paste0(plus_cols, collapse=","),") |> "),
      "  dplyr::filter(dplyr::row_number() == 1) |> ",
      "  dplyr::ungroup() |>",
      paste0("  dplyr::select(",paste0(dose_select_cols, collapse=", "),") |> "),
      paste0("  dplyr::mutate(", col_time, " = ", col_time,"-", col_ntime ,")  # Calculating the dose time from on the nominal offset")
      )

    # Constructing dose_obj
    cmd=c(cmd,
      "",
      "# NCA dosing object",
      paste0(nca_dose_object_name, " = ",
             "PKNCA::PKNCAdose(",
             nca_drec_object_name,", ",
             col_dose, "~",col_time,"|",
             paste(plus_cols, collapse="+"), ", ",
             'route = "', col_route, '"',
             ")"
        )
      )

    # Constructing conc_obj
    col_dur_component =  NULL
    if(col_dur != "N/A"){
      col_dur_component = paste0(', duration = "', col_dur, '"')
    }

    col_analyte_component = NULL
    if(col_analyte != "N/A"){
    col_analyte_component =paste0("/",col_analyte)
    }
    cmd=c(cmd,
      "",
      "# NCA concentration object",
      paste0(
        nca_conc_object_name, " = ",
        "PKNCA::PKNCAconc(",
          nca_ds_object_name, ",",
          col_conc, "~",col_time,"|",
          paste(plus_cols, collapse="+"),
          col_analyte_component,
          ", ",
          'time.nominal = "',col_ntime,'", ',
          'sparse = ', is_sparse,
          col_dur_component,
        ")"
        )
      )

    # Building out the units component
    if(current_ana[["include_units"]]){
    cmd=c(cmd,
      "",
      "# NCA units table",
      paste0(
        nca_units_object_name, " = ",
        "PKNCA::pknca_units_table(",
        'concu = "',   current_ana[["units_conc"]],    '", ',
        'doseu = "',   current_ana[["units_dose"]],    '", ',
        'amountu = "', current_ana[["units_amt"]],     '", ',
        'timeu = "',   current_ana[["units_time"]],    '"',
        ")"
        )
      )
    }


    #--------------------------
    # Creating the intervals:
    # First we collect all of the parameters we want to collect
    nca_params_found = c()
    for(intidx in 1:nrow(current_ana[["intervals"]])){
      params_from_str =
        stringr::str_split(
          string   = current_ana[["intervals"]][intidx, ][["np_actual"]],
          pattern  = "," ,
          simplify = TRUE)
      nca_params_found = c(nca_params_found, params_from_str)
    }


    # This will build the components of the intervals data frame:
    int_df_comp = list()
    for(intidx in 1:nrow(current_ana[["intervals"]])){
      int_df_comp[["start"]] = c( int_df_comp[["start"]], toString(current_ana[["intervals"]][intidx, ][["start"]]))
      int_df_comp[["end"]]  = c( int_df_comp[["end"]],  toString(current_ana[["intervals"]][intidx, ][["stop"]]))

      # These are the parameters for the current interval:
      params_from_str =
        stringr::str_split(
          string   = current_ana[["intervals"]][intidx, ][["np_actual"]],
          pattern  = "," ,
          simplify = TRUE)
      # Now we loop through
      for(param in nca_params_found){
        if(param %in% params_from_str){
          int_df_comp[[param]] = c( int_df_comp[[param]], "TRUE")
        } else {
          int_df_comp[[param]] = c( int_df_comp[[param]], "FALSE")
        }
      }
    }


    cmd = c(cmd,
      "",
      "# Dataframe containing the analysis intervals",
      paste0(nca_ints_object_name, " = "),
      "  data.frame("
      )

    comps     = names(int_df_comp)
    last_comp = rev(comps)[1]
    for(comp in comps){
      if(comp != last_comp){
        comma_str = ","
      } else {
        comma_str = ""
      }

      cmd = c(cmd,
      paste0("    ",comp,"=c(",paste0(int_df_comp[[comp]], collapse=", "), ")", comma_str)
      )

    }

    cmd = c(cmd,
      "  )"
      )
    #--------------------------
    # Creating the data object
    PKNCAdata_args = c(
      paste0("  data.conc = ", nca_conc_object_name),
      paste0("  data.dose = ", nca_dose_object_name),
      paste0("  intervals = ", nca_ints_object_name)
      )
    if(current_ana[["include_units"]]){
      PKNCAdata_args = c(
        PKNCAdata_args,
        paste0("  units = ", nca_units_object_name)
      )
    }
    cmd = c(cmd,
      "",
      "# Pulling everything together to create the data object.",
      paste0(
        nca_data_object_name, " = " ,
        "PKNCA::PKNCAdata(",
         "",
         paste0(PKNCAdata_args, collapse = ",\n"),
        ")"
        )
      )

    # Running the NCA
    cmd = c(cmd,
      "",
      "# Running the NCA",
      paste0(
        nca_res_object_name, " = ",
        "PKNCA::pk.nca(",
          nca_data_object_name,
        ")"
        )
      )

    # code for figures and tables
    # JMH add other UI optoins to function calls here:
    code_fg_ind_obs   = c(
      "",
      "# Generating figures of indiviudal profiles",
      paste0(nca_fg_ind_obs_object_name,
             " = mk_figure_ind_obs(",
             nca_res_object_name, ", ",
             "nfrows = ", current_ana$fg_ind_obs_nrow, ", ",
             "nfcols = ", current_ana$fg_ind_obs_ncol, ", ",
             "log_scale = ", current_ana$fg_ind_obs_logy,
             ")"))

    code_tb_ind_obs   = c(
      "",
      "# Generating tables of individual profiles",
      paste0(nca_tb_ind_obs_object_name, " = mk_table_ind_obs("),
      paste0('  nca_res      = ',  nca_res_object_name,','),
             "  obnd         = rpt,",
      paste0('  notes_detect = c("', paste0(names(state[["MC"]][["reporting"]][["notes"]]), collapse='", "'), '"), '),
      paste0('  not_sampled  = "', state[["MC"]][["reporting"]][["not_sampled"]], '",'),
      paste0('  blq          = "', state[["MC"]][["reporting"]][["blq"]], '",'),
      paste0('  digits       = ',  state[["MC"]][["reporting"]][["digits"]]),
             ")")

    code_tb_ind_params   = c(
      "",
      "# Generating tables of individual NCA parameters",
      paste0(nca_tb_ind_params_object_name, " = mk_table_nca_params("),
      paste0("  nca_res      = ", nca_res_object_name, ", "),
             "  obnd         = rpt, ",
      paste0('  notes_detect = c("', paste0(names(state[["MC"]][["reporting"]][["notes"]]), collapse='", "'), '"), '),
             "  nps          = NCA_nps, ",
      paste0('  text_format  = "', state[["MC"]][["reporting"]][["text_format"]] ,'",'),
      paste0('  not_calc     = "', state[["MC"]][["reporting"]][["not_calc"]] ,'",'),
      paste0('  infinity     = "', state[["MC"]][["reporting"]][["infinity"]] ,'",'),
      paste0("  digits       = ",  state[["MC"]][["reporting"]][["digits"]]   ,','),
      paste0('  mult_str     = "', state[["MC"]][["reporting"]][["mult_str"]] ,'")'))

    # Working out the little code elements:
    code_ana_only      = paste(cmd, collapse="\n")
    code_fg_ind_obs    = paste(code_fg_ind_obs, collapse="\n")
    code_tb_ind_obs    = paste(code_tb_ind_obs, collapse="\n")
    code_tb_ind_params = paste(code_tb_ind_params, collapse="\n")
    code_previous      = ds[["code"]]
    code               = paste(code_previous,
                               code_ana_only,
                               code_fg_ind_obs,
                               code_tb_ind_obs,
                               code_tb_ind_params,
                               collapse="\n")

  } else {
    isgood = FALSE
  }

 #myDS = ds$DS
 #eval(parse(text=code_ana_only))
 #a = paste(code, collapse = "\n")
 #browser()

  # saving any messages:
  msgs = c(msgs, current_ana[["msgs"]])

  state = formods::FM_set_ui_msg(state, msgs)

  # Saving the state information
  current_ana[["isgood"]]            = isgood
  current_ana[["code"]]              = code
  current_ana[["code_components"]]   = list(
    code_previous      = code_previous,
    code_ana_only      = code_ana_only,
    code_fg_ind_obs    = code_fg_ind_obs,
    code_tb_ind_obs    = code_tb_ind_obs,
    code_tb_ind_params = code_tb_ind_params
  )
  current_ana[[ "objs" ]][[ "ds"           ]][[ "name" ]]   = nca_ds_object_name
  current_ana[[ "objs" ]][[ "rm"           ]][[ "name" ]]   = nca_rm_object_name
  current_ana[[ "objs" ]][[ "drec"         ]][[ "name" ]]   = nca_drec_object_name
  current_ana[[ "objs" ]][[ "ints"         ]][[ "name" ]]   = nca_ints_object_name
  current_ana[[ "objs" ]][[ "dose"         ]][[ "name" ]]   = nca_dose_object_name
  current_ana[[ "objs" ]][[ "data"         ]][[ "name" ]]   = nca_data_object_name
  current_ana[[ "objs" ]][[ "conc"         ]][[ "name" ]]   = nca_conc_object_name
  current_ana[[ "objs" ]][[ "res"          ]][[ "name" ]]   = nca_res_object_name
  current_ana[[ "objs" ]][[ "fg_ind_obs"   ]][[ "name" ]]   = nca_fg_ind_obs_object_name
  current_ana[[ "objs" ]][[ "tb_ind_obs"   ]][[ "name" ]]   = nca_tb_ind_obs_object_name
  current_ana[[ "objs" ]][[ "tb_ind_params"]][[ "name" ]]   = nca_tb_ind_params_object_name

  # If we don't include units then the object wont be available, so we only
  # add this conditionally:
  if(current_ana[["include_units"]]){
    current_ana[[ "objs" ]][[ "units"     ]][[ "name" ]]   = nca_units_object_name
  } else {
    current_ana[[ "objs" ]][[ "units"     ]] = NULL
  }

  # Storing the current ana with the updated data:
  state = NCA_set_current_ana(state, current_ana)

state}

#'@title Runs NCA for the Current Analysis
#'@description Takes the current state and runs the current analysis in that
#'state.
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param components List of components to run. By default it will run all of
#'the following. If you just need to regenerate a figure based on the current
#'nca results you can just specify that component. These are the valid
#'components:
#' \itemize{
#'  \item{nca:}               Run NCA analysis
#'  \item{fg_ind_obs:}    Build the figure(s) with the indiviudal observations.
#'  \item{tb_ind_obs:}     Build the table(s) with the indiviudal observations.
#'}
#'@return List with the following components:
#' \itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{msgs:}      Error messages if any issues were encountered.
#'  \item{nca_res:}   PKNCA results if run was successful.
#'}
run_nca_components = function(
  state,
  components=c("nca",
               "fg_ind_obs",
               "tb_ind_obs",
               "tb_ind_params")){

  # Generating code. The following will be added
  # to the current analysis
  #   - code and code components
  #   - exit status of generation
  #   - any messages that were generated
  state = nca_builder(state)

  #valid_components = c("nca", "fg_ind_obs", "tb_ind_obs", "tb_ind_params")

  # Pulling out the current analysis to use and update below
  current_ana = NCA_fetch_current_ana(state)

  # Short name for objects created when running NCA.
  # These are all of the possible names:
  obs_sns_nca  = c("res", "drec", "rm", "ints",
                   "dose", "data", "conc", "units")

  # Sometimes only a subset are generated. For example when units is
  # unchecked. So this will only keep those that exist for this analysis:
  obs_sns_nca = obs_sns_nca[obs_sns_nca %in% names(current_ana[["objs"]])]

  # Messages to pass back to the user
  msgs = c()


  # If current_ana is bad then something in the nca_builder() failed.
  # We want to pass those back to the user.
  if(current_ana[["isgood"]]){
    # Pulling out the dataset for the analysis
    dsview      = current_ana[["ana_dsview"]]
    DS          = state[["NCA"]][["DSV"]][["ds"]][[dsview]][["DS"]]
    #---------------------------------------------
    # Running NCA
    if("nca" %in% components){
      if(current_ana[["isgood"]]){
        # Source to run
        cmd = current_ana[["code_components"]][["code_ana_only"]]

        # NCA environment environment:
        tc_env = list()
        tc_env[[dsview]] = DS
        tc_env[["rpt"]]  = state[["NCA"]][["docx_rpt"]]
        tc_env[["NCA_nps"]]  = state[["NCA"]][["nca_parameters"]][["summary"]]

        # Creating the vector of object names to capture:
        capture = c()
        for(obs_sn in obs_sns_nca){
          capture = c(
           capture,
           current_ana[["objs"]][[obs_sn]][["name"]]
          )
        }

        # Running the analysis and trapping any errors
        nca_run_res = formods::FM_tc(cmd, tc_env, capture)

        # Capturing the exit status
        if(nca_run_res[["isgood"]]){
          # Pulling out the capture objects and saving them
          # to the current analysis
          for(obs_sn in obs_sns_nca){
            current_ana[["objs"]][[obs_sn]][["value"]] =
            nca_run_res[["capture"]][[  current_ana[["objs"]][[obs_sn]][["name"]]  ]]
          }
        } else {
          # If the run failed we capture the error messages to be passed back to the
          # user:
          msgs = c(msgs, nca_run_res[["msgs"]] )

          # We flag the analysis as bad:
          current_ana[["isgood"]] = FALSE
        }
      }
    }
    #---------------------------------------------
    # This list maps the component name to the key
    # with the code for that component
    fig_tabs = list("fg_ind_obs"   = list(code      = "code_fg_ind_obs",
                                          current   = "curr_fg_ind_obs",
                                          list_name = "figures"),
                    "tb_ind_obs"   = list(code      ="code_tb_ind_obs",
                                          current   ="curr_tb_ind_obs",
                                          list_name = "tables"),
                    "tb_ind_params" = list(code      ="code_tb_ind_params",
                                           current   ="curr_tb_ind_params",
                                           list_name = "tables"))
    for(fig_tab in names(fig_tabs)){
      if(current_ana[["isgood"]]){
        if(fig_tab %in% components){
          # Figure or table generation code:
          cmd = current_ana[["code_components"]][[  fig_tabs[[fig_tab]][["code"]]  ]]

          # Object name created to capture:
          capture = current_ana[["objs"]][[fig_tab]][["name"]]

          # In the environment environment we define the dataset,
          # the report template, the nca parameters summary(nps),
          # as the objects created in the NCA analysis:
          tc_env = list()
          tc_env[[dsview]] = DS
          tc_env[["rpt"]]  = state[["NCA"]][["docx_rpt"]]
          tc_env[["NCA_nps"]]  = state[["NCA"]][["nca_parameters"]][["summary"]]

          for(obs_sn in obs_sns_nca){
            tc_env[[  current_ana[["objs"]][[obs_sn]][["name"]]  ]] =
            current_ana[["objs"]][[obs_sn]][["value"]]
          }

          # Running the table or figure generation code:
          nca_run_res = formods::FM_tc(cmd, tc_env, capture)

          # Capturing the results. There are two points of failure here. One
          # is that the code can fail to execute. That should mark the current
          # analysis as bad and pass those failures on to the user.
          if(nca_run_res[["isgood"]]){
            # Pulling out the captured object
            tmp_res = nca_run_res[["capture"]][[  current_ana[["objs"]][[fig_tab]][["name"]]  ]]

            # This pulls out any names for figures, tables, etc:
            list_names = names(tmp_res[[  fig_tabs[[fig_tab]][["list_name"]] ]])

            # We store the result for the current component
            current_ana[["objs"]][[fig_tab]][["value"]] = tmp_res

            # There should be at least one element here:
            if(!is.null(list_names)){
              # This checks the currently selected list element for this
              # component. If it doesn't exist we replace that value with the
              # first element:
             ## JMH broken here:
             #cat(paste0(fig_tab, "\n"))
             #if(fig_tab == "tb_ind_params"){
             #  NCA_1_res = tc_env$NCA_1_res
             #  rpt = tc_env$rpt
             #  NCA_nps = tc_env$NCA_nps
             #
             #  browser()
             #}
              if(!(current_ana[[ fig_tabs[[fig_tab]][["current"]] ]] %in% list_names)){
                current_ana[[ fig_tabs[[fig_tab]][["current"]] ]] = list_names[1]
              }
            } else {
              # The second point of failure is that the component simply has
              # no figures, tables, etc. This might mean that there are none
              # that are valid for this current component. So for that we just
              # set a message so if that component is selected it says there
              # isn't anything to show:

              # If we don't see an element not we set the list name to null
              current_ana[["objs"]][[fig_tab]][["value"]][[
                fig_tabs[[fig_tab]][["list_name"]] ]] = NULL

              # We also attach a message to be returned to the user
              current_ana[["objs"]][[fig_tab]][["value"]][["msgs"]] = c(
                current_ana[["objs"]][[fig_tab]][["value"]][["msgs"]],
                paste0("No ", fig_tabs[[fig_tab]][["list_name"]],
                       " elements found when generating: ", fig_tab)
              )
            }
          } else {
            # If the run failed we capture the error messages to be passed back to the
            # user:
            msgs = c(msgs, nca_run_res[["msgs"]] )

            # We flag the analysis as bad:
            current_ana[["isgood"]] = FALSE
          }
        }
      }

      # If we encounter a failure anywhere above we attach a generic message
      # about the current figure or table
      if(!current_ana[["isgood"]]){
        err_msgs = paste0(c("Unable to create: ", fig_tab, "see above for details."))
        msgs = c(msgs, err_msgs)
      }
    }
  } else {
    msgs = c(msgs, current_ana[["msgs"]] )

  }



  #If there is something wrong we set that in the messages
  if(!current_ana[["isgood"]]){
    state = formods::FM_set_ui_msg(state, msgs)
  }

  # Setting the analysis checksum based on the objs portion.
  current_ana[["checksum"]] = digest::digest(current_ana[["objs"]], algo=c("md5"))

  # Saving any changes to the analysis
  state = NCA_set_current_ana(state, current_ana)

  # Creating a checksum for the entire module:
  all_checksum = ":"
  for(ana_id in names(state[["NCA"]][["anas"]])){
    all_checksum = paste0(all_checksum, ana_id, ":", state[["NCA"]][["anas"]][[ana_id]][["checksum"]], ":")
  }
  state[["NCA"]][["checksum"]] = digest::digest(all_checksum, algo=c("md5"))
  FM_le(state, paste0("module checksum updated:", state[["NCA"]][["checksum"]]))
  

state}

#'@title Creates Tables of Individual Observations from PKNCA Result
#'@description Takes the output of PKNCA and creates a tabular view of the
#'individual observation data. This can be spread out of over several tables
#'(pages) if necessary.
#'@param nca_res Output of PKNCA.
#'@param obnd        onbrand reporting object.
#'@param not_sampled Character string to use for missing data when pivoting.
#'@param blq         Character string to use to indicate data below the level of quantification (value of 0 in the dataset).
#'@param digits      Number of significant figures to report (set to \code{NULL} to disable rounding)
#'@param text_format      Either \code{"md"} for markdown or \code{"text"} (default) for plain text.
#'@param max_width   Maximum width of the final table in inches (A value of \code{NULL} will use 100 inches).
#'@param max_height  Maximum height of the final table in inches (A value of \code{NULL} will use 100 inches).
#'@param max_col     Maximum number of columns to have on a page. Spillover will be wrapped to multiple pages.
#'@param max_row     Maximum number of rows to have on a page. Spillover will hang over the side of the page..
#'@param notes_detect              Vector of strings to detect in output tables (example \code{c("NC", "BLQ")}).
#'@param rows_by Can be either "time" or "id". If it is "time", there will be a
#'column for time and separate column for each subject ID. If rows_by is set
#'to "id" there will be a column for ID and a column for each individual time.
#'@return list containing the following elements
#'\itemize{
#'}
mk_table_ind_obs = function(
  nca_res   ,
  obnd         = NULL,
  not_sampled  = "NS",
  blq          = "BLQ",
  digits       = 3,
  text_format  = "text",
  max_height   = 7,
  max_width    = 6.5,
  max_row      = NULL,
  max_col      = 9,
  notes_detect = NULL,
  rows_by      = "time"){

  # Extracting the needed column names:
  col_id      = nca_res[["data"]][["conc"]][["columns"]][["subject"]]
  col_time    = nca_res[["data"]][["conc"]][["columns"]][["time"]]
  col_conc    = nca_res[["data"]][["conc"]][["columns"]][["concentration"]]
  col_analyte = nca_res[["data"]][["conc"]][["columns"]][["groups"]][["group_analyte"]]

  # Plucking out the units for time and conc
  time_units = ""
  conc_units = ""

  if(!is.null(nca_res[["data"]][["units"]])){
    # Perhaps there is a better way to extract these things but this is the
    # most obvious to me :)
    time_units = nca_res[["data"]][["units"]][nca_res[["data"]][["units"]][["PPTESTCD"]] == "start", ][["PPORRESU"]]
    conc_units = nca_res[["data"]][["units"]][nca_res[["data"]][["units"]][["PPTESTCD"]] == "cmax",  ][["PPORRESU"]]
  }

  # Retaining only the needed columns
  cols_keep = c(col_id, col_time, col_analyte, col_conc)

  # These are the columns that contain the
  # row headers for the table

  # This is the original dataset along with what looks like
  # some extra columns added by PKNCA
  all_data = dplyr::as_tibble(as.data.frame(nca_res[["data"]][["conc"]])) |>
    dplyr::select(dplyr::all_of(cols_keep)) |>
    dplyr::rename(CONC = dplyr::all_of(col_conc))|>
    dplyr::rename(TIME = dplyr::all_of(col_time))|>
    dplyr::rename(ID   = dplyr::all_of(col_id))

  # BILL how does PKNCA handle units with multiple analytes?

  # If we have an analyte column we rename it:
  row_common_head    = NULL
 #row_header_cols    = c()
 #row_header_label   = c()
 #row_header_lengths = c()

  units_data = c()

  # Applying significant digits
  if(!is.null(digits)){
    # For concentration data it is pretty
    # straight forward
    all_data[["CONC"]] = as.character(signif(all_data[["CONC"]], digits))
    all_data[["TIME"]] = as.character(floor(all_data[["TIME"]]) + signif(all_data[["TIME"]] %% 1, digits))
  } else {
    all_data[["CONC"]] = as.character(all_data[["CONC"]])
    all_data[["TIME"]] = as.character(all_data[["TIME"]])
  }

  # Replacing all BLQ and NA values with the appropriate text
  all_data[is.na(all_data[["CONC"]]), ][["CONC"]] = not_sampled
  all_data[all_data[["CONC"]] == "0", ][["CONC"]] = blq

  if(rows_by == "time"){
    if(is.null(row_common_head)){
      row_common_head = data.frame(TIME="Time")
    } else {
      row_common_head = cbind(row_common_head, 
                              data.frame(TIME="Time"))
    }
  # row_header_cols = c("TIME",  row_header_cols)
  # row_header_label   = c("Time", row_header_label)
  # row_header_lengths = c(     1, row_header_lengths)
    if(time_units == ""){
      units_data = c(units_data, "")
    } else {
      units_data = c(units_data, paste0("(", time_units,")"))
    }
    col_header_label = "Concentration ===CONCUNITS=== for ID"
    all_data = tidyr::pivot_wider(all_data, names_from="ID", values_from="CONC", values_fill=not_sampled)
  } else  if(rows_by == "id"){
    if(is.null(row_common_head)){
      row_common_head = data.frame(ID="ID")
    } else {
      row_common_head = cbind(row_common_head, 
                              data.frame(ID="ID"))
    }
  # row_header_cols    = c("ID", row_header_cols)
  # row_header_label   = c("ID", row_header_label)
  # row_header_lengths = c(   1, row_header_lengths)
    if(time_units == ""){
      units_data = c(units_data, "")
    } else {
      units_data = c(units_data, "--")
    }
    col_header_label = "Concentration ===CONCUNITS=== for ID Observation Time ===TIMEUNITS==="
    all_data = tidyr::pivot_wider(all_data, names_from="TIME", values_from="CONC", values_fill=not_sampled)
  }

  if(length(col_analyte) == 1){
    all_data = dplyr::rename(all_data, ANALYTE=col_analyte) |>
               dplyr::arrange(!!as.name("ANALYTE"), !!as.name("TIME"))
    if(is.null(row_common_head)){
      row_common_head = data.frame(ANALYTE="Analyte")
    } else {
      row_common_head = cbind(row_common_head, 
                              data.frame(ANALYTE="Analyte"))
    }
    analyte_units    = " "
    units_data = c(units_data, analyte_units)
  }

  if(conc_units == ""){
    col_header_label = stringr::str_replace(col_header_label, "===CONCUNITS===", "")
  } else {
    col_header_label = stringr::str_replace(col_header_label, "===CONCUNITS===", paste0("(", conc_units, ")"))
  }

  if(time_units == ""){
    col_header_label = stringr::str_replace(col_header_label, "===TIMEUNITS===", "")
  } else {
    col_header_label = stringr::str_replace(col_header_label, "===TIMEUNITS===", paste0("(", time_units, ")"))
  }


  # Splitting the data into header columns and observations
  row_common   = dplyr::select(all_data,  dplyr::all_of(names(row_common_head)))
  table_body   = dplyr::select(all_data, -dplyr::all_of(names(row_common_head)))

  # This converts everything to text:
  table_body        = dplyr::mutate_all(table_body, as.character)

  table_body_head = NULL 
  for(tmpID in names(table_body)){
    if(is.null(table_body_head)){
      table_body_head = eval(parse(text=paste0('dplyr::tibble("', tmpID,'" = c("', col_header_label, '", tmpID))')))
    } else {
      table_body_head[[tmpID]] = c(col_header_label, tmpID)
    }
  }


  # We need to attach the units to the rows as well:
  row_common_head = rbind(row_common_head, units_data)

  #browser()
  stres = onbrand::span_table(
             table_body      = table_body,  
             row_common      = row_common,    
             table_body_head = table_body_head,
             row_common_head = row_common_head,
             obnd            = obnd,
             header_format   = text_format,
             max_width       = max_width,
             max_height      = max_height,
             max_row         = max_row,
             max_col         = max_col,
             notes_detect    = notes_detect)

  res = list(
    #all_data   = all_data,
    isgood     = stres[["isgood"]],
    one_table  = stres[["one_table"]],
    one_header = stres[["one_header"]],
    one_body   = stres[["one_body"]],
    tables     = stres[["tables"]],
    msgs       = stres[["msgs"]]
  )


res}

#'@title Creates Figures of Individual Observations from PKNCA Result
#'@description Takes the output of PKNCA and creates `ggplot` figures faceted
#'by subject id highlighting of certain NCA aspects (e.g. points used for half-life)
#'@param nca_res Output of PKNCA.
#'@param OBS_LAB    Label of the observation axis with optional ===CONCUNITS=== placeholder for units.
#'@param TIME_LAB   Label of the time axis with optional ===TIMEUNITS=== placeholder for units.
#'@param OBS_STRING Label for observation data.
#'@param BLQ_STRING Label for BLQ data.
#'@param NA_STRING  Label for missing data.
#'@param log_scale  Boolean variable to control y-scale (\code{TRUE}: Log 10, \code{FALSE}: linear).
#'@param scales     String to determine the scales used when faceting. Can be either \code{"fixed"}, \code{"free"}, \code{"free_x"}, or \code{"free_y"}.
#'@param nfrows     Number of facet rows per page.
#'@param nfcols     Number of facet cols per page.
#'@return list containing the element \code{figures} which is a list of figure
#'        pages (\code{"Figure 1"}, \code{"Figure 2"}, etc.). Each of these is
#'        a also a list containing two elements:
#'\itemize{
#' \item{gg:}    A ggplot object for that page.
#' \item{notes:} Placeholder for future notes, but NULL now.
#'}
mk_figure_ind_obs = function(
  nca_res       ,
  OBS_LAB         = "Concentration ===CONCUNITS===",
  TIME_LAB        = "Time ===TIMEUNITS===",
  OBS_STRING      = "Observation",
  BLQ_STRING      = "BLQ",
  NA_STRING       = "Missing",
  log_scale       = TRUE,
  scales          = "fixed",
  nfrows          = 4,
  nfcols          = 3){

  # Plucking out the units for time and conc
  time_units = ""
  conc_units = ""
  if(!is.null(nca_res[["data"]][["units"]])){
    # Perhaps there is a better way to extract these things but this is the
    # most obvious to me :)
    time_units = nca_res[["data"]][["units"]][nca_res[["data"]][["units"]][["PPTESTCD"]] == "start", ][["PPORRESU"]]
    conc_units = nca_res[["data"]][["units"]][nca_res[["data"]][["units"]][["PPTESTCD"]] == "cmax",  ][["PPORRESU"]]
  }

  if(conc_units == ""){
    OBS_LAB  = stringr::str_replace(OBS_LAB,  "===CONCUNITS===", "")
  } else {
    OBS_LAB  = stringr::str_replace(OBS_LAB,  "===CONCUNITS===", paste0("(", conc_units, ")"))
  }

  if(time_units == ""){
    TIME_LAB = stringr::str_replace(TIME_LAB, "===TIMEUNITS===", "")
  } else {
    TIME_LAB = stringr::str_replace(TIME_LAB, "===TIMEUNITS===", paste0("(", time_units, ")"))
  }

  col_id      = nca_res[["data"]][["conc"]][["columns"]][["subject"]]
  col_time    = nca_res[["data"]][["conc"]][["columns"]][["time"]]
  col_conc    = nca_res[["data"]][["conc"]][["columns"]][["concentration"]]
  col_group   = nca_res[["data"]][["conc"]][["columns"]][["groups"]][["group_vars"]]
  col_analyte = nca_res[["data"]][["conc"]][["columns"]][["groups"]][["group_analyte"]]


  # Retaining only the needed columns
  cols_keep = unique(c(col_id, col_time, col_conc, col_group, col_analyte))

  # If this value is NULL it will be set to the lowest value for that
  # individual

  # This is the original dataset along with what looks like
  # some extra columns added by PKNCA
  all_data = dplyr::as_tibble(as.data.frame(nca_res[["data"]][["conc"]])) |>
    dplyr::select(dplyr::all_of(cols_keep))                               |>
    dplyr::rename(CONC = dplyr::all_of(col_conc))                         |>   # Renaming columns to standard values
    dplyr::rename(TIME = dplyr::all_of(col_time))                         |>
    dplyr::rename(ID   = dplyr::all_of(col_id))                           |>
    dplyr::group_by(!!as.name(col_group))                                 |>
    dplyr::mutate(NONOBS = min(.data[["CONC"]][.data[["CONC"]]>0 &       
                               !is.na(.data[["CONC"]])]))                 |>   # Creating a concentration for non-observations (0 and NA below)
    dplyr::ungroup()                                                      |>
    dplyr::mutate(PKNCA_DATA_TYPE = OBS_STRING)                           |>   # This sets all data types to "Observation":w
    dplyr::mutate(PKNCA_DATA_TYPE = ifelse(.data[["CONC"]]==0,                 # Setting the BLQ Flag
                                           BLQ_STRING,
                                           .data[["PKNCA_DATA_TYPE"]]))   |>
    dplyr::mutate(PKNCA_DATA_TYPE = ifelse(is.na(.data[["CONC"]]),             # Setting the Missing Flag
                                           NA_STRING,
                                           .data[["PKNCA_DATA_TYPE"]]))   |>
    dplyr::mutate(CONC            = ifelse(is.na(.data[["CONC"]]),             # Setting missing values to values for plotting
                                           .data[["NONOBS"]],
                                           .data[["CONC"]]))       |>
    dplyr::mutate(CONC            = ifelse(.data[["CONC"]] == 0,               # Setting missing values to values for plotting
                                           .data[["NONOBS"]],
                                           .data[["CONC"]]))



  # Subjects remaining to plot
  subs_left = unique(all_data[["ID"]])
  # Number of subjects per plot:
  subs_pp   = nfrows*nfcols

  # This is a list where we'll store the figures
  figures = list()

  if(length(col_analyte) > 0){
    col_group_all = c(col_group, col_analyte)
  } else {
    col_group_all = c(col_group)
  }


  # Constructing the mutate command based on the col_group_all above
  mucmd = c(
    "all_data = dplyr::mutate(all_data, GROUP_ALL = paste0(",
    paste(col_group_all, collapse=",':',"),
    "))"
  )
  eval(parse(text=paste0(mucmd, collapse="")))

  fig_idx=1
  while(length(subs_left) > 0){

    fig_key = paste0("Figure_", fig_idx)

    # Figuring out how many subjects to plot on the
    # current figure
    if(length(subs_left) < subs_pp){
      last_sub = length(subs_left)
    } else {
      last_sub = subs_pp
    }
    subs_current = subs_left[c(1:last_sub)]

    # This dataset contains the subset of the current subjects for plotting.
    plot_ds = dplyr::filter(all_data, .data[["ID"]] %in% subs_current)


    p = ggplot2::ggplot(data = plot_ds)

    p = p + ggplot2::geom_line( ggplot2::aes(x=.data[["TIME"]],
                                             y=.data[["CONC"]],
                                             group=.data[["GROUP_ALL"]]),
                                             linetype='dashed', color='grey')

    if(length(col_analyte) == 1){
      # If there is an analyte we set the shape based on the analyte column
      p = p + ggplot2::geom_point(ggplot2::aes(x     = .data[["TIME"]],
                                               y     = .data[["CONC"]],
                                               shape = !!as.name(col_analyte),
                                               group = .data[["GROUP_ALL"]],
                                               color = .data[["PKNCA_DATA_TYPE"]]))

    } else {
      p = p + ggplot2::geom_point(ggplot2::aes(x     = .data[["TIME"]],
                                               y     = .data[["CONC"]],
                                               group = .data[["GROUP_ALL"]],
                                               color = .data[["PKNCA_DATA_TYPE"]]))
    }
    p = p + ggplot2::facet_wrap(.~.data[["ID"]], ncol=nfcols, nrow=nfrows, scales=scales)
    p = p + ggplot2::theme_light()
    if(log_scale){
      p = p + ggplot2::scale_y_log10()
    }
    p = p + ggplot2::xlab(TIME_LAB)
    p = p + ggplot2::ylab(OBS_LAB)
    p = p + ggplot2::labs(color="Data Type")
    p = p + ggplot2::theme(legend.position="bottom")

    figures[[fig_key]]$gg    = p
    figures[[fig_key]]$notes = NULL

    # Removing the plotted subjects from the list
    subs_left = subs_left[!(subs_left %in% subs_current)]
    fig_idx = fig_idx+1
  }

  res = list(
    figures = figures
  )


res}

#'@export
#'@title Fetches the Current Analysis Object
#'@description Takes the current state and object type and returns the
#'currently selected object. For example if you have specified figure, it will
#'look at the output figure selected and the figure number of that figure and
#'return the ggplot object for that.
#'by subject id highlighting of certain NCA aspects (e.g. points used for half-life)
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param obj_type Type of object to return (either "table" or "figure").
#'@return list with a format that depends on the obj_type.
#' For figures:
#'\itemize{
#'  \item[ggplot:]   ggplot object of the figure. 
#'  \item{isgood:}   Return status of the function.
#'  \item{msgs:}     Messages to be passed back to the user.
#'}
#' For tables:
#'\itemize{
#'  \item[df:]       Dataframe of the current table.
#'  \item[ft:]       Flextable object of the current table.
#'  \item[notes:]    Any table notes to be included.
#'  \item{isgood:}   Return status of the function.
#'  \item{msgs:}     Messages to be passed back to the user.
#'}
#'@examples 
#'# We need a state object to use below
#'sess_res = NCA_test_mksession(session=list())
#'state = sess_res$state
#'
#' # Current active table:
#' res = NCA_fetch_current_obj(state, "table")
#' res$ft
#'
#' # Current active figure:
#' res = NCA_fetch_current_obj(state, "figure")
#' res$ggplot
NCA_fetch_current_obj = function(state, obj_type){

  obj    = list()
  isgood = TRUE
  msgs   = c()

  obj_types = c("figure", "table")

  if(obj_type %in% obj_types){
    current_ana = NCA_fetch_current_ana(state)
    if(current_ana[["isgood"]]){
      if(obj_type == "figure"){
        # We process this based on the fig_view
        if( current_ana[["fig_view"]] == "fg_ind_obs"){
    
          # This is the current facet page selected in the interface:
          curr_fg_ind_obs = current_ana[["curr_fg_ind_obs"]]
          if( curr_fg_ind_obs  %in%
             names(current_ana[["objs"]][["fg_ind_obs"]][["value"]][["figures"]])){
    
             # Pulling out the ggplot object
             obj[["ggplot"]] = current_ana[["objs"]][["fg_ind_obs"]][["value"]][["figures"]][[ curr_fg_ind_obs ]][["gg"]]
    
          } else {
            # This can happen when you are switching between a faceting that has
            # more facets than the one you currently have. So if you go from a
            # 1x1 facet and have figure 20 selected and then you switch to a
            # 2x3. Then figure 20 doesn't exist any longer.
    
            # In that case we just return the first value:
            new_fg_ind_obs =names(current_ana[["objs"]][["fg_ind_obs"]][["value"]][["figures"]])[1]
            obj[["ggplot"]] = current_ana[["objs"]][["fg_ind_obs"]][["value"]][["figures"]][[ new_fg_ind_obs ]][["gg"]]
          }
        } else {
          isgood          = FALSE
          msgs            = paste0("Unknown figure output: ", current_ana[["fig_view"]])
          obj[["ggplot"]] = formods::FM_mk_error_fig(msgs)
        }
      }
      if(obj_type == "table"){
        # We process this based on the tab_view
        # Individual observations:
        if( current_ana[["tab_view"]] == "tb_ind_obs"){
          # This is the current table page selected in the interface:
          curr_tb_ind_obs = current_ana[["curr_tb_ind_obs"]]
          if( curr_tb_ind_obs  %in%
             names(current_ana[["objs"]][["tb_ind_obs"]][["value"]][["tables"]])){
             # Pulling out the current table object
             obj = current_ana[["objs"]][["tb_ind_obs"]][["value"]][["tables"]][[ curr_tb_ind_obs ]]
    
          } else {
            # In that case we just return the first value:
            new_tb_ind_obs =names(current_ana[["objs"]][["tb_ind_obs"]][["value"]][["tables"]])[1]
            obj = current_ana[["objs"]][["tb_ind_obs"]][["value"]][["tables"]][[ new_tb_ind_obs ]]
          }
    
        # NCA raw:
        } else if( current_ana[["tab_view"]] == "tb_nca_raw"){
          # The raw output is just pulled from the raw results:
          obj[["df"]]    = as.data.frame(current_ana[["objs"]][["res"]][["value"]])
          obj[["ft"]]    = flextable::flextable(as.data.frame(current_ana[["objs"]][["res"]][["value"]]))
          obj[["notes"]] = ""
        # Table of individual NCA parameters
        } else if( current_ana[["tab_view"]] == "tb_ind_params"){
          # This is the current table page selected in the interface:
          curr_tb_ind_params = current_ana[["curr_tb_ind_params"]]
    
          if( curr_tb_ind_params  %in%
             names(current_ana[["objs"]][["tb_ind_params"]][["value"]][["tables"]])){
             # Pulling out the current table object
             obj = current_ana[["objs"]][["tb_ind_params"]][["value"]][["tables"]][[ curr_tb_ind_params ]]
          } else {
            # In that case we just return the first value:
            new_tb_ind_params =names(current_ana[["objs"]][["tb_ind_params"]][["value"]][["tables"]])[1]
            obj = current_ana[["objs"]][["tb_ind_params"]][["value"]][["tables"]][[ new_tb_ind_params ]]
          }
        #  Summary of NCA Parameters
        } else if( current_ana[["tab_view"]] == "tb_sum_params"){
          # JMH add NCA parameters summary view
          isgood = FALSE
          msgs   = paste0("Placeholder for table not yet defined: ", current_ana[["tab_view"]])
        } else{
          isgood = FALSE
          msgs   = paste0("Unknown table output: ", current_ana[["tab_view"]])
        }
      }
    }
  } else {
    isgood = FALSE
    msgs   = c(msgs, paste0("Unknown object type: ", obj_type))
  }

  # Appending msgs and isgood status
  obj[["isgood"]] = isgood
  obj[["msgs"]]   = msgs

obj}

#'@export
#'@title Run the {ruminate} Shiny App
#'@description Runs the pharmacometrics ruminate app.
#'@return Nothing.
#'@examples
#'if (interactive()) {
#' ruminate()
#'}
ruminate = function(){
  shiny::runApp(system.file(package="ruminate", "templates","ruminate.R"))}


#'@title Create Tabular Output from PKNCA Results
#'@description Create paginated tables from PKNCA to use in reports and Shiny
#'apps.
#'@param nca_res Output of PKNCA.
#'@param type     Type of table to generate. Can be either \code{"individual"} or \code{"summary"]}.
#'@param grouping How to group columns in tables. Can be either \code{"interval"} or \code{"parameter"]}.
#'@param not_calc Text string to replace NA values with to indicated values were not calculated.
#'@param mult_str Text string to replace * values in units.
#'@param infinity Text string to replace infinity in time intervals in column headers.
#'@param obnd     onbrand reporting object.
#'@param nps NCA parameter summary table with the following columns.
#'   \itemize{
#'     \item{parameter:}      PKNCA Paramter name.
#'     \item{text:}           Name used in text output.
#'     \item{md:}             Name used markdown output.
#'     \item{latex:}          Name used in latex output.
#'     \item{description:}    Verbose textual description of the parameter.
#'   }
#'@param digits           Number of significant figures to report (set to \code{NULL} to disable rounding)
#'@param text_format      Either \code{"md"} for markdown or \code{"text"} (default) for plain text.
#'@param notes_detect     Vector of strings to detect in output tables (example \code{c("NC", "BLQ")}).
#'@param max_width        Maximum width of the final table in inches (A value of \code{NULL} will use 100 inches).
#'@param max_height       Maximum height of the final table in inches (A value of \code{NULL} will use 100 inches).
#'@param max_col          Maximum number of columns to have on a page. Spillover will be wrapped to multiple pages.
#'@param max_row          Maximum number of rows to have on a page. Spillover will hang over the side of the page..
#'@return list containing the following elements
#'\itemize{
#'   \item{raw_nca:}     Raw PKNCA output.
#'   \item{isgood:}      Boolean indicating the exit status of the function.
#'   \item{one_table:}   Dataframe of the entire table with the first lines containing the header.
#'   \item{one_body:}    Dataframe of the entire table (data only).
#'   \item{one_header:}  Dataframe of the entire header (row and body, no data).
#'   \item{tables:}      Named list of tables. Each list element is of the output
#'   \item{msgs:}        Vector of text messages describing any errors that were found.
#'   format from \code{\link[onbrand]{build_span}}.
#'}
mk_table_nca_params = function(
  nca_res   ,
  type         = "individual",
  grouping     = "interval",
  not_calc     = "NC",
  obnd         = NULL,
  nps          = NULL,
  mult_str     = "*",
  # JMH make sure this is used as intended below
  infinity     = "inf",
  digits       = NULL,
  text_format  = "text",
  notes_detect = NULL,
  max_height   = 7,
  max_width    = 6.5,
  max_row      = NULL,
  max_col      = NULL){

  # Pulling out the different pieces from the PKNCA object
  raw_nca   = as.data.frame(nca_res)
  intervals = nca_res[["data"]][["intervals"]]

  # Extracting the needed column names:
  col_id      = nca_res[["data"]][["conc"]][["columns"]][["subject"]]
  col_group   = c(nca_res[["data"]][["conc"]][["columns"]][["groups"]][["group_vars"]],
                  nca_res[["data"]][["conc"]][["columns"]][["groups"]][["group_analyte"]])

  # This holds a summary of the interval information. Essentially it contains
  # any parameter name where there was a request output (for any interval):
  int_sum   = c()
  ints_all = names(intervals)[!(names(intervals) %in% c("start", "end"))]
  for(iname in ints_all){
    if(any(intervals[[iname]])){
      int_sum = c(int_sum, iname)
    }
  }

  # These are just the NCA data the user requested:
  nca_data = dplyr::filter(raw_nca, .data[["PPTESTCD"]] %in% int_sum)

  # Determining if we have multiple dose data or not:
  if(unique(length(nca_data[["DOSE_NUM"]])) > 1){
    IS_MD = TRUE
  } else{
    IS_MD = FALSE
  }

  # Determining if we have units
  if(is.null(nca_res[["data"]][["units"]])){
   HAS_UNITS = FALSE
  } else{
   HAS_UNITS = TRUE
  }

  # Applying significant digits
  if(!is.null(digits)){
    nca_data[["PPORRES"]] = signif(digits=digits,  nca_data[["PPORRES"]])
  }

  int_temp = "[===START===,===END===]"
  nca_data = nca_data |>
    dplyr::mutate(range = int_temp) |>
    dplyr::mutate(range = stringr::str_replace(
                          string      = .data[["range"]],
                          pattern     = "===START===",
                          replacement = as.character(.data[["start"]]))) |>
    dplyr::mutate(range = stringr::str_replace(
                          string      = .data[["range"]],
                          pattern     = "===END===",
                          replacement = as.character(.data[["end"]])))  |>
    dplyr::mutate(range = stringr::str_replace(
                          string      = .data[["range"]],
                          pattern     = "Inf",
                          replacement = infinity))


  if(type == "summary"){
    # JMH create summary table here
  }
  if(type == "indiviudal"){
    # NAs come from parameters that were not calculated for whatever reason.
    # Having NAs pop up in reports is kind of sloppy so here we replace them
    # with the user value:
    nca_data = nca_data |>
      dplyr::mutate(PPORRES =as.character(.data[["PPORRES"]])) |>
      dplyr::mutate(PPORRES = ifelse(.data[["PPORRES"]] == "NA",
                                     not_calc,
                                     .data[["PPORRES"]] ))
  }


  # We're going to group by everything except the id
  rpt_name_group = c(col_group[!(col_group %in% col_id)])

  nca_data = dplyr::mutate(nca_data,
                           pnames=paste0(.data[["PPTESTCD"]],
                                         ":",
                                         .data[["range"]]))

  # We're separating out the data from the other elements of the table
  # like parameter names, intervals, etc so we'll have two different
  # objects:
  #   - tdata:
  #        Data frame with the actual data to be reported.
  #   - nca_data:
  #        This has the same information in long format and can be
  #        used to look up things like units when constructing the
  #        headers for reporting.
  col_keep = c(col_id, rpt_name_group, "pnames", "PPORRES")

  tdata = nca_data |>
    dplyr::mutate(PPORRES = ifelse(is.na(.data[["PPORRES"]]), not_calc, .data[["PPORRES"]])) |>
    dplyr::select(dplyr::all_of(col_keep)) |>
    tidyr::pivot_wider(names_from="pnames",
                       values_from="PPORRES")

  # Creating a lookup table to group output and create headers
  col_lookup = nca_data                     |>
    dplyr::group_by(.data[["pnames"]])      |>
    dplyr::filter(row_number() == 1)        |>
    dplyr::rename(parameter  = "PPTESTCD")

  col_lookup_keep = c("range", "pnames", "parameter")
  if(HAS_UNITS){
    col_lookup_keep = c(col_lookup_keep, "units")
    col_lookup = dplyr::rename(col_lookup, units="PPORRESU")
  }

  # This is lookup with information from the dataset:
  col_lookup = dplyr::select(col_lookup,dplyr::all_of(col_lookup_keep))

  # Now we need to merge in the column names used for reporting:
  nps_found =
       dplyr::filter(nps,  .data[["parameter"]] %in% col_lookup[["parameter"]])   |>
       dplyr::right_join(col_lookup,  by=c("parameter"="parameter"))

  if(grouping == "interval"){
    if(text_format == "md"){
      nps_found = dplyr::arrange(nps_found, .data[["range"]], .data[["md"]]) 
    } else {
      nps_found = dplyr::arrange(nps_found, .data[["range"]], .data[["text"]]) 
    }
  }

  if(grouping == "parameter"){
    if(text_format == "md"){
      nps_found = dplyr::arrange(nps_found, .data[["md"]], .data[["range"]])
    } else {
      nps_found = dplyr::arrange(nps_found, .data[["text"]], .data[["range"]])
    }
  }


  # Now we reorder the columns of tdata according to the grouping above:
  tdata = dplyr::select(tdata,
                        dplyr::all_of(c(col_id, rpt_name_group, nps_found[["pnames"]])))

  # Rows that are common for each table:
  row_common  = dplyr::select(tdata, dplyr::all_of(c(col_id, rpt_name_group)))

  # The table body
  table_body  = dplyr::select(tdata, dplyr::all_of((nps_found[["pnames"]])))

  # Identifies which of the common rows to highlight.
  table_body_head = c()
  row_common_head = c()

  # here we're repalcing the multiplication sign in the units with
  # whatever is in the mult_str:
  if(HAS_UNITS){
    nps_found[["units"]] = stringr::str_replace(
      string  = nps_found[["units"]],
      pattern = "\\*",
      replacement = mult_str)
  }


  # Here we pull out the headers based on the text format specieid
  if(text_format == "md"){
    pheader_names = nps_found[["md"]] 
  } else {
    pheader_names = nps_found[["text"]] 
  }
  # Headers are calculated differently with and without units:
  if(HAS_UNITS){
    table_body_head = matrix(
      data = c(pheader_names, nps_found[["units"]], nps_found[["range"]]),
      byrow = TRUE,
      ncol  = nrow(nps_found))
    row_common_head = matrix(
      data  = c(names(row_common), rep("", ncol(row_common)*2)),
      byrow = TRUE,
      ncol  = ncol(row_common) )
  } else {
    table_body_head = matrix(
      data = c(pheader_names, nps_found[["range"]]),
      byrow = TRUE,
      ncol  = nrow(nps_found))
    row_common_head = matrix(
      data  = c(names(row_common), rep("", ncol(row_common))),
      byrow = TRUE,
      ncol  = ncol(row_common) )
  }

  stres = onbrand::span_table(
             table_body      = table_body,
             row_common      = row_common,
             table_body_head = table_body_head,
             row_common_head = row_common_head,
             obnd            = obnd,
             header_format   = text_format,
             max_width       = max_width,
             max_height      = max_height,
             max_row         = max_row,
             max_col         = max_col,
             notes_detect    = notes_detect)

  res = list(
    raw_nca    = raw_nca,
    isgood     = stres[["isgood"]],
    one_table  = stres[["one_table"]],
    one_header = stres[["one_header"]],
    one_body   = stres[["one_body"]],
    tables     = stres[["tables"]],
    msgs       = stres[["msgs"]]
  )

res}

#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_UD An ID string that corresponds with the ID used to call the UD modules UI elements
#'@param id_DW An ID string that corresponds with the ID used to call the DW modules UI elements
#'@param id_ASM An ID string that corresponds with the ID used to call the ASM modules UI elements
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:} Boolean indicating the exit status of the function.
#'   \item{session:} The value Shiny session variable (in app) or a list (outside of app) after initialization.
#'   \item{input:} The value of the shiny input at the end of the session initialization.
#'   \item{state:} App state.
#'   \item{rsc:} The \code{react_state} components.
#'}
#'@examples
#' sess_res = NCA_test_mksession(session=list())
NCA_test_mksession = function(session, id = "NCA", id_UD="UD", id_DW="DW", id_ASM="ASM"){

  isgood = TRUE
  rsc    = NULL
  input  = list()

  # Populating the session with UD and DW components
  sess_res = formods::DW_test_mksession(session=session, id=id_DW, id_UD = id_UD)
  if(!("ShinySession" %in% class(session))){
    session = sess_res[["session"]]
  }

  # Pulling out the react state components
  rsc         = sess_res$rsc
  react_state = rsc

  # YAML files for the fetch calls below
  FM_yaml_file  = system.file(package = "formods",  "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "ruminate", "templates", "NCA.yaml")

  # empty input
  input = list()

  # Creating an empty state object
  state = NCA_fetch_state(id             = id,
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         id_ASM          = id_ASM,
                         id_UD           = id_UD,
                         id_DW           = id_DW,
                         react_state     = react_state)



  #------------------------------------
  # First First dose PK only with BQL values
  current_ana = NCA_fetch_current_ana(state)
  # Setting the dataset:
  current_ana[["ana_dsview"]] = "DW_myDS_7"

  # Defining the columns
  current_ana[["col_id"]]           = "ID"
  current_ana[["col_time"]]         = "TIME_DY"
  current_ana[["col_ntime"]]        = "NTIME_DY"
  current_ana[["col_conc"]]         = "DV"
  current_ana[["col_dose"]]         = "DOSE"
  current_ana[["col_route"]]        = "ROUTE"
  current_ana[["col_analyte"]]      = "N/A"    #Analyte  
  current_ana[["col_dur"]]          = "N/A"    #Duration 

  # Setting the caption/analysis key
  current_ana[["key"]]              = "PK NCA Analysis Example"

  # This will update the changes to the current analysis:
  state = NCA_set_current_ana(state, current_ana)

  # This will load the scenario
  state = NCA_load_scenario(state, "sd_iv")

  # And then this will run the intervals in the scenario:
  state = run_nca_components(state)
  #------------------------------------
  # First First dose PK/PD with BQL values
  # Create a new analysis
  state = NCA_new_ana(state)
  current_ana = NCA_fetch_current_ana(state)
  # Setting the dataset:
  current_ana[["ana_dsview"]] = "DW_myDS_6"

  # Setting the caption/analysis key
  current_ana[["key"]]              = "PK/PD NCA Analysis Example"

  # Defining the columns
  current_ana[["col_id"]]           = "ID"
  current_ana[["col_time"]]         = "TIME_DY"
  current_ana[["col_ntime"]]        = "NTIME_DY"
  current_ana[["col_conc"]]         = "DV"
  current_ana[["col_dose"]]         = "DOSE"
  current_ana[["col_route"]]        = "ROUTE"
  current_ana[["col_analyte"]]      = "CMT"    #Analyte  
  current_ana[["col_dur"]]          = "N/A"    #Duration 

  # This will update the changes to the current analysis:
  state = NCA_set_current_ana(state, current_ana)

  # This will load the scenario
  state = NCA_load_scenario(state, "sd_iv")

  # And then this will run the intervals in the scenario:
  state = run_nca_components(state)
  #------------------------------------

  # This functions works both in a shiny app and outside of one
  # if we're in a shiny app then the 'session' then the class of
  # session will be a ShinySession. Otherwise it'll be a list if
  # we're not in the app (ie just running test examples) then
  # we need to set the state manually
  if(("ShinySession" %in% class(session))){
    FM_set_mod_state(session, id, state)
  } else {
    session = FM_set_mod_state(session, id, state)
  }

  # Required for proper reaction:
  #rsc[[id]]  = list(FG = list(checksum=state[["FG"]][["checksum"]]))

  res = list(
    isgood  = isgood,
    session = session,
    input   = input,
    state   = state,
    rsc     = rsc
  )
}

#'@title Loads Pre-Defined Scenario
#'@description Loads a pre-defined analysis scneario from the NCA YAML config
#'file.
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param ana_scenario  Short name of the analysis scenario to load from the config file.
#'@return NCA state object with the scenario loaded and relevant notifications
#'set. 
NCA_load_scenario = function(state, ana_scenario){

    formods::FM_le(state, paste0("loading analysis scenario: ", ana_scenario))

    scenario_def  =  state[["MC"]][["ana_scenarios"]][[ana_scenario]]

    # Pulling out the current analysis
    current_ana = NCA_fetch_current_ana(state)

    # Overwriting the scenario components in the current analysis
    current_ana[["nca_parameters"]] = scenario_def[["nca_parameters"]]
    current_ana[["sampling"]]       = scenario_def[["sampling"]]

    # Removing any previous intervals
    current_ana[["intervals"]]      = NULL

    # Storing any changes here:
    state = NCA_set_current_ana(state, current_ana)

    # The current analysis will be further updated internally
    # in NCA_add_init()
    for(int_idx in 1:length(scenario_def[["intervals"]])){
      scenario_row = scenario_def[["intervals"]][[int_idx]]
      interval_start = as.numeric(as.character(scenario_row$row[["start"]]))
      interval_stop  = as.numeric(as.character(scenario_row$row[["stop"]]))
      nca_parameters = scenario_row$row[["nca_parameters"]]

      state = NCA_add_int(state=state,
        interval_start = interval_start,
        interval_stop  = interval_stop,
        nca_parameters = nca_parameters)
    }

    notify_text = paste0("Scenario: ", state[["MC"]][["ana_scenarios"]][[current_ana[["ana_scenario"]]]][["description"]])
    state = FM_set_notification(state, notify_text, "NCA scenario set", "info")

state}
