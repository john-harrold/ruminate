#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor


# JMH
# Load state notes:
# - Replace current state with loaded state
# - Change button values to current or zero


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
NCA_Server <- function(id,
                      FM_yaml_file  = system.file(package = "formods",   "templates", "formods.yaml"),
                      MOD_yaml_file = system.file(package = "ruminate",  "templates", "NCA.yaml"),
                      id_ASM        = "ASM",
                      id_UD         = "UD",
                      id_DW         = "DW",
                      react_state  = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Create ui outputs here:
    output$ZZ_ui_element = renderUI({
      uiele = NULL
      uiele})

    #------------------------------------
    # Generated data reading code
    observe({
      # Reacting to file changes
      input$input_data_file
      input$input_select_sheet
      state = NCA_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             id_ASM          = id_ASM,
                             id_UD           = id_UD,
                             id_DW           = id_DW,
                             react_state     = react_state)

      if(is.null(state[["NCA"]][["code"]])){
        uiele = "# No file loaded"
      } else {
        uiele = state[["NCA"]][["code"]]
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "NCA_ui_ace_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # User messages:
    output$ui_nca_msg = renderText({
      input[["button_state_save"]]
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
                        placeholder  = state[["MC"]][["labels"]][["ph"]][["ana_key"]])

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
                           placeholder  = state[["MC"]][["labels"]][["ph"]][["notes"]])


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
    # react_state[[id_UD]]
    # react_state[[id_DW]]
    # react_state[[id_ASM]]
    #
    # input[["button_ana_new"]]
    # input[["button_ana_del"]]
    # input[["button_ana_copy"]]
    # input[["button_ana_save"]]
    # input[["select_current_ana"]]
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
      req(input$select_current_view)


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

      # If this is triggered before datasets have been loaded the state will
      # be bad:
      # Pulling out the data set views catalog
      ds_catalog = state[["NCA"]][["DSV"]][["catalog"]]

      if(current_ana[["ana_dsview"]] %in% ds_catalog[["object"]]){
        current_view_id= current_ana[["ana_dsview"]]
      } else {
        current_view_id = ds_catalog[["object"]][1]
        FM_le(state, paste0("ui_nca_curr_views: dataset view missing."   ))
        FM_le(state, paste0("ana_key: ",     current_ana[["key"]]       ))
        FM_le(state, paste0("ana_dsview: ",  current_ana[["ana_dsview"]]))
        FM_le(state, paste0("switching to view:", current_view_id ))
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
    })
    #------------------------------------
    # interval specification
    output$ui_nca_ana_int = renderUI({

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



      # Pulling out the current analysis to get the column information
      current_ana = NCA_fetch_current_ana(state)

      # getting the dataset
      dsview   = current_ana$ana_dsview
      col_time = current_ana$col_time
      time_vals = state[["NCA"]][["DSV"]][["ds"]][[dsview]][["DS"]][[col_time]]


      # If this is true then something has gone wrong
      # and we are unable to find the time column. In this
      # case we will return an error:
      if(is.null(time_vals)){
        uiele = "Time column not found"
      } else {

        # Here we include 0 in the time values for the user to choose from
        time_vals = sort(unique(c(0, time_vals)))

        # By default the selected are the current values for this analysis:
        selected = current_ana[["interval"]]

        # If any of the selected values are NA or _not_ found in
        # the time columns then we overwrite them here)
        if(any(is.na(selected)) |
           any(!(selected %in% time_vals))){

          # Now we just select the entire range:
          selected = c(min(time_vals), max(time_vals))
        }

        uiele =
          shinyWidgets::sliderTextInput(
            inputId   = NS(id, "slider_ana_interval"),
            label     = state[["MC"]][["labels"]][["ana_interval"]],
            width     = state[["MC"]][["formatting"]][["slider_ana_interval"]][["width"]],
            choices   = time_vals,
            selected  = selected
            )
      }




      uiele})
    #------------------------------------
    # NCA parameters to compute
    output$ui_nca_ana_params = renderUI({
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
    # NCA parameters to compute
    output$ui_nca_ana_source_sampling = renderUI({
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

           ## Creating the tool tip if shinyBS is installed
            tmp_tt = NULL
           #if(system.file(package="shinyBS") != ""){
           #  # Next we only include tooltips if they are enabled for this
           #  # module
           #  if(state[["MC"]][["tooltips"]][["include"]]){
           #    # If a tooltip is defined for this element then we attach it
           #    if(!is.null((nca_config[[cfg_ele]][["tooltip"]]))){
           #    tmp_tt =
           #      shinyBS::bsTooltip(
           #        id        = NS(id, nca_config[[cfg_ele]][["ui_id"]]),
           #        title     = nca_config[[cfg_ele]][["tooltip"]],
           #        placement = "right"
           #    )
           #    }
           #  }
           #}

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

    # This can be used to trigger notifications
   #toNotify <- reactive({
   #  list(input$A,
   #       input$B)
   #})
   #observeEvent(toNotify(), {
   #  state = NCA_fetch_state(id              = id,
   #                         input           = input,
   #                         session         = session,
   #                         FM_yaml_file    = FM_yaml_file,
   #                         MOD_yaml_file   = MOD_yaml_file,
   #                         id_ASM          = id_ASM,
   #                         id_UD           = id_UD,
   #                         id_DW           = id_DW,
   #                         react_state     = react_state)
   #
   #  # Triggering optional notifications
   #  notify_res =
   #  FM_notify(state   = state,
   #            session = session)
   #})
    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
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

        FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        state_list = list(
         checksum = state[["NCA"]][["checksum"]],
         ui       = state[["NCA"]][["ui"]]
        )
        react_state[[id]][["NCA"]] = state_list
      })
    }
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

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["NCA"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)
    #------------------------------------

  })
}

#'@export
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
#'@return list containing the current state of the app including default
#'values from the yaml file as well as any changes made by the user. The list
#'has the following structure:
#' \itemize{
#' \item{yaml:} Full contents of the supplied yaml file.
#' \item{MC:} Module components of the yaml file.
#' \item{NCA:}  # JMH populate the rest of this list
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the analysis was successfully run.
#'   \item{checksum:} This is an MD5 sum of the (JMH update) element and can be
#'   used to detect changes in the state.
#'   \item{key:           Analysis key acts as a title/caption (user editable)}
#'   \item{notes:         Analysis notes  (user editable)}
#'   \item{id:            Character id (\code{ana_idx})}
#'   \item{idx:           Numeric id (\code{1})}
#'   \item{nca_config:}   List of PKNCA configuration options for this analysis.
#'   \item{code:          Code to run the analysis from start to finish.}
#'   \item{code_previous: Code to load and/or wrangle the dataset.}
#'   \item{code_ana_only: Code to just to run the analysis}
#' }
#' \item{MOD_TYPE:} Character data containing the type of module \code{"NCA"}
#' \item{id:} Character data containing the module id module in the session variable.
#' \item{FM_yaml_file:} App configuration file with FM as main section.
#' \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#'}
NCA_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file, id_ASM, id_UD, id_DW, react_state){

  # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
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
    FM_le(state, "Updating DS")
    # updating the "DSV" components
    state[["NCA"]][["DSV"]] = FM_fetch_ds(state, session, c(id_UD, id_DW))
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
         FM_le(state, paste0("setting analysis: ", ana_name, " = ", paste(state[["NCA"]][["ui"]][[ui_name]], collapse=", ")))
      }

      current_ana[[ana_name]] = state[["NCA"]][["ui"]][[ui_name]]
    }

    # Storing any changes here:
    state = NCA_set_current_ana(state, current_ana)
  }

  # JMH process scenario button selection here to overwrite the
  # values in the current analysis


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
  # New Analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_new"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_new"]])){

    FM_le(state, "creating new analysis")
    msgs = c()

    # Creating a new analysis
    state = NCA_new_ana(state)

    # Setting hold for analysis select
    state = set_hold(state, inputId = "select_current_ana")
    state = set_hold(state, inputId = "select_current_view")

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_new"]] =
      state[["NCA"]][["ui"]][["button_ana_new"]]

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)
  }
  # Delete analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_del"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_del"]])){

    FM_le(state, "deleting analysis")
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
    state = FM_set_ui_msg(state, msgs)
  }
  # Save analysis
  if(has_changed(ui_val   = state[["NCA"]][["ui"]][["button_ana_save"]],
                 old_val  = state[["NCA"]][["button_counters"]][["button_ana_save"]])){

    FM_le(state, "saving changes to current analysis")

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

    # Saving the button state to the counter
    state[["NCA"]][["button_counters"]][["button_ana_save"]] =
      state[["NCA"]][["ui"]][["button_ana_save"]]

    # Updating any messages
    state = FM_set_ui_msg(state, msgs)

    # JMH consider this in the context of NCA
    ## Forcing a rebuild of the figure:
    #state = NCA_build( state=state, del_row = NULL, cmd = NULL)
  }
  #---------------------------------------------
  # Include units switch
 #if(has_changed(ui_val   = state[["NCA"]][["ui"]][["switch_ana_include_units"]],
 #               old_val  = state[["NCA"]][["button_counters"]][["switch_ana_include_units"]])){
 #
 #  # Pulling out the current analysis
 #  current_ana = NCA_fetch_current_ana(state)
 #
 #  # updating the switch
 #  current_ana[["include_units"]] = state[["NCA"]][["ui"]][["switch_ana_include_units"]]
 #
 #  # Updating the analysis in the state
 #  state = NCA_set_current_ana(state, current_ana)
 #
 #  FM_le(state, paste0("include units set to: ", current_ana[["include_units"]]))
 #
 #  # Updating the change tracking
 #  state[["NCA"]][["button_counters"]][["switch_ana_include_units"]] =
 #    state[["NCA"]][["ui"]][["switch_ana_include_units"]]
 #}
  #---------------------------------------------
  # Passing any messages back to the user
  state = FM_set_ui_msg(state, msgs)

  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}
#'@export
#'@title Initialize NCA Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param id_UD  ID string for the upload data module used to handle uploads or the name of the list element in react_state where the data set is stored.
#'@param id_DW  ID string for the data wrangling module to process any uploaded data
#'@return list containing an empty NCA state
NCA_init_state = function(FM_yaml_file, MOD_yaml_file,  id, id_UD, id_DW,  session){

  button_counters = c("button_ana_new",
                      "button_ana_del",
                      "button_ana_save",
                      "button_ana_copy",
                      "button_ana_use_scenario",
                      "switch_ana_include_units")

  # mapping name in UI to name in analysis
  ui_ana_map = list(
    "switch_ana_include_units"   = "include_units",
    "slider_ana_interval"        = "interval",
    "slider_ana_source_sampling" = "sampling",
    "select_ana_nca_parameters"  = "nca_parameters",
    "select_ana_scenario"        = "ana_scenario",
    "select_ana_units_time"      = "units_time",
    "select_ana_units_conc"      = "units_conc",
    "select_ana_units_dose"      = "units_dose",
    "select_ana_units_amt"       = "units_amt",
    "select_ana_col_id"          = "col_id"   ,
    "select_ana_col_time"        = "col_time" ,
    "select_ana_col_dose"        = "col_dose" ,
    "select_ana_col_conc"        = "col_conc" ,
    "select_ana_col_group"       = "col_group"
  )

  # We add all of the button counters as well as
  # the ui inputs specified in ui_ana_map above.
  # Then we append the other ui elements we want to
  # use in the state:
  ui_ids          = c(button_counters,
                      names(ui_ana_map),
                      "select_current_ana",
                      "select_current_view",
                      "text_ana_key",
                      "text_ana_notes")

  ui_hold         = c("select_current_ana",
                      "select_current_view")



  isgood          = TRUE

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
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


  # Now we recreate the initialized state with all of the ui_ids
  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    id              = id,
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


  # Finding the dataset
  DSV = FM_fetch_ds(state, session, c(id_UD, id_DW))

  # If the dataset isn't good then we need to
  # flag the whole module as not being good
  if(!DSV[["isgood"]]){
    isgood = FALSE
  }

  # Module-specific elements
  state[["NCA"]][["isgood"]]        = isgood
  state[["NCA"]][["isgood"]]        = isgood
  state[["NCA"]][["DSV"]]           = DSV
  state[["NCA"]][["anas"]]          = NULL
  state[["NCA"]][["ana_cntr"]]      = 0
  state[["NCA"]][["current_ana"]]   = NULL

  FM_le(state, "State initialized")

  if(isgood){
    # Initializing an empty analysis
    state = NCA_new_ana(state)
  }


  FM_le(state, "State initialized")
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
#'@description Description
#'@param state NCA state from \code{NCA_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link{formods::FM_generate_report}}.
#'@param rpttype Type of report to generate (supported "xlsx", "pptx", "docx").
#'@param gen_code_only Boolean value indicating that only code should be
#'generated (\code{FALSE}).
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}    Return status of the function.
#'  \item{hasrptele:} Boolean indicator if the module has any reportable elements.
#'  \item{code:}      Data wrangling R command.
#'  \item{msgs:}      Messages to be passed back to the user.
#'  \item{rpt:}       Report with any additions passed back to the user.
#'}
#'@seealso \code{\link{formods::FM_generate_report}}
NCA_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The NCA module only supports the following report types:
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
#'@title Fetch Module Datasets
#'@description Fetches the datasets contained in the model
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
NCA_fetch_ds = function(state){
  hasds  = FALSE
  isgood = TRUE
  msgs   = c()
  ds     = list()

  # Empty list for new datasets
  NEWDS = list(label      = NULL,
               MOD_TYPE   = NULL,
               id         = NULL,
               DS         = NULL,
               DSMETA     = NULL,
               code       = NULL,
               checksum   = NULL,
               DSchecksum = NULL)

  # This prevents returning a dataset if this is triggered before data has
  # been loaded
  if(state[["NCA"]][["isgood"]]){

    # Fill in the DS creation stuff here
    isgood = FALSE

    # Putting it all into the ds object to be returned
    ds[[object_name]] = NEWDS
  }

  res = list(hasds  = hasds,
             isgood = isgood,
             msgs   = msgs,
             ds     = ds)
res}


#'@export
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
    list(key             = nca_id,
         id              = nca_id,
         idx             = state[["NCA"]][["ana_cntr"]],
         nca_object_name = nca_object_name,
         nobj            = NULL,
         msgs            = c(),
         ana_dsview      = ana_dsview,
         nca_config      = state[["NCA"]][["nca_config"]][["default"]],
         checksum        = digest::digest(NULL, algo=c("md5")),
         ana_scenario    = "",
         code_ana_only   = NULL,
         code_previous   = NULL,
         code            = NULL,
         col_id          = "",          # The col_* values will be populated later
         col_conc        = "",
         col_dose        = "",
         col_time        = "",
         col_group       = "",
         include_units   = "",
         interval        = c(NA,NA),
         nca_parameters  = "",
         sampling        = "",
         units_conc      = "",
         units_dose      = "",
         units_amt       = "",
         units_time      = "",
         units_group     = "",
         notes           = "",
         isgood          = FALSE)


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
  nca_def[["col_dose"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["dose"]],
    dscols   = dscols)
  nca_def[["col_conc"]] = NCA_find_col(
    patterns = state[["MC"]][["detect_col"]][["conc"]],
    dscols   = dscols)

  # Setting the units switch for the analysis
  nca_def[["include_units"]]= state[["MC"]][["units"]][["include_units"]]

  # and also for the counters:
  state[["NCA"]][["button_counters"]][["switch_ana_include_units"]] =
      state[["MC"]][["units"]][["include_units"]]

  # add the default units
  nca_def[["units_conc"]] = state[["MC"]][["units"]][["conc_def"]]
  nca_def[["units_time"]] = state[["MC"]][["units"]][["time_def"]]
  nca_def[["units_dose"]] = state[["MC"]][["units"]][["dose_def"]]
  nca_def[["units_amt"]]  = state[["MC"]][["units"]][["amt_def"]]

  # Setting the new analysis id as the current analysis
  state[["NCA"]][["current_ana"]]    = nca_id

  # Setting scenario specific options for the default scenario
  ana_scenarios    = state[["MC"]][["ana_scenarios"]]
  ana_scenario_def = state[["MC"]][["ana_scenario_def"]]

  nca_def[["ana_scenario"]]   = ana_scenario_def
  nca_def[["nca_parameters"]] = ana_scenarios[[ana_scenario_def]][["parameters"]]
  nca_def[["sampling"]]       = ana_scenarios[[ana_scenario_def]][["sampling"]]


  # Storing the empty analysis object in the state
  state = NCA_set_current_ana(state, nca_def)

state}


#'@export
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
#'@description Takes the current value from the UI, an optional list of
#'patterns to search, column names from a dataset, and an optional list of
#'column names to exclude.
#'
#' - Excluded columns are removed from dscols
#'
#' - If curr_ui has a none NULL non "" value it is compared to dscols. If it
#'   is found there that value is returned.
#'
#' - If not then the patterns are considered. IF they are not null they are
#'   compared sequentially to the columns names. The first match found is
#'   returned.
#'
#' - If nothing is found then the first value of dscols is returned.
#'
#'@param curr_ui  Current value in UI
#'@param patterns List of regular expression patterns to consider.
#'@param dscols   Columns from the dataset.
#'@param excol    Columns to exclude from consideration (default: \code{NULL}).
#'@param null_ok  Logical value indicating if a null result (nothing found) is
#'       OK (default: \code{FALSE})
#'@return Value List containing the details of the current analysis. The structure
#'of this list is the same as the structure of \code{state$NCA$anas} in the output of
#'\code{ANA_fetch_state()}.
NCA_find_col             = function(curr_ui  = NULL,
                                    patterns = NULL,
                                    dscols,
                                    excol=NULL,
                                    null_ok = FALSE){

  value = NULL

  COL_FOUND = FALSE

  if(!is.null(excol)){
    browser()
    # Removing excol from dscols
  }

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



#'@export
#'@title Fetches Current Analysis
#'@description Takes an NCA state and returns the current active analysis
#'@param state NCA state from \code{NCA_fetch_state()}
#'@return List containing the details of the current analysis. The structure
#'of this list is the same as the structure of \code{state$NCA$anas} in the output of
#'\code{ANA_fetch_state()}.
NCA_fetch_current_ana    = function(state){

  # Current analysis ID
  ana_id = state[["NCA"]][["current_ana"]]

  # Current analysis
  ana = state[["NCA"]][["anas"]][[ana_id]]

ana}


#'@export
#'@title Fetches PKNCA Metadata
#'@description Compiles Metadata from PKNCA
#'@return List containing data frames of PKNCA meta data
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


