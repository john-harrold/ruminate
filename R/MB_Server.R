#'@import rhandsontable
#'@import shiny
#'@importFrom digest digest
#'@importFrom shinyAce aceEditor updateAceEditor

#'@export
#'@title Model Builder State Server
#'@description Server function for the Model Builder  Shiny Module
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM ID string for the app state managment module used to save and load app states
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param deployed Boolean variable indicating whether the app is deployed or not.
#'@param react_state Variable passed to server to allow reaction outside of module (\code{NULL})
#'@return MB Server object
# JMH Add example
MB_Server <- function(id,
               id_ASM        = "ASM",
               FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
               MOD_yaml_file = system.file(package = "ruminate",  "templates", "MB.yaml"),
               deployed      = FALSE,
               react_state   = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Generating the model selection catalog
    output$ui_select_model_catalog = renderUI({
      input$element_selection
      req(input[["base_from"]])
      uiele = NULL
      if(input[["base_from"]] == "catalog"){
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        # Extracting the model catalog:
        model_catalog = state[["MB"]][["model_catalog"]]

        current_ele = MB_fetch_current_element(state)

        if( model_catalog[["isgood"]]){

           # If there are multiple sources we display the items grouped by
           # source if there is only one source we use the plain method
           if(length(names(model_catalog[["select_group"]])) > 1){
             choices = model_catalog[["select_group"]]
           } else {
             choices = model_catalog[["select_plain"]]
           }

           choicesOpt = list(
            subtext = stringr::str_trunc(model_catalog[["select_subtext"]],
                                         width= state[["MC"]][["formatting"]][["catalog_selection"]][["truncate"]]
            )
           )

           uiele =
           shinyWidgets::pickerInput(
             selected   = current_ele[["ui"]][["catalog_selection"]],
             inputId    = NS(id, "catalog_selection"),
             label      = state[["MC"]][["labels"]][["catalog_selection"]],
             choices    = choices,
             width      = state[["MC"]][["formatting"]][["catalog_selection"]][["width"]],
             options    = list(
               size          = state[["yaml"]][["FM"]][["ui"]][["select_size"]],
               'live-search' =TRUE),
             choicesOpt = choicesOpt)

          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["catalog_selection"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["catalog_selection"]][["tooltip_position"]])

        } else {
          if(is.null( model_catalog[["msgs"]])){
            uiele = "Unable to extract catalog"
          } else {
            uiele =   model_catalog[["msgs"]]
          }
        }
      }

      uiele})
    #------------------------------------
    # Generating the timescale selection UI
    output$ui_select_time_scale    = renderUI({
        input[["element_name"]]
        input[["ui_mb_model"]]
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        current_ele = MB_fetch_current_element(state)

        choices = list()
        for(cname in names(state[["MC"]][["formatting"]][["time_scales"]][["choices"]])){
          choices[[ state[["MC"]][["formatting"]][["time_scales"]][["choices"]][[cname]][["verb"]] ]] =
            cname
        }

        uiele =
        shinyWidgets::pickerInput(
          selected   = current_ele[["ui"]][["time_scale"]],
          inputId    = NS(id, "time_scale"),
          label      = state[["MC"]][["labels"]][["time_scales"]],
          choices    = choices,
          width      = state[["MC"]][["formatting"]][["time_scales"]][["width"]])

        uiele = formods::FM_add_ui_tooltip(state, uiele,
          tooltip     = state[["MC"]][["formatting"]][["time_scales"]][["tooltip"]],
          position    = state[["MC"]][["formatting"]][["time_scales"]][["tooltip_position"]])
      uiele})
    #------------------------------------
    output$ui_upload_model_file = renderUI({
      req(input[["base_from"]])
      uiele = NULL
      if(input[["base_from"]] == "user"){
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        uiele = fileInput(NS(id, "uploaded_model"),
           width = state[["MC"]][["formatting"]][["upload_model_file"]][["width"]],
           label = state[["MC"]][["labels"]][["upload_model_file"]])
      }
      uiele})
    #------------------------------------
    output$ui_upload_model_type = renderUI({
      req(input[["base_from"]])
      uiele = NULL
      if(input[["base_from"]] == "user"){
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)
        choices = list()
        for(cname in names(state[["MC"]][["formatting"]][["model_type_selection"]][["choices"]])){
          choices[[ state[["MC"]][["formatting"]][["model_type_selection"]][["choices"]][[cname]] ]] =
            cname
        }

        uiele =
        shinyWidgets::pickerInput(
          selected   = state[["MB"]][["model_type_selection"]],
          inputId    = NS(id, "model_type_selection"),
          label      = state[["MC"]][["labels"]][["model_type_selection"]],
          choices    = choices,
          width      = state[["MC"]][["formatting"]][["model_type_selection"]][["width"]])

        uiele = formods::FM_add_ui_tooltip(state, uiele,
          tooltip     = state[["MC"]][["formatting"]][["model_type_selection"]][["tooltip"]],
          position    = state[["MC"]][["formatting"]][["model_type_selection"]][["tooltip_position"]])
      }
    uiele})
    #------------------------------------
    # Generating the UI to select model from catalog or user-defined model
    output$ui_select_base_from = renderUI({
      input[["element_name"]]
      input[["ui_mb_model"]]
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choices = list()
      for(cname in names(state[["MC"]][["formatting"]][["base_from"]][["choices"]])){
        choices[[ state[["MC"]][["formatting"]][["base_from"]][["choices"]][[cname]] ]] =
          cname
      }

      # JMH base_from should probably be stored in the current_element and
      # initialized that way
      current_ele = MB_fetch_current_element(state)

      uiele =
      shinyWidgets::radioGroupButtons(
        inputId    = NS(id, "base_from"),
       #selected   = state[["MB"]][["base_from"]],
        selected   = current_ele[["ui"]][["base_from"]],
        label      = state[["MC"]][["labels"]][["base_from"]],
        choices    = choices,
        status     = state[["MC"]][["formatting"]][["base_from"]][["status"]],
        checkIcon = list(
           yes = icon("ok",
           lib = "glyphicon"),
           no  = icon("remove",
           lib = "glyphicon"))
        )


      uiele = formods::FM_add_ui_tooltip(state, uiele,
        tooltip     = state[["MC"]][["formatting"]][["base_from"]][["tooltip"]],
        position    = state[["MC"]][["formatting"]][["base_from"]][["tooltip_position"]])
    uiele})
    #------------------------------------
    # Select the active model
    output$ui_select_element = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choices = list()
      for(element_id in names(state[["MB"]][["elements"]])){
        choices[[ state[["MB"]][["elements"]][[element_id]][["ui"]][["element_name"]] ]] = element_id
      }

      uiele =
      shinyWidgets::pickerInput(
        selected   = state[["MB"]][["current_element"]],
        inputId    = NS(id, "element_selection"),
        label      = state[["MC"]][["labels"]][["current_element"]],
        choices    = choices,
        width      = state[["MC"]][["formatting"]][["current_element"]][["width"]])

      uiele = formods::FM_add_ui_tooltip(state, uiele,
        tooltip     = state[["MC"]][["formatting"]][["current_element"]][["tooltip"]],
        position    = state[["MC"]][["formatting"]][["current_element"]][["tooltip_position"]])


      uiele})
    #------------------------------------
    # Current model name:
    output$ui_text_element_name = renderUI({
      input$element_selection
      input$catalog_selection
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_ele = MB_fetch_current_element(state)

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
    # Create an empty UI for the append model. It will update based on the
    # observe function below it.
    output$ui_select_append_model = renderUI({
      req(input[["element_selection"]])
      #input[["ui_mb_model"]]
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choicesOpt = NULL
      uiele =
        shinyWidgets::pickerInput(
          selected   = "PH",
          inputId    = NS(id, "append_model"),
          label      = state[["MC"]][["labels"]][["append_model"]],
          choices    = c(state[["MC"]][["formatting"]][["append_model"]][["no_models"]]),
          width      = state[["MC"]][["formatting"]][["append_model"]][["width"]],
          choicesOpt = choicesOpt)

      uiele})

    #------------------------------------
    # Model export buttons
    # NONMEM
    output$ui_mb_export_nonmem = renderUI({
      req(input[["element_selection"]])
      #input[["ui_mb_model"]]
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choicesOpt = NULL
      uiele = NULL
      if(state[["MB"]][["isgood"]]){
        uiele = shinyWidgets::downloadBttn(
                  outputId = NS(id, "export_nonmem"),
                  label    = state[["MC"]][["labels"]][["export_nonmem"]],
                  style    = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size     = state[["MC"]][["formatting"]][["export_nonmem"]][["size"]],
                  block    = state[["MC"]][["formatting"]][["export_nonmem"]][["block"]],
                  color    = state[["MC"]][["formatting"]][["export_nonmem"]][["color"]],
                  icon     = icon("arrow-down"))

          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["export_nonmem"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["export_nonmem"]][["tooltip_position"]])

      
      }

      uiele})
    output$export_nonmem     = downloadHandler(
      filename = function() {
        # This is the default filename the user is prompted with when the
        # download starts:
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)
        current_element = MB_fetch_current_element(state)
        dlfn = paste0(state[["MC"]][["element_object_name"]], "_", current_element[["idx"]], "-nonmem.zip")
        FM_le(state, paste0("pushing export: ", dlfn))
       dlfn},
      content = function(file) {
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)
        current_element = MB_fetch_current_element(state)
        component   = MB_fetch_component(state, current_element)

        FM_pause_screen(state   = state,
                        message = state[["MC"]][["labels"]][["export_pause"]],
                        session = session)


        ex_sub_dir = paste0(state[["MC"]][["element_object_name"]], "_", current_element[["idx"]])
        export_dir =  file.path(tempfile(pattern="dir"))
        if(!dir.exists(file.path(export_dir, ex_sub_dir))){
          dir.create(path=file.path(export_dir, ex_sub_dir), recursive = TRUE)
        }

        rtores = rx2other(object=component[["rx_obj"]],
                          out_type="nonmem",
                          export_path=file.path(export_dir, ex_sub_dir))

        # If there are any messages we dump those as well to be visible to the
        # user:
        if(!is.null(rtores[["msgs"]])){
          msgs_file = file.path(export_dir, ex_sub_dir, "export_messages.txt")
          file.create(msgs_file)
          write(rtores[["msgs"]], file=msgs_file)

          # Defining the notification type
          if(rtores[["isgood"]]){
            notification_type = "warning"
          } else {
            notification_type = "failure"
          }

          # Logging messages
          FM_le(state, rtores[["msgs"]])

          # Setting notification and saving the state.
          state = FM_set_notification(
            state       = state,
            notify_text = rtores[["msgs"]],
            notify_id   = "NONMEM export messages",
            type        = notification_type)
          FM_set_mod_state(session, id, state)
        }

        zip::zip(zipfile=file,
                 files               = dir(export_dir),
                 recurse             = TRUE,
                 root                = export_dir,
                 include_directories = TRUE)


        FM_resume_screen(state   = state,
                         session = session)
        }
    )
    # Monolix
    output$ui_mb_export_monolix = renderUI({
      req(input[["element_selection"]])
      #input[["ui_mb_model"]]
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)
      choicesOpt = NULL
      uiele = NULL
      if(state[["MB"]][["isgood"]]){
        uiele = shinyWidgets::downloadBttn(
                  outputId = NS(id, "export_monolix"),
                  label    = state[["MC"]][["labels"]][["export_monolix"]],
                  style    = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size     = state[["MC"]][["formatting"]][["export_monolix"]][["size"]],
                  block    = state[["MC"]][["formatting"]][["export_monolix"]][["block"]],
                  color    = state[["MC"]][["formatting"]][["export_monolix"]][["color"]],
                  icon     = icon("arrow-down"))

          uiele = formods::FM_add_ui_tooltip(state, uiele,
            tooltip     = state[["MC"]][["formatting"]][["export_monolix"]][["tooltip"]],
            position    = state[["MC"]][["formatting"]][["export_monolix"]][["tooltip_position"]])

      
      }

      uiele})
    output$export_monolix    = downloadHandler(
      filename = function() {
        # This is the default filename the user is prompted with when the
        # download starts:
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)
        current_element = MB_fetch_current_element(state)
        dlfn = paste0(state[["MC"]][["element_object_name"]], "_", current_element[["idx"]], "-monolix.zip")
        FM_le(state, paste0("pushing export: ", dlfn))
       dlfn},
      content = function(file) {
        state = MB_fetch_state(id              = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)
        current_element = MB_fetch_current_element(state)
        component   = MB_fetch_component(state, current_element)

        FM_pause_screen(state   = state,
                        message = state[["MC"]][["labels"]][["export_pause"]],
                        session = session)


        ex_sub_dir = paste0(state[["MC"]][["element_object_name"]], "_", current_element[["idx"]])
        export_dir =  file.path(tempfile(pattern="dir"))
        if(!dir.exists(file.path(export_dir, ex_sub_dir))){
          dir.create(path=file.path(export_dir, ex_sub_dir), recursive = TRUE)
        }

        rtores = rx2other(object=component[["rx_obj"]],
                          out_type="monolix",
                          export_path=file.path(export_dir, ex_sub_dir))

        # If there are any messages we dump those as well to be visible to the
        # user:
        if(!is.null(rtores[["msgs"]])){
          msgs_file = file.path(export_dir, ex_sub_dir, "export_messages.txt")
          file.create(msgs_file)
          write(rtores[["msgs"]], file=msgs_file)

          # Defining the notification type
          if(rtores[["isgood"]]){
            notification_type = "warning"
          } else {
            notification_type = "failure"
          }

          # Logging messages
          FM_le(state, rtores[["msgs"]])

          # Setting notification and saving the state.
          state = FM_set_notification(
            state       = state,
            notify_text = rtores[["msgs"]],
            notify_id   = "Monolix export messages",
            type        = notification_type)
          FM_set_mod_state(session, id, state)
        }

        zip::zip(zipfile=file,
                 files               = dir(export_dir),
                 recurse             = TRUE,
                 root                = export_dir,
                 include_directories = TRUE)


        FM_resume_screen(state   = state,
                         session = session)
        }
    )


    #------------------------------------
    # Updates append_model selection based on the current model
    observe({
      req(input[["append_model"]])
      req(input[["element_selection"]])
      req(input[["ui_mb_model"]])
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_element = MB_fetch_current_element(state)

      appendable_models = MB_fetch_appends(state, current_element)

      if(appendable_models[["isgood"]]){
        choices    = appendable_models[["select_plain"]]
        choicesOpt = appendable_models[["choicesOpt"]]
      } else {
        choices    = c(state[["MC"]][["errors"]][["fetch_appends_failed"]])
        choicesOpt = NULL
        FM_le(state, appendable_models[["msgs"]])
      }

      shinyWidgets::updatePickerInput(
        session    = session,
        inputId    = "append_model",
        choices    = choices,
        choicesOpt = choicesOpt)

    })
    #------------------------------------
    # Generated model
    observe({
      req(input[["element_selection"]])
      input[["time_scale"]]
      input[["catalog_selection"]]
      input[["button_clk_save"]]
      input[["button_clk_del"]]
      input[["button_clk_append_model"]]
      input[["uploaded_model"]]

      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_element = MB_fetch_current_element(state)
      component       = MB_fetch_component(state, current_element)

      if(current_element[["update_model_code"]]){
        if(component[["isgood"]]){
          uiele = component[["fcn_def"]]
        } else {
          uiele = paste0("# ", state[["MC"]][["errors"]][["no_model_found"]])
        }


        # Syncing the ui contents with the tracked ui value
        current_element[["ui"]][["ui_mb_model"]] = uiele
        current_element[["update_model_code"]]   = FALSE
        state = MB_set_current_element(
          state   = state,
          element = current_element)
        FM_set_mod_state(session, id, state)

        shinyAce::updateAceEditor(
          session         = session,
          editorId        = "ui_mb_model",
          theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
          showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
          readOnly        = FALSE,
          mode            = "r",
          value           = uiele)
      }

    })
    #------------------------------------
    # Generated model building code
    observe({
      req(input[["element_selection"]])
      input[["ui_mb_model"]]
      input[["time_scale"]]
      input[["catalog_selection"]]
      input[["button_clk_save"]]
      input[["button_clk_del"]]
      input[["button_clk_append_model"]]
      input[["uploaded_model"]]


      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      current_element = MB_fetch_current_element(state)
      component       = MB_fetch_component(state, current_element)

      if(component[["isgood"]]){
        uiele = component[["model_code_sa"]]
      } else {
        uiele = paste0("# ", state[["MC"]][["errors"]][["no_model_found"]])
      }


      shinyAce::updateAceEditor(
        session         = session,
        editorId        = "ui_mb_code",
        theme           = state[["yaml"]][["FM"]][["code"]][["theme"]],
        showLineNumbers = state[["yaml"]][["FM"]][["code"]][["showLineNumbers"]],
        readOnly        = state[["MC"]][["code"]][["readOnly"]],
        mode            = state[["MC"]][["code"]][["mode"]],
        value           = uiele)

    })
    #------------------------------------
    # Side buttons:
    # new
    output$ui_mb_new_btn = renderUI({
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
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
    output$ui_mb_save_btn = renderUI({
      state = MB_fetch_state(id        = id,
                             id_ASM          = id_ASM,
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
    # clip code
    output$ui_mb_clip_code = renderUI({
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
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
    output$ui_mb_del_btn   = renderUI({
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
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
    output$ui_mb_copy_btn   = renderUI({
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
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
    # append model
    output$ui_mb_append_model_btn   = renderUI({
      req(input[["element_selection"]])
      req(input[["ui_mb_model"]])
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


      current_element   = MB_fetch_current_element(state)
      appendable_models = MB_fetch_appends(state, current_element)

      uiele = NULL

      # Only when appendable models exist
      if(appendable_models[["hasappends"]]){
        uiele = shinyWidgets::actionBttn(
                  inputId = NS(id, "button_clk_append_model"),
                  label   = state[["MC"]][["labels"]][["append_model_btn"]],
                  style   = state[["yaml"]][["FM"]][["ui"]][["button_style"]],
                  size    = state[["MC"]][["formatting"]][["button_clk_append_model"]][["size"]],
                  block   = state[["MC"]][["formatting"]][["button_clk_append_model"]][["block"]],
                  color   = "primary",
                  icon    = icon("paperclip"))
       
        # Optinally adding the tooltip:
        uiele = formods::FM_add_ui_tooltip(state, uiele,
                 tooltip             = state[["MC"]][["formatting"]][["append_model"]][["tooltip"]],
                 position    = state[["MC"]][["formatting"]][["append_model"]][["tooltip_position"]])
      }


      uiele =    div(style=paste0("width:", state[["MC"]][["formatting"]][["append_model"]][["width"]]), 
                     uiele)

      uiele})
    #------------------------------------
    # User messages:
    output$ui_mb_msg = renderText({
      input[["button_clk_append_model"]]
      input[["element_name"]]
      input[["time_scale"]]
      input[["ui_mb_model"]]
      input[["button_clk_save"]]
      input[["uploaded_model"]]
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      uiele = state[["MB"]][["ui_msg"]]

      uiele})
    # Creates the ui for the compact view of the module
    #------------------------------------
    # Compact ui
    output$MB_ui_compact  =  renderUI({
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

     if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
        uiele_code_button = NULL
        # Generating code button if enabled
        if( state[["MC"]][["compact"]][["code"]]){
          uiele_code = tagList(shinyAce::aceEditor(
            NS(id, "ui_mb_code"),
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
             status  = "danger btn-custom-mb",
             icon    = icon("code", lib="font-awesome"),
             tooltip = shinyWidgets::tooltipOptions(title = state[["MC"]][["tooltips"]][["show_code"]]))
          )

        }

        # Button with MB elements table
        uiele_mb_elements_button = NULL
     ### Uncomment this if your model has a components table
     ###uiele_mb_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_mb_elements"))
     ###uiele_mb_elements_button = tagList(
     ### shinyWidgets::dropdownButton(
     ###   uiele_mb_elements,
     ###   inline  = FALSE,
     ###   right   = TRUE ,
     ###   size    = "sm",
     ###   circle  = FALSE,
     ###   status  = "primary btn-custom-mb",
     ###   icon    = icon("layer-group", lib="font-awesome"),
     ###   tooltip = tooltipOptions(title = state[["MC"]][["tooltips"]][["elements"]]))
     ###)

        uiele = tagList(
          div(style="display:inline-block", htmlOutput(NS(id, "ui_select_element"))),
          div(style="display:inline-block", htmlOutput(NS(id, "ui_text_element_name"))),
        # tags$br(),
        # div(style="display:inline-block", htmlOutput(NS(id, "ui_element_notes"))),
          tags$br(),
          div(style="display:inline-block", verbatimTextOutput(NS(id, "ui_mb_msg"))),
          tags$br()
        )

        # We only show the clip button if it's enabled
        uiele_clip_button = NULL
        if(state[["MC"]][["compact"]][["clip"]]){
          uiele_clip_button = htmlOutput(NS(id, "ui_mb_clip_code"))
        }

        uiele_buttons_right = tagList(
                 tags$style(".btn-custom-mb {width: 100px;}"),
                 div(style="display:inline-block;vertical-align:top;height:100px",
                 uiele_mb_elements_button,
                 uiele_code_button,
                 uiele_clip_button,
                 htmlOutput(NS(id, "ui_mb_save_btn")),
                 htmlOutput(NS(id, "ui_mb_copy_btn")),
                 htmlOutput(NS(id, "ui_mb_del_btn")),
                 htmlOutput(NS(id, "ui_mb_new_btn")),
                 htmlOutput(NS(id, "ui_mb_export_nonmem")),
                 htmlOutput(NS(id, "ui_mb_export_monolix"))
                 ))

        # Appending the preview
        div_style = paste0("display:inline-block;vertical-align:top;",
          "width:",   state[["MC"]][["formatting"]][["preview"]][["width"]],  ";",
          "height: ", state[["MC"]][["formatting"]][["preview"]][["height"]])
        uiele_preview = div(style=div_style,
             shinyAce::aceEditor(NS(id, "ui_mb_model"),
               height = state[["MC"]][["formatting"]][["preview"]][["height"]]
                                 ))
        uiele = tagList(
          uiele,
          tags$h3(state[["MC"]][["labels"]][["head_model_code"]], icon_link(href=state[["MC"]][["tooltips"]][["url_rxode"]])),
          uiele_preview,
          uiele_buttons_right,
          tags$br()
        )


        uiele = tagList( uiele,
          tags$br(),
          fluidRow(
          column(7,
            tags$h3(state[["MC"]][["labels"]][["head_base_model"]]),
            div(style="display:inline-block", htmlOutput(NS(id, "ui_select_base_from"))),
            div(style="display:inline-block", htmlOutput(NS(id, "ui_upload_model_type"))),
            div(style="display:inline-block", icon_link(href=state[["MC"]][["tooltips"]][["url_model_types"]])),
            tags$br(),
            div(style="display:inline-block", htmlOutput(NS(id, "ui_upload_model_file"))),
            div(style="display:inline-block", htmlOutput(NS(id, "ui_select_model_catalog"))),
          ),
          column(5,
          tags$h3(state[["MC"]][["labels"]][["head_time_scale"]]),
          htmlOutput(NS(id, "ui_select_time_scale"))
          )
          ),
          fluidRow(
          column(4,
            div(style="display:inline-block", 
                htmlOutput(NS(id, "ui_select_append_model")),
                htmlOutput(NS(id, "ui_mb_append_model_btn")))),
          column(8,NULL)
          )
        )
      } else {
        uiele = NULL
         if( Sys.getenv("ruminate_rxode2_found")=="FALSE"){
           uiele = tagList(uiele, "rxode2 package was not found.", tags$br())
         }
         if( Sys.getenv("ruminate_nonmem2rx_found")=="FALSE"){
           uiele = tagList(uiele, "nonmem2rx package was not found.", tags$br())
         }
         if( Sys.getenv("ruminate_nlmixr2lib_found")=="FALSE"){
           uiele = tagList(uiele, "nlmixr2lib package was not found.", tags$br())
         }
      }

      uiele
    })

    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
           # react_state[[id_ASM]])
             input[["element_selection"]],
             input[["time_scale"]],
             input[["catalog_selection"]],
             input[["button_clk_new"]],
             input[["button_clk_del"]],
             input[["button_clk_copy"]],
             input[["button_clk_save"]])
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = MB_fetch_state(id        = id,
                               id_ASM          = id_ASM,
                               input           = input,
                               session         = session,
                               FM_yaml_file    = FM_yaml_file,
                               MOD_yaml_file   = MOD_yaml_file,
                               react_state     = react_state)

        FM_le(state, "reaction state updated")
        #react_state[[id]] = state
        react_state[[id]][["MB"]][["checksum"]] = state[["MB"]][["checksum"]]
      }, priority=99)
    }
    #------------------------------------
    # Copying element code to the clipboard
    observeEvent(input$button_clk_clip, {
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
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
          current_element = MB_fetch_current_element(state)
          component       = MB_fetch_component(state, current_element)
          if(component[["isgood"]]){
            uiele = component[["model_code_sa"]]
          } else {
            uiele = paste0("# ", state[["MC"]][["errors"]][["no_model_found"]])
          }

          clipr::write_clip(uiele)
        }
    })

    #------------------------------------
    # This can be used to trigger notifications
    toNotify <- reactive({
      list(
       # JMH figure out how to trigger when the export buttons are clicked
       input[["export_nonmem"]],
       input[["export_monolix"]],
       input[["element_selection"]],
       input[["catalog_selection"]],
       input[["uploaded_model"]],
       input[["button_clk_append_model"]],
       input[["button_clk_save"]],
       input[["button_clk_copy"]],
       input[["button_clk_del"]],
       input[["button_clk_new"]]
      )
    })
    observeEvent(toNotify(), {
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
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
             input$button_clk_new,
           # input$button_clk_del,
           # input$button_clk_copy,
           # input$button_clk_save,
             input$element_selection,
             input$catalog_selection,
             input$current_element)
      })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = MB_fetch_state(id              = id,
                             id_ASM          = id_ASM,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      FM_le(state, "removing holds")
      # Removing all holds
      for(hname in names(state[["MB"]][["ui_hold"]])){
        remove_hold(state, session, hname)
      }
    }, priority = -100)
    #------------------------------------


  })
}

#'@export
#'@title Fetch Model Builder State
#'@description Merges default app options with the changes made in the UI
#'@param id Shiny module ID
#'@param id_ASM ID string for the app state management module used to save and load app states
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
#' \item{MB:}
#' \itemize{
#'   \item{isgood:} Boolean object indicating if the file was successfully loaded.
#'   \item{checksum:} This is an MD5 sum of the contents element and can be
#'   used to detect changes in the state.
#' }
#'  \item{MOD_TYPE:} Character data containing the type of module \code{"MB"}
#'  \item{id:} Character data containing the module id module in the session variable.
#'  \item{FM_yaml_file:} App configuration file with FM as main section.
#'  \item{MOD_yaml_file:}  Module configuration file with MC as main section.
#' }
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = MB_test_mksession(session=list(), full_session=FALSE)
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "ruminate", "templates", "MB.yaml")
#'
#' # Creating an empty state object
#' state = MB_fetch_state(id              = "MB",
#'                        id_ASM          = "ASM",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
MB_fetch_state = function(id, id_ASM, input, session, FM_yaml_file, MOD_yaml_file, react_state){

    # Template for an empty dataset
  #---------------------------------------------
  # Getting the current state
  state = FM_fetch_mod_state(session, id)
  # If the state has not yet been defined then we
  # initialize it
  if(is.null(state)){
    # General state information
    state = MB_init_state(FM_yaml_file, MOD_yaml_file, id, session)
  }

  #---------------------------------------------
  # Here we update the state based on user input
  for(ui_name in state[["MB"]][["ui_ids"]]){
    if(!is.null(isolate(input[[ui_name]]))){
       state[["MB"]][["ui"]][[ui_name]] = isolate(input[[ui_name]])
     } else {
       if(ui_name %in% names(state[["MB"]][["button_counters"]])){
         state[["MB"]][["ui"]][[ui_name]] = 0
       } else {
         state[["MB"]][["ui"]][[ui_name]] = ""
       }

       # initializing the previous ui values as well:
       if(is.null(state[["MB"]][["ui_old"]][[ui_name]])){
         state[["MB"]][["ui_old"]][[ui_name]] = state[["MB"]][["ui"]][[ui_name]]
       }
     }
   }

  # Starting out with no messages
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
  current_ele = MB_fetch_current_element(state)
  # There are scenarios where you wouldn't want to do this. Like when
  # switching elements in the ui. You would need to add some logic to
  # only update below conditionally.

  for(ui_name in state[["MB"]][["ui_ids"]]){
    if(!fetch_hold(state, ui_name)){
      if(ui_name %in% names(state[["MB"]][["button_counters"]])){
        # Button changes are compared to the button click tracking values
        change_detected =
          has_updated(ui_val    = state[["MB"]][["ui"]][[ui_name]],
                      old_val   = state[["MB"]][["button_counters"]][[ui_name]],
                      init_val  = c("", "0"))

        if(change_detected){
          formods::FM_le(state, paste0("button click: ", ui_name, " = ", state[["MB"]][["ui"]][[ui_name]]))

          # Saving the change:
          state[["MB"]][["button_counters"]][[ui_name]] =
            state[["MB"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)

          # Flagging the ui as initialized
          state[["MB"]][["ui_ids_init"]][[ui_name]] = TRUE
        }
      }else{
        change_detected =
          has_updated(ui_val    = state[["MB"]][["ui"]][[ui_name]],
                      old_val   = state[["MB"]][["ui_old"]][[ui_name]],
                      init_val  = c(""))

        if(change_detected){
          formods::FM_le(state, paste0("setting model: ", ui_name, " = ", paste(state[["MB"]][["ui"]][[ui_name]], collapse=", ")))

          # Saving the change:
          state[["MB"]][["ui_old"]][[ui_name]] = state[["MB"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)

          # Flagging the ui as initialized
          state[["MB"]][["ui_ids_init"]][[ui_name]] = TRUE

          # This also updates the current element if that ui_name is part of
          # an element
          if(ui_name %in% state[["MB"]][["ui_ele"]]){
            formods::FM_le(state, paste0("element updated: ", ui_name))
            current_ele[["ui"]][[ui_name]] = state[["MB"]][["ui"]][[ui_name]]
          }
        }
      }
    }
  }
  # Updating the element with any changes:
  state = MB_set_current_element(
    state   = state,
    element = current_ele)
  #---------------------------------------------
  # Here we react to changes between the UI and the current state
  # save model
  if("button_clk_save" %in% changed_uis){
    FM_le(state, "save model")
    current_ele = MB_fetch_current_element(state)

    current_ele[["ui"]][["element_name"]] =
      state[["MB"]][["ui"]][["element_name"]]

    if(current_ele[["ui"]][["ui_mb_model"]]  !=
      state[["MB"]][["ui"]][["ui_mb_model"]]){

      FM_pause_screen(
          state   = state,
          session = session,
          message = state[["MC"]][["labels"]][["building_model"]])

      # Rebuilding the model uses the rxode2 output
      mk_rx_res = mk_rx_obj(
        type="rxode2",
        model = list(
                     fcn_def = paste0("fcn_obj = ", state[["MB"]][["ui"]][["ui_mb_model"]]),
                     fcn_obj = "fcn_obj"))

      FM_resume_screen(state, session)

      if(mk_rx_res[["isgood"]]){
        # Pulling out the current output
        current_ele = MB_fetch_current_element(state)

        note_str    = "Manual update"

        current_ele = MB_update_model(
          state       = state,
          session     = session,
          current_ele = current_ele,
          rx_obj      = mk_rx_res[["capture"]][["rx_obj"]],
          note        = note_str,
          reset       = FALSE)

        state = FM_set_notification(
          state       = state,
          notify_text = note_str,
          notify_id   = "Manual model update done",
          type        = "success")
        # MB_update_model
        # state[["MB"]][["ui"]][["ui_mb_model"]]
        state = MB_set_current_element(
          state   = state,
          element = current_ele)

        FM_le(state, note_str)

      }else{
        state = FM_set_notification(
          state       = state,
          notify_text = state[["MC"]][["errors"]][["manual_model_update_failed"]],
          notify_id   = "Manual update failed",
          type        = "failure")

        msgs = c(msgs,
                 state[["MC"]][["errors"]][["manual_model_update_failed"]],
                 mk_rx_res[["msgs"]])

        FM_le(state, state[["MC"]][["errors"]][["manual_model_update_failed"]])
        FM_le(state, mk_rx_res[["msgs"]])
      }

    }
  }
  #---------------------------------------------
  # clip model
  if("button_clk_clip" %in% changed_uis){
    FM_le(state, "clip model")
  }
  #---------------------------------------------
  # time scale changes
  if("time_scale" %in% changed_uis){

    # Pulling out the current element and corresponding component
    current_ele = MB_fetch_current_element(state)
    component   = MB_fetch_component(state, current_ele)

    # We only update the model if there is an object. This prevents updates
    # during initialization. 
    if(!is.null(component[["rx_obj"]])){
      FM_le(state, "time scale changed")
      current_ele = MB_update_model(
        state       = state,
        session     = session,
        current_ele = current_ele,
        rx_obj      = component[["rx_obj"]],
        note        = "Updated time scale",
        reset       = FALSE)
    }
    
    # Saving the updated element
    state = MB_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # copy model
  if("button_clk_copy" %in% changed_uis){
    FM_le(state, "copy model")

    # First we pull out the current element:
    old_ele = MB_fetch_current_element(state)

    # Now we create a new element and make it the current element
    state   = MB_new_element(state)
    new_ele = MB_fetch_current_element(state)

    # This is a list of UI elements to skip when copying:
    ui_copy_skip = c("element_name")

    # Here we copy all the ui elements from old to new skipping those flagged
    # for skipping.
    for(tmp_ui_name in names(new_ele[["ui"]])){
      if(!(tmp_ui_name %in% ui_copy_skip)){
        new_ele[["ui"]][[tmp_ui_name]]  = old_ele[["ui"]][[tmp_ui_name]]
      }
    }

    model_comps = MB_fetch_component(state, old_ele)

    # This is a list of fields to copy from old to new:
    ele_copy = c("code_previous",
                 "selected_component_id",
                 "components_list",
                 "components_table",
                 "model_fcn")
    for(ele_name in ele_copy){
      new_ele[[ele_name]]  = old_ele[[ele_name]]
    }


    # Rebuilding the appropriate columns in the components_table
    for(tmp_id_str in new_ele[["components_table"]][["id_str"]]){

      # Current component values
      tmp_id = new_ele[["components_table"]][new_ele[["components_table"]][["id_str"]] == tmp_id_str][["id"]]
      component = MB_fetch_component(
                    state        = state,
                    current_ele  = new_ele,
                    component_id = tmp_id)

      # rebuilding the code around the function for the new element
      tmp_fcn_def = component[["fcn_def"]]

      bcres =
      MB_build_code(
        state        = state,
        session      = session,
        fcn_def      = tmp_fcn_def,
        time_scale   = new_ele[["ui"]][["time_scale"]],
        fcn_obj_name = new_ele[["fcn_obj_name"]] ,
        rx_obj_name  = new_ele[["rx_obj_name"]]  ,
        ts_obj_name  = new_ele[["ts_obj_name"]]  )

      new_ele[["components_table"]][new_ele[["components_table"]][["id_str"]] == tmp_id_str][["ts_code"]] =
        paste0(bcres[["ts_code"]], collapse="\n")
      new_ele[["components_table"]][new_ele[["components_table"]][["id_str"]] == tmp_id_str][["model_code"]] =
        paste0(bcres[["model_code"]], collapse="\n")
      new_ele[["components_table"]][new_ele[["components_table"]][["id_str"]] == tmp_id_str][["model_code_sa"]] =
        paste0(bcres[["model_code_sa"]], collapse="\n")
    }


    # Updating the model in the state:
    state = MB_set_current_element(
      state   = state,
      element = new_ele)
  }
  #---------------------------------------------
  # del model
  if("button_clk_del" %in% changed_uis){
    FM_le(state, "delete model")
    state = MB_del_current_element(state)
  }
  #---------------------------------------------
  # selected model changed
  if("element_selection" %in% changed_uis){
    state[["MB"]][["current_element"]] =
       state[["MB"]][["ui"]][["element_selection"]]

    # Forces the ui to update the model code
    current_ele = MB_fetch_current_element(state)
    current_ele[["update_model_code"]] = TRUE
    state = MB_set_current_element(
      state   = state,
      element = current_ele)
    # Setting the hold for all the other UI elements
    state = set_hold(state)
  }
  #---------------------------------------------
  # Appending model
  if("button_clk_append_model" %in% changed_uis){
    FM_le(state, "append model")

    # The counter is getting reset to zero and triggering a second append that
    # fails. This will prevent that from happening
    if(as.numeric(state[["MB"]][["ui"]][["button_clk_append_model"]]) > 0){
      current_ele = MB_fetch_current_element(state)
      component   = MB_fetch_component(state, current_ele)
      
      FM_pause_screen(
          state   = state,
          session = session,
          message = state[["MC"]][["labels"]][["building_model"]])
      
      
      # Getting the model to be appended:
      mod_id    = state[["MB"]][["ui"]][["append_model"]]
      mod_sum   = state[["MB"]][["model_catalog"]][["summary"]]
      mod_sum   = mod_sum[mod_sum[["mod_id"]] == mod_id, ]
      app_fun   = mod_sum[["Model"]][1]
      app_obj   = mod_sum[["Object"]][1]
      
      fun_cmd = c(app_fun,
                  paste0("rx_obj = rxAppendModel(rx_obj, ",  app_obj, ")"))
      
      tc_res = formods::FM_tc(
        capture="rx_obj", 
        cmd    = fun_cmd,
        tc_env = list(rx_obj=component[["rx_obj"]]))
      
      if(tc_res[["isgood"]]){
      
        # Updating the model
        current_ele = MB_update_model(
          state       = state,
          session     = session,
          current_ele = current_ele,
          rx_obj      = tc_res[["capture"]][["rx_obj"]],
          note        = "Append sub-model",
          reset       = FALSE)
      
        # Saving the updated element
        state = MB_set_current_element(
          state   = state,
          element = current_ele)
      
      } else {
      
        # logging the error
        formods::FM_le(state, state[["MC"]][["errors"]][["append_failed"]])
        if(!is.null(tc_res[["msgs"]])){
          formods::FM_le(state, tc_res[["msgs"]])
        }
      
        # Notifying the user
        msgs  = c(msgs,
                  state[["MC"]][["errors"]][["append_failed"]],
                  tc_res[["msgs"]]) 
      
        state = FM_set_notification(
          state       = state,
          notify_text = state[["MC"]][["errors"]][["append_failed"]],
          notify_id   = "Append failed",
          type        = "failure")
      
      }
      FM_resume_screen(state, session)
    }

    
  }
  #---------------------------------------------
  # model catalog selection changed, new button selected
  if(any(c("button_clk_new", "catalog_selection") %in% changed_uis)){

    #---------------------------------------------
    # new model was clicked so we create a new empty model
    # and it will be set as the current element:
    if("button_clk_new" %in% changed_uis){
      FM_le(state, "new model")
      state = MB_new_element(state)
    }

    # This will overwrite the currently selected base model so
    # it will delete the model chain:

    # Pulling out the current output
    current_ele = MB_fetch_current_element(state)

    # This is the model catalog
    all_models = state[["MB"]][["model_catalog"]]

    # This is the row with the model
    model_row  =
      all_models[["summary"]][
      all_models[["summary"]][["mod_id"]] == current_ele[["ui"]][["catalog_selection"]], ]

    # By default we update the base model
    update_basemodel = TRUE

    # Now we check the current element to see if the base model is the same as
    # what is currently selected. If they are the same then the detected base
    # model change results from switching elements and we don't need to update
    # anything:
    if(current_ele[["base_model"]] != ""){
      if(current_ele[["base_model"]] == current_ele[["ui"]][["catalog_selection"]]){
        update_basemodel=FALSE
      }
    }

    if(update_basemodel){
      note_str    = paste0("base model: ", model_row[["Name"]])

      FM_pause_screen(
          state   = state,
          session = session,
          message = state[["MC"]][["labels"]][["building_model"]])


      # Here we build the base model depending on the input model type.
      # The capture variables after building should be:
      # mk_rx_res$capture$fun_obj - rxode2 function object
      # mk_rx_res$capture$rx_obj - rxode2 object of the model
      if(model_row[["Type"]][1] == "rxode2"){
        mk_rx_res = mk_rx_obj(
          type="rxode2",
          model = list(fcn_def = model_row[["Model"]][1],
                       fcn_obj = model_row[["Object"]][1]))
      }

      if(model_row[["Type"]][1] == "NONMEM"){
        mk_rx_res = mk_rx_obj(
          type="NONMEM",
          model = list(model_file = model_row[["Model"]][1])
         )
      }

      FM_resume_screen(state, session)


      if(mk_rx_res[["isgood"]]){
        current_ele = MB_update_model(
          state       = state,
          session     = session,
          current_ele = current_ele,
          rx_obj      = mk_rx_res[["capture"]][["rx_obj"]],
          note        = note_str,
          reset       = TRUE)

        # saving the base model information for the current model
        current_ele[["base_model"]]      =  current_ele[["ui"]][["catalog_selection"]]
        current_ele[["base_model_name"]] =  model_row[["Name"]]

        # Holding elements to prevent update from current ui
        state = set_hold(state)

        state = FM_set_notification(
          state       = state,
          notify_text = paste0("base model: ", model_row[["Name"]] ),
          notify_id   = "creating base model",
          type        = "success")

      }else{
        state = FM_set_notification(
          state       = state,
          notify_text = state[["MC"]][["errors"]][["base_model_build_failed"]],
          notify_id   = "creating base model",
          type        = "failure")

        msgs = c(msgs, mk_rx_res[["msgs"]])
      }

      # Updating the model in the state:
      state = MB_set_current_element(
        state   = state,
        element = current_ele)
    }
  }
  #---------------------------------------------
  # model upload detected
  if(any(c("uploaded_model") %in% changed_uis)){

    model_type = state[["MB"]][["ui"]][["model_type_selection"]]
    model_file = state[["MB"]][["ui"]][["uploaded_model"]]

    FM_pause_screen(
        state   = state,
        session = session,
        message = state[["MC"]][["labels"]][["building_model"]])

    if(model_type == "rxode2"){
      fcn_def = paste(readLines(model_file[["datapath"]]), collapse="\n")
      fcn_obj = "my_fcn"
      mk_rx_res = mk_rx_obj(
        type="rxode2",
        model = list(fcn_def = fcn_def,
                     fcn_obj = fcn_obj))
    }

    if(model_type == "NONMEM"){
      mk_rx_res = mk_rx_obj(
        type="NONMEM",
        model = list(model_file = model_file[["datapath"]])
       )
    }

    FM_resume_screen(state, session)

    if(mk_rx_res[["isgood"]]){
      # Pulling out the current output
      current_ele = MB_fetch_current_element(state)

      note_str    = paste0("User-file: ", model_file[["name"]])

      current_ele = MB_update_model(
        state       = state,
        session     = session,
        current_ele = current_ele,
        rx_obj      = mk_rx_res[["capture"]][["rx_obj"]],
        note        = note_str,
        reset       = FALSE)

      state = FM_set_notification(
        state       = state,
        notify_text = note_str,
        notify_id   = "User-file upload",
        type        = "success")

      state = MB_set_current_element(
        state   = state,
        element = current_ele)

      FM_le(state, note_str)

    }else{
      state = FM_set_notification(
        state       = state,
        notify_text = state[["MC"]][["errors"]][["user_file_upload_failed"]],
        notify_id   = "User-file upload failed",
        type        = "failure")

      msgs = c(msgs,
               state[["MC"]][["errors"]][["user_file_upload_failed"]],
               mk_rx_res[["msgs"]])

      FM_le(state, state[["MC"]][["errors"]][["user_file_upload_failed"]])
      FM_le(state, mk_rx_res[["msgs"]])
    }

  }

  # Triggering save messages:
  if(any(c("element_name", "ui_mb_model") %in% changed_uis)){
    change_detected = FALSE
    if("element_name" %in% changed_uis){
      change_detected = FALSE
      if(current_ele[["ui"]][["element_name"]]  !=
        state[["MB"]][["ui"]][["element_name"]]){
        msgs = c(msgs, state[["MC"]][["labels"]][["element_name_diff"]])
        change_detected = TRUE
      }
    }
    if("ui_mb_model" %in% changed_uis){
      if(current_ele[["ui"]][["ui_mb_model"]]  !=
        state[["MB"]][["ui"]][["ui_mb_model"]]){
        msgs = c(msgs, state[["MC"]][["labels"]][["model_code_diff"]])
        change_detected = TRUE
      }
    }

    if(change_detected){
      msgs = c(msgs, state[["MC"]][["labels"]][["save_change_detected"]])
    }
  }
  #---------------------------------------------
  # Passing any messages back to the user
  if(!is.null(changed_uis)){
    state = FM_set_ui_msg(state, msgs)
  }

  #---------------------------------------------
  # Saving the state
  FM_set_mod_state(session, id, state)

  # Returning the state
  state}

#'@export
#'@title Initialize MB Module State
#'@description Creates a list of the initialized module state
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
#'@param id ID string for the module.
#'@param session Shiny session variable
#'@return list containing an empty MB state
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = MB_test_mksession(session=list(), full_session=FALSE)
#' session = sess_res$session
#' input   = sess_res$input
#'
#' state = MB_init_state(
#'    FM_yaml_file  = system.file(package = "formods",
#'                                "templates",
#'                                "formods.yaml"),
#'    MOD_yaml_file = system.file(package = "ruminate",
#'                                "templates",
#'                                "MB.yaml"),
#'    id              = "MB",
#'    session         = session)
#'
#' state
MB_init_state = function(FM_yaml_file, MOD_yaml_file,  id, session){


  button_counters = c("button_clk_save",
                      "button_clk_clip",
                      "button_clk_del",
                      "button_clk_copy",
                      "button_clk_new",
                      "button_clk_append_model"
                      )
  # These are the module ui elements that are associated with
  # the current element
  ui_ele          = c("catalog_selection",
                      "base_from",
                      "element_name",
                      "time_scale")

  # This contains all of the relevant ui_ids in the module
  ui_ids          = c(button_counters, ui_ele,
                      "model_type_selection",
                      "ui_select_element",
                      "ui_mb_model",
                      "model_type_selection",
                      "uploaded_model",
                      "append_model",
                      "element_selection")

  # Making all the ui_ids holdable
  ui_hold         = ui_ids



  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
    dep_mod_ids     = c(),
    id              = id,
    MT              = "MB",
    button_counters = button_counters,
    ui_ids          = ui_ids,
    ui_hold         = ui_hold,
    session         = session)

  # Storing the ui_ids for the elements
  state[["MB"]][["ui_ele"]]               = ui_ele

  # This tracks elements for the module
  state[["MB"]][["code_previous"]]        = NULL
  state[["MB"]][["elements"]]             = NULL
  state[["MB"]][["current_element"]]      = NULL
  state[["MB"]][["element_cntr"]]         = 0
  state[["MB"]][["model_type_selection"]] =
    state[["MC"]][["formatting"]][["model_type_selection"]][["default"]]



  #------------------------------------
  # Checking for rxpackages
  # If all the suggested packages are found this will be true:
  state[["MB"]][["suggested"]][["found"]]        =  TRUE

  pkgs = c("rxode2", "nonmem2rx", "nlmixr2lib")
  for(pkg in pkgs){
    if(!is_installed(pkg)){
      state[["MB"]][["suggested"]][["pkgs"]][[pkg]][["found"]] =  FALSE
      state[["MB"]][["suggested"]][["pkgs"]][[pkg]][["msg"]]   =  paste0(pkg, " package was not found.")
      state[["MB"]][["suggested"]][["found"]]                  =  FALSE
      # this is a temp file created to make sure that notifications have only
      # been issued once
      pkg_file = file.path(tempdir(), paste0("MB_pkg_not_found_", pkg))

      if(!file.exists(pkg_file)){
        FM_message(paste0("The package ", pkg, " is not installed"), entry_type="warning")
        file.create(pkg_file)
      }
    } else {
      state[["MB"]][["suggested"]][["pkgs"]][[pkg]][["found"]] =  TRUE
      state[["MB"]][["suggested"]][["pkgs"]][[pkg]][["msg"]]   =  ""
    }
  }
  #------------------------------------
  # Pulling out the model sources
  state[["MB"]][["model_catalog"]]        =  MB_fetch_catalog(state)

  # Creating the time scales details
  ts_details = list()
  for(tmpts in names(state[["MC"]][["formatting"]][["time_scales"]][["choices"]])){
    tmp_conv = eval(parse(text=state[["MC"]][["formatting"]][["time_scales"]][["choices"]][[tmpts]][["conv"]]))
    tmp_verb = state[["MC"]][["formatting"]][["time_scales"]][["choices"]][[tmpts]][["verb"]]
    ts_details[[tmpts]] = list(verb = tmp_verb, conv=tmp_conv)
  }
  state[["MB"]][["ts_details"]] = ts_details

  # Creating a default element:
  state = MB_new_element(state)

  state = MB_update_checksum(state)

  FM_le(state, "State initialized")
state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state MB state from \code{MB_fetch_state()}
#'@return Character object vector with the lines of code
#'@example inst/test_apps/MB_funcs.R
MB_fetch_code = function(state){

  cmds = c()

  enames = names(state[["MB"]][["elements"]])
  if(length(enames) > 0){
    for(ename in enames){
      current_element = state[["MB"]][["elements"]][[ename]]
      component       = MB_fetch_component(state, current_element)
      cmds            = c(cmds, paste0("# Base model: ", current_element[["base_model_name"]], "===="))
      cmds            = c(cmds, component$model_code)
      cmds            = c(cmds, "\n")
    }
    cmds = c("# Model Building ----", cmds)
  } else {
    cmds = "# No models available"
  }

  code_str = paste0(cmds, collapse="\n")

code_str}

#'@export
#'@title Append Report Elements
#'@description Appends report elements to a formods report.
#'@param state MB state from \code{MB_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for
#' \code{\link[formods]{FM_generate_report}}
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
MB_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The MB module only supports the following report types:
  supported_rpttypes = c("docx")

  if(rpttype %in% supported_rpttypes){
    enames = names(state[["MB"]][["elements"]])
    if(length(enames) > 0){
      # This will hold objects for the try catch environment later:
      tc_env = list()
      for(ename in enames){
        element = state[["MB"]][["elements"]][[ename]]
        if(element[["isgood"]]){
          hasrptele = TRUE

          component       = MB_fetch_component(state, element)
          tc_env[[ element[["fcn_obj_name"]] ]] =eval(parse(text=component[["fcn_def"]]))
          
          code = c(code,
                paste0('# Inserting header with model description'),
                       'rpt = onbrand::report_add_doc_content(rpt,',
                       '        type     = "text",',
                       '        content  = list(',
                paste0('          text            =  ',deparse( element[["ui"]][["element_name"]]),','),
                paste0('          style           = "Heading_2"))'),
                "",
                paste0('# Inserting the model code'),
                paste0('fcn_lines = deparse(', element[["fcn_obj_name"]], ')'),
                       'for(tmp_line in fcn_lines){',
                       '  rpt = onbrand::report_add_doc_content(rpt,',
                       '          type     = "text",',
                       '          content  = list(',
                paste0('            text            =  tmp_line,'),
                paste0('            style           = "Code"))'),
                       '}',
                "")
        }
      }
    }
  }

  code = paste0(code, collapse="\n")
  if(hasrptele & !gen_code_only){
    tc_env[["rpt"]] = rpt
    tc_res = formods::FM_tc(capture="rpt", cmd=code, tc_env = tc_env)
    if(tc_res[["isgood"]]){
      rpt = tc_res[["capture"]][["rpt"]]
    } else {
      formods::FM_le(state, "Failed to add report element: ")
      if(!is.null(tc_res[["msgs"]])){
        formods::FM_le(state, tc_res[["msgs"]])
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
#'@title Fetch Model Builder Module Models
#'@description Fetches the models contained in the module.
#'@param state MB state from \code{MB_fetch_state()}
#'@return list containing the following elements
#'\itemize{
#'  \item{isgood:}      Return status of the function.
#'  \item{hasmdl:}      Boolean indicator if the module has any models
#'  \item{msgs:}        Messages to be passed back to the user.
#'  \item{mdl:}         List with models. Each list element has the name of
#'  the R-object for that dataset. Each element has the following structure:
#'  \itemize{
#'    \item{label:}       Text label for the model (e.g. one-compartment model).
#'    \item{MOD_TYPE:}    Type of module.
#'    \item{id:}          Module ID.
#'    \item{rx_obj:}      The rxode2 object.
#'    \item{rx_obj_name:} The rxode2 object name that holds the model.
#'    \item{ts_obj}       List with elements system and details 
#'    \item{ts_obj_name:} The object name that holds the model time scale information.
#'    \item{fcn_def:}     Text to define the model
#'    \item{MDLMETA:}     Notes about the model.
#'    \item{code:}        Code to generate the model.
#'    \item{checksum:}    Module checksum.
#'    \item{MDLchecksum:} Model checksum.
#'  }
#'}
#'@examples
#'# We need a module state:
#'sess_res = MB_test_mksession(session=list(), full_session=FALSE)
#'state = sess_res$state
#'
#'mdls = MB_fetch_mdl(state)
#'
#'names(mdls)
MB_fetch_mdl = function(state){

  hasmdl      = FALSE
  isgood      = TRUE
  msgs        = c()
  mdl         = list()

  # General timescale information 
  ts_details = state[["MB"]][["ts_details"]]

  # This prevents returning a dataset if this is triggered before data has
  # been loaded
  if(state[["MB"]][["isgood"]]){

    # Checksum for the module
    m_checksum = state[["MB"]][["checksum"]]
    elements = names(state[["MB"]][["elements"]])
    if(!is.null(elements)){
      # We have at least 1 model
      hasmdl = TRUE
      for(element in elements){
        # current element
        ce = state[["MB"]][["elements"]][[element]]
        ce_checksum = ce[["checksum"]]

        # current component of the current element
        cc = MB_fetch_component(state, ce)

        # Saving the model
        mdl[[ ce[["rx_obj_name"]] ]] =
          list(label       = ce[["ui"]][["element_name"]],
               MOD_TYPE    = "MB",
               id          = state[["id"]],
               ts_details  = ts_details,
               rx_obj      = cc[["rx_obj"]],
               rx_obj_name = ce[["rx_obj_name"]],
               ts_obj      = cc[["ts_obj"]],
               ts_obj_name = ce[["ts_obj_name"]],
               fcn_def     = cc[["fcn_def"]],
               MDLMETA     = cc[["note"]],
               code        = cc[["model_code"]],
               checksum    = m_checksum,
               MDLchecksum = ce_checksum)
      }
    }
  } else {
    isgood = FALSE
    msgs = c(msgs, "Bad MB state")
  }

  res = list(hasmdl      = hasmdl,
             isgood      = isgood,
             ts_details  = ts_details,
             msgs        = msgs,
             mdl         = mdl)
res}

#'@export
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@param id_ASM An ID string that corresponds with the ID used to call the ASM module
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
#' sess_res = MB_test_mksession(session=list(), full_session=FALSE)
MB_test_mksession = function(session, id = "MB", id_ASM="ASM", full_session=TRUE){


  isgood = TRUE
  rsc    = list()
  input  = list()
  state  = list()

  # Configuration files
  FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "ruminate", "templates", "MB.yaml")

  # Creating an empty state object
  state = MB_fetch_state(id              = id,
                         id_ASM          = id_ASM,
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         react_state     = NULL)

  if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
    # This will provide a list of the available models
    models = MB_fetch_catalog(state)

    #-------------------------------------------------------
    # Simplest model:
   #ridx = which(models[["summary"]][["Name"]] == "PK_1cmt_des")
   #model_row  = models[["summary"]][ridx, ]


    tmp_Object          = "my_model"
    tmp_base_model      = "manual"
    tmp_base_model_name = "manual"
    tmp_Model =  ' my_model = function () 
     {
         description <- "One compartment PK model with linear clearance using differential equations"
         ini({
             lka <- 0.45
             label("Absorption rate (Ka)")
             lcl <- 1
             label("Clearance (CL)")
             lvc <- 3.45
             label("Central volume of distribution (V)")
             propSd <- c(0, 0.5)
             label("Proportional residual error (fraction)")
             etalcl ~ 0.1
         })
         model({
             ka <- exp(lka)
             cl <- exp(lcl + etalcl)
             vc <- exp(lvc)
             kel <- cl/vc
             d/dt(depot) <- -ka * depot
             d/dt(central) <- ka * depot - kel * central
             Cc <- central/vc
             Cc ~ prop(propSd)
         })
     }'

    mk_rx_res = mk_rx_obj(
      type="rxode2",
      model = list(fcn_def = tmp_Model,       # model_row[["Model"]][1],
                   fcn_obj = tmp_Object))     # model_row[["Object"]][1]))

    current_ele = MB_fetch_current_element(state)
    current_ele = MB_update_model(
      state       = state,
      session     = session,
      current_ele = current_ele,
      rx_obj      = mk_rx_res[["capture"]][["rx_obj"]],
      note        = "base model",
      reset       = TRUE)

    current_ele[["ui"]][["element_name"]] = "One compartment model"
    current_ele[["ui"]][["base_from"]]    = "user"

    current_ele[["base_model"]]      =  tmp_base_model      # model_row[["mod_id"]][1]
    current_ele[["base_model_name"]] =  tmp_base_model_name # model_row[["Name"]][1]

    # Synching the catlog selection with the base model
    current_ele[["ui"]][["catalog_selection"]] = tmp_base_model # model_row[["mod_id"]][1]

    current_ele[["ui"]][["time_scale"]] = "hours"

    # Timescales are found here:
    # state[["MC"]][["formatting"]][["time_scales"]][["choices"]]

    state = MB_set_current_element(
      state   = state,
      element = current_ele)

    #-------------------------------------------------------
    if(full_session){
      # New element
      state = MB_new_element(state)

      # Building the element around the PK/biomarker test system in the
      # package
      example_file =  system.file(package="ruminate", "test_apps", "test_rxode2_system.R")

      fcn_def = readChar(example_file, file.info(example_file)$size)
      fcn_obj = "my_model"

      mk_rx_res = mk_rx_obj(
        type="rxode2",
        model = list(fcn_def = fcn_def,
                     fcn_obj = fcn_obj))

      current_ele = MB_fetch_current_element(state)
      current_ele = MB_update_model(
        state       = state,
        session     = session,
        current_ele = current_ele,
        rx_obj      = mk_rx_res[["capture"]][["rx_obj"]],
        note        = "base model",
        reset       = TRUE)

      current_ele[["base_model"]]           =  "user" #model_row[["mod_id"]][1]
      current_ele[["base_model_name"]]      =  "user" # model_row[["Name"]][1]
      current_ele[["ui"]][["element_name"]] = "PK Biomarker"
      current_ele[["ui"]][["base_from"]]    = "user"

      # Synching the catlog selection with the base model
      current_ele[["ui"]][["catalog_selection"]] =  "mod_1" # model_row[["mod_id"]][1]

      current_ele[["ui"]][["time_scale"]] = 'days'

      state = MB_set_current_element(
        state   = state,
        element = current_ele)
    }

    # JMH when loading ruminate with the session populated it the Model code
    # isn't switching correctly between the elements

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
  } else {
    isgood = FALSE
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
#'@title New Model Building Model
#'@description Appends a new empty model to the MB state object
#'and makes this new model the active model.
#'@param state MB state from \code{MB_fetch_state()}
#'@return MB state object containing a new model and that
#'model is set as the current active model. See the help for
#'\code{MB_fetch_state()} for model format.
#'@example inst/test_apps/MB_funcs.R
MB_new_element = function(state){

  # Incrementing the element counter
  state[["MB"]][["element_cntr"]] = state[["MB"]][["element_cntr"]] + 1

  # Creating a default element ID
  element_id = paste0("element_", state[["MB"]][["element_cntr"]])

  # Creating the object names for this element
  fcn_obj_name  = paste0(state[["MC"]][["element_object_name"]],
                    "_", state[["MB"]][["element_cntr"]], "_fcn")
  rx_obj_name   = paste0(state[["MC"]][["element_object_name"]],
                    "_", state[["MB"]][["element_cntr"]], "_rx")
  ts_obj_name   = paste0(state[["MC"]][["element_object_name"]],
                    "_", state[["MB"]][["element_cntr"]], "_ts")
  # Extracting the model catalog:
  model_catalog = state[["MB"]][["model_catalog"]]

  # Default for a new element:
  element_def =
    list(
         # internal use only
         isgood                 = TRUE,
         ui                     =
           list(
                ui_mb_model          = "",
                time_scale           = state[["MC"]][["formatting"]][["time_scales"]][["default"]],
                element_name         = paste0("Model ", state[["MB"]][["element_cntr"]]),
                catalog_selection    = model_catalog[["summary"]][1, "mod_id"],
                base_from            = state[["MC"]][["formatting"]][["base_from"]][["default"]]
                ),
         id                     = element_id,
         idx                    = state[["MB"]][["element_cntr"]],
         fcn_obj_name           = fcn_obj_name,
         rx_obj_name            = rx_obj_name,
         ts_obj_name            = ts_obj_name,
         msgs                   = c(),
         code_previous          = NULL,
         update_model_code      = FALSE,
         # user facing
         # This is used if you build the element in a layering method sort of
         # like how the ggplot figures in the FG module builds using different
         # ggplot commands (layers).
         components_table       = data.frame(),
         selected_component_id  = NULL,
         components_list        = list(),
         # Generated on save
         checksum               = "",
         base_model_name        = "",
         base_model             = "")

  # This contains the code to generate the input dataset
  code_previous = ""
  element_def[["code_previous"]] = code_previous

  # Dropping the new element into the state
  state[["MB"]][["elements"]][[element_id]] = element_def

  # updating the checksum for the current element
  state[["MB"]][["elements"]][[element_id]][["checksum"]] = digest::digest(element_def, algo=c("md5"))

  # Setting the new element as current
  state[["MB"]][["current_element"]]     = element_id

state}


#'@export
#'@title Update MB Module Checksum
#'@description Takes a MB state and updates the checksum used to trigger
#'downstream updates
#'@param state MB state from \code{MB_fetch_state()}
#'@return MB state object with the checksum updated
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = MB_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # We also need a state variable
#' state = sess_res$state
#'
#' state = MB_update_checksum(state)
MB_update_checksum     = function(state){

  # checksum string
  chk_str = ""

  # We'll concatinate all the individual checksums together
  # and create a checksum of those:
  element_ids = names(state[["MB"]][["elements"]])
  for(element_id in element_ids){
    # We trigger updates when the element changes:
    chk_str = paste0(chk_str, ":", state[["MB"]][["elements"]][[element_id]][["checksum"]])
  }

  old_chk = state[["MB"]][["checksum"]]
  new_chk = digest::digest(chk_str, algo=c("md5"))

  if(has_updated(old_chk, new_chk)){
    state[["MB"]][["checksum"]] = new_chk
    FM_le(state, paste0("module checksum updated:", state[["MB"]][["checksum"]]))
  }

state}


#'@export
#'@title Fetches Current model
#'@description Takes a MB state and returns the current active
#'model object.
#'@param state MB state from \code{MB_fetch_state()}
#'@return List containing the details of the active data view. The structure
#'of this list is the same as the structure of \code{state$MB$elements} in the output of
#'\code{MB_fetch_state()}.
#'@example inst/test_apps/MB_funcs.R
MB_fetch_current_element    = function(state){

  element_id = state[["MB"]][["current_element"]]

  current_element = state[["MB"]][["elements"]][[element_id]]

current_element}


#'@export
#'@title Sets the Value for the  Current model
#'@description Takes a MB state and returns the current active
#'model
#'@param state MB state from \code{MB_fetch_state()}
#'@param element Element list from \code{MB_fetch_current_element()}
#'@return MB state object with the current model set using the
#'supplied value.
#'@example inst/test_apps/MB_funcs.R
MB_set_current_element    = function(state, element){

  element_id = state[["MB"]][["current_element"]]

  # updating the checksum for the current element:
  tmp_ele = element
  tmp_ele[["checksum"]]  = ""
  tmp_checksum  = digest::digest(tmp_ele, algo=c("md5"))
  if(has_updated(element[["checksum"]], tmp_checksum)){
    FM_le(state, paste0("model checksum updated: ", tmp_checksum))
    element[["checksum"]]  = tmp_checksum
  }

  # this saves the element
  state[["MB"]][["elements"]][[element_id]] = element

  # This will update the checksum for the module
  state = MB_update_checksum(state)

state}

#'@export
#'@title Deletes Current model
#'@description Takes a MB state and deletes the current model.
#'If that is the last element, then a new default will be added.
#'@param state MB state from \code{MB_fetch_state()}
#'@return MB state object with the current model deleted.
#'@example inst/test_apps/MB_funcs.R
MB_del_current_element    = function(state){

  # We need the current element and corresponding ID
  current_element = MB_fetch_current_element(state)
  element_id = current_element[["id"]]

  # This deletes the current element ID
  state[["MB"]][["elements"]][[element_id]] = NULL

  if(length(names(state[["MB"]][["elements"]])) == 0){
    # This is triggered when we've deleted the last element,
    # So now we will create a new one that will be active:
    state = MB_new_element(state)
  } else {
    # If there is at least one left, we pull off the first
    # one and make that active:
    element_id = names(state[["MB"]][["elements"]])[1]
    state[["MB"]][["current_element"]] = element_id
  }

state}


#'@export
#'@title Updates Current Element with rxode2 Model
#'@description Takes an rxode2 object and updates the model components of the
#'current element.
#'@param state MB state from \code{MB_fetch_state()}
#'@param session Shiny session variable
#'@param current_ele MB model element from \code{MB_fetch_current_element()}
#'@param rx_obj rxode2 model from \code{rxode2::rxode2()}
#'@param note   text indicating what this update does (e.g. "added parameter")
#'@param reset  boolean indicating that the element needs to be reset (i.e. if
#'you change the base model) default: \code{FALSE}.
#'@return current_element with model attached
#'@example inst/test_apps/MB_funcs.R
MB_update_model   = function(state, session, current_ele, rx_obj, note, reset=FALSE){

  # We default to good
  isgood     = TRUE

  # Any checks of the rx_obj can be made here:
  # XXX

  if( Sys.getenv("ruminate_rxfamily_found")){
    if(isgood){
      # This will reset the current model 
      if(reset){
        # Zeros out the components table
        current_ele[["components_table"]] = data.frame()
        # Updating the default time units for the model
        current_ele[["ui"]][["time_scale"]] = state[["MC"]][["formatting"]][["time_scales"]][["default"]]
        if(!is.null(rx_obj$meta$units)){
          if("time" %in% names(rx_obj$meta$units)){
            # updating the timescale from the value specified in the models
            # meta data. This will look at the time units and compare them to
            # the valid matches for that timescale in the MB.yaml file. If
            # it's there it will set the time scale to the current that short 
            # name (ts_sn)
            for(ts_sn in names(state[["MC"]][["formatting"]][["time_scales"]][["choices"]])){
              if(rx_obj$meta$units$time %in% state[["MC"]][["formatting"]][["time_scales"]][["choices"]][[ts_sn]][["match"]]){
                current_ele[["ui"]][["time_scale"]] = ts_sn
              }
            }
          }
        }
      }

      # String for creating model function in R
      cmd = 'fcn_def    = paste0(deparse(as.function(rx_obj$fun)), collapse="\n")'
      tcres = 
        FM_tc(cmd     = cmd,
              tc_env  = list(rx_obj=rx_obj),
              capture = c("fcn_def"))

      if(tcres[["isgood"]]){
        fcn_def = tcres[["capture"]][["fcn_def"]]

        if(nrow(current_ele[["components_table"]]) == 0){
          component_id = 1
        }else{
          component_id = max(current_ele[["components_table"]][["id"]]) + 1
        }

        component_id_str = paste0("component_", component_id)

        bcres =
          MB_build_code(state        = state,
                        session      = session,
                        fcn_def      = fcn_def,
                        time_scale   = current_ele[["ui"]][["time_scale"]],
                        fcn_obj_name = current_ele[["fcn_obj_name"]],
                        rx_obj_name  = current_ele[["rx_obj_name"]],
                        ts_obj_name  = current_ele[["ts_obj_name"]])

        # Pulling out the time scale code and building the object
        tcres_ts = 
          FM_tc(cmd     = bcres[["ts_code"]],
                tc_env  = list(),
                capture = c( current_ele[["ts_obj_name"]]))
        ts_obj = tcres_ts[["capture"]][[ current_ele[["ts_obj_name"]] ]]
        
        tmpdf =
        data.frame(id            = component_id,
                   id_str        = component_id_str,
                   note          = note,
                   ts_code       = paste0(bcres[["ts_code"]],    collapse="\n"),
                   model_code    = paste0(bcres[["model_code"]],    collapse="\n"),
                   model_code_sa = paste0(bcres[["model_code_sa"]], collapse="\n"),
                   fcn_def       = fcn_def)


        if(is.null(current_ele[["components_table"]])){
          current_ele[["components_table"]] = tmpdf
        }else{
          current_ele[["components_table"]] = rbind(
            current_ele[["components_table"]],
            tmpdf)
        }

        # Saving the rxode2 object. The component ID is saved as a string
        # "component_N":
        current_ele[["components_list"]][[component_id_str]][["rx_obj"]] = rx_obj
        current_ele[["components_list"]][[component_id_str]][["ts_obj"]] = ts_obj

        # Setting the added component as the selected id:
        current_ele[["selected_component_id"]]  = component_id
      } else {
        isgood = FALSE
        FM_le(state, "MB_update_model() failed", entry_type="danger")
        FM_le(state, tcres[["msgs"]], entry_type="danger")
      }
    }
  } else {
    isgood = FALSE
  }

  # Triggering the update of the model code in the editor
  current_ele[["update_model_code"]] = TRUE
  # Updating the element status
  current_ele[["isgood"]] = isgood

current_ele}

#'@export
#'@title Fetch Selected Current Model Component
#'@description Fetches the selected component of the provided model.
#'@param state MB state from \code{MB_fetch_state()}
#'@param current_ele MB model element from \code{MB_fetch_current_element()}
#'@param component_id The numeric component id to select (default \code{NULL})
#'will return the selected ID.
#'@return list with the current component with the following attributes
#'\itemize{
#'  \item{isgood:} Boolean object indicating success.
#'  \item{rx_obj:} rxode2 object for the model.
#'  \item{ts_obj:} timescale object for the model.
#'  \item{fcn_def:} Just the model function definition.
#'  \item{note:} Note field from the components_table
#'  \item{model_code:} Code to generate model.
#'  \item{model_code_sa:} Stand-alone code to generate model with
#'  \item{msgs:}      Messages to be passed back to the user.
#'}
#'@example inst/test_apps/MB_funcs.R
MB_fetch_component = function(state, current_ele, component_id = NULL){

  # Default outputs
  isgood        = TRUE
  msgs          = c()
  rx_obj        = NULL
  ts_obj        = NULL
  fcn_def       = ""
  note          = ""
  model_code    = ""
  model_code_sa = ""

  if(is.null(component_id)){
    component_id    = current_ele[["selected_component_id"]]
  }

  comp_row  = current_ele[["components_table"]][current_ele[["components_table"]] == component_id, ]
  comp_list = current_ele[["components_list"]][[paste0("component_", component_id)]]

  if(is.null(comp_list)){
    isgood = FALSE
    msgs   = c(msgs,
    state[["MC"]][["errors"]][["selected_id_bad_list"]],
    paste0("list element: component_", component_id))
  }

  if(nrow(comp_row) != 1){
    isgood = FALSE
    msgs   = c(msgs,
    state[["MC"]][["errors"]][["selected_id_bad_row"]],
    paste0("rows: ", nrow(comp_row)))
  }


  if(isgood){
    rx_obj         = comp_list[["rx_obj"]]
    ts_obj         = comp_list[["ts_obj"]]
    fcn_def        = comp_row[["fcn_def"]]
    note           = comp_row[["note"]]
    model_code     = comp_row[["model_code"]]
    model_code_sa  = comp_row[["model_code_sa"]]
  }

  component = list(
    isgood         = isgood,
    rx_obj         = rx_obj,
    ts_obj         = ts_obj,
    fcn_def        = fcn_def,
    note           = note,
    model_code     = model_code,
    model_code_sa  = model_code_sa,
    msgs           = msgs
  )

component}

#'@export
#'@title Build Code to Generate Model
#'@description Takes the function definition from an rxode object, a function
#'object name and an rxode object name and creates the code to build those
#'objects.
#'@param state MB state from \code{MB_fetch_state()}
#'@param session Shiny session variable
#'@param fcn_def Character string containing the function definition for the
#'model
#'@param time_scale  Short name for the model timescale (see names of state$MC$formatting$time_scales$choices).
#'@param fcn_obj_name Object name of the function to create.
#'@param rx_obj_name Object name of the rxode2 object to create.
#'@param ts_obj_name Object name of the tiemscale object to create.
#'@return List with the following elements
#'\itemize{
#'  \item{model_code} Block of code to create the model in the context of a
#'  larger script.
#'  \item{model_code_sa} Same as the \code{model_code} element but meant to
#'  stand alone.
#'}
#'@example inst/test_apps/MB_funcs.R
MB_build_code  = function(state, session, fcn_def, time_scale, fcn_obj_name, rx_obj_name, ts_obj_name){

  if( Sys.getenv("ruminate_rxode2_found")){

    # Creating the time scale details
    ts_details = state[["MB"]][["ts_details"]]

    ts_code = c(
      paste0(ts_obj_name,  " = list("),
      paste0("  system  = ", deparse(time_scale), ","),
      paste0("  details = "),
      paste0("    ", deparse(ts_details), collapse="\n"),
      ")"
      )



    model_code = c(paste0(fcn_obj_name, " = ", fcn_def),
                   "",
                   paste0(rx_obj_name,  " =  rxode2::rxode2(", fcn_obj_name,")"),
                   "",
                   ts_code
                   )
  } else {
    model_code  = "# rxode2 package was not found."
  }

  deps          = FM_fetch_deps(state = state, session = session)
  model_code_sa = c(deps[["package_code"]],
                   "",
                   model_code)
  mc = list(
    model_code    = model_code,
    ts_code       = ts_code,
    model_code_sa = model_code_sa)

mc}

#'@export
#'@title Fetches List of Available Models
#'@description Creates a catalog of the models available in the system file.
#'@param state MB state from \code{MB_fetch_state()}
#'@return List with the following attributes:
#'\itemize{
#'  \item{summary:} Dataframe with a summary of the models in the catlog
#'  \item{sources:} Same information a that found in the summary table but in
#'  list form.
#'  \item{select_group:} List with the models grouped by source.
#'  \item{select_plain:} Flat list with the models (ungrouped).
#'  \item{select_subtext:} Subtext for pulldown menus.
#'  \item{msgs:} Messages to be passed back to the user.
#'  \item{hasmdl:} Boolean value indicating if any models were found.
#'  \item{isgood:} Boolean variable indicating success or failure.
#'}
#'@example inst/test_apps/MB_funcs.R
MB_fetch_catalog   = function(state){

  isgood         = TRUE
  msgs           = c()
  model_summary  = NULL
  select_subtext = c()
  select_group   = list()
  select_plain   = list()

  # looking for packages to use conditionally below
  found_nlmixr2lib = Sys.getenv("ruminate_nlmixr2lib_found")

  mod_idx  = 1
  mod_srcs = state[["MC"]][["sources"]]

  if(length(mod_srcs)==0){
    isgood = FALSE
    msgs   = c(msgs, state[["MC"]][["labels"]][["catalog_empty"]])
  }

  if(isgood){
    for(src_idx in 1:length(mod_srcs)){

      # This contains the current model source
      mod_src = mod_srcs[[src_idx]][["source"]]

      #---------------------------------------
      # Appends all of the nlmixr2lib models
      if(mod_src[["type"]] == "nlmixr2lib"){
        if(found_nlmixr2lib == "TRUE"){
          for(ridx in 1:nrow(nlmixr2lib::modeldb)){
            model_row = nlmixr2lib::modeldb[ridx, ]

            full_filename  = system.file(package="nlmixr2lib", "modeldb", model_row[["filename"]])

            if(file.exists(full_filename)){
              mod_id          = paste0("mod_", mod_idx)
              mod_name        = model_row[["name"]]
              mod_description = model_row[["description"]]
              if(is.na(mod_description)){
                mod_description = ""
              }

              # This sets ana_sol to no for sysems that have ODEs and don't
              # have linCmt() calls.
              if(!model_row[["algebraic"]] & 
                 !model_row[["linCmt"]]){
                ana_sol = "no"
              } else{
                ana_sol = "yes"
              }

              # This flags any model dependencies
              depends = model_row[["depends"]]

              # Appending to the summary table
              model_summary = rbind(model_summary,
              data.frame(
                ana_sol     = ana_sol,
                depends     = depends,
                mod_id      = mod_id,
                Name        = mod_name,
                Object      = mod_name,
                Type        = "rxode2",
                Model       = paste(readLines(full_filename, warn=FALSE), collapse="\n"),
                Description = mod_description
              )
              )

              # Appending to the selector elements
              if(ana_sol == "no" & is.na(depends)){
                select_group[[mod_src[["group"]]]][[mod_name]] = mod_id
                select_plain[[mod_name]]                      = mod_id
                select_subtext                                = c(select_subtext, mod_description)
              }

              mod_idx  = mod_idx + 1
            } else {
              msgs = c(msgs, paste0("nlmixr2lib file not found: ", full_filename))
            }
          }
        } else {
          # This can return the missing package message back to the user so
          # they wont wonder why there are no library packages available.
          msgs = c(msgs, state[["MC"]][["errors"]][["nlmir2lib_not_found"]])
        }
      }
      #---------------------------------------
      # User defined rxode2 models
      if(mod_src[["type"]] == "rxode2"){

        file_cmd = paste0("file_name = ", mod_src[["file"]])
        tcres =
          FM_tc(cmd     = file_cmd,
                tc_env  = NULL,
                capture = c("file_name"))

        if(tcres[["isgood"]]){
          # This is the name of the user define model file
          user_filename = tcres[["capture"]][["file_name"]]
          if(file.exists(user_filename)){

            mod_id           = paste0("mod_", mod_idx)
            mod_name         = mod_src[["name"]]
            model_summary = rbind(model_summary,
            data.frame(
              ana_sol        = "no",
              mod_id         = mod_id,
              Name           = mod_name,
              Object         = mod_src[["obj"]],
              Type           = mod_src[["type"]],
              Model          = paste(readLines(user_filename), collapse="\n"),
              Description    = mod_src[["description"]]
            )
            )

            select_group[[mod_src[["group"]]]][[mod_name]] = mod_id
            select_plain[[mod_name]]                      = mod_id
            select_subtext                                = c(select_subtext, mod_src[["description"]])

            mod_idx  = mod_idx + 1
          } else {
            FM_le(state, paste0("User-defined model: ", user_filename, " not found (skipping)"), entry_type="warning")
          }
        } else {
          FM_le(state, paste0("Unable to process: ", mod_src[["file"]]), entry_type="danger")
          FM_le(state, tcres[["msgs"]], entry_type="danger")

        }
      }
      #/rxode2
      #---------------------------------------
      # NONMEM
      if(mod_src[["type"]] == "NONMEM"){

        file_cmd = paste0("file_name = ", mod_src[["file"]])
        tcres =
          FM_tc(cmd     = file_cmd,
                tc_env  = NULL,
                capture = c("file_name"))

        if(tcres[["isgood"]]){
          # This is the name of the user define model file
          user_filename = tcres[["capture"]][["file_name"]]
          if(file.exists(user_filename)){

            mod_id           = paste0("mod_", mod_idx)
            mod_name         = mod_src[["name"]]
            model_summary = rbind(model_summary,
            data.frame(
              ana_sol        = "no",
              mod_id         = mod_id,
              Name           = mod_name,
              Object         = "",
              Type           = mod_src[["type"]],
              Model          = user_filename,
              Description    = mod_src[["description"]]
            )
            )

            select_group[[mod_src[["group"]]]][[mod_name]] = mod_id
            select_plain[[mod_name]]                      = mod_id
            select_subtext                                = c(select_subtext, mod_src[["description"]])

            mod_idx  = mod_idx + 1
          } else {
            FM_le(state, paste0("User-defined model: ", user_filename, " not found (skipping)"), entry_type="warning")
          }
        } else {
          FM_le(state, paste0("Unable to process: ", mod_src[["file"]]), entry_type="danger")
          FM_le(state, tcres[["msgs"]], entry_type="danger")
        }
      }
      # NONMEM
      #---------------------------------------
    }
  }

  if(length(mod_srcs) > 0){
    hasmdl = TRUE
  } else {
    # If there are no models we flip both of the is/has bits:
    hasmdl = FALSE
    isgood = FALSE
  }



  catalog = list(
    sources        = mod_srcs,
    summary        = model_summary,
    select_group   = select_group,
    select_plain   = select_plain,
    select_subtext = select_subtext,
    msgs           = msgs,
    hasmdl         = hasmdl,
    isgood         = isgood)

catalog}


#'@export
#'@title Fetches List of Available Models
#'@description Creates a catalog of the models available in the system file.
#'@param state MB state from \code{MB_fetch_state()}
#'@param current_ele MB model element from \code{MB_fetch_current_element()}
#'@return List with the following attributes:
#'\itemize{
#'  \item{isgood:} Boolean variable indicating success or failure.
#'  \item{msgs:} Messages to be passed back to the user.
#'  \item{hasappends:} Boolean variable indicating if appendable models were found.
#'  \item{select_plain:} Flat list with the models (ungrouped).
#'  \item{choicesOpt} List witht he subtext filled out.
#'}
#'@example inst/test_apps/MB_funcs.R
MB_fetch_appends   = function(state, current_ele){

  isgood          = TRUE
  hasappends      = FALSE
  msgs            = c()
  select_plain    = list()
  select_subtext  = c()
  choicesOpt      = NULL

  component       = MB_fetch_component(state, current_ele)

  model_catalog   = state[["MB"]][["model_catalog"]]
  if(model_catalog[["isgood"]]){

    # Getting all of the things the current component provides
    rxinfo = fetch_rxinfo(component[["rx_obj"]])
    provides = c(
     rxinfo[["elements"]][["states"]],
     rxinfo[["elements"]][["outputs"]],
     rxinfo[["elements"]][["parameters"]],
     rxinfo[["elements"]][["secondary"]])

    # getting the models that are only ODEs and also have dependencies:
    model_summary = state[["MB"]][["model_catalog"]][["summary"]]
    model_summary = model_summary[model_summary[["ana_sol"]] == "no", ]   # Only ODEs
    model_summary = model_summary[!is.na(model_summary[["depends"]]), ]   # Has dependencies 

    for(mod_id in model_summary[["mod_id"]]){
      # Pulling out the current row
      model_summary_row = model_summary[model_summary$mod_id == mod_id, ]

      # Taking the string dep1, dep2, dep3 and converting it into a vector
      depends =  strsplit(model_summary_row[["depends"]][1], "\\W*,\\W*")[[1]]

      # If all the dependencies are there, then we allow it to be appended
      if(all(depends %in% provides)){
         hasappends = TRUE
         select_subtext = model_summary_row[["Description"]]
         select_plain[[ model_summary_row[["Name"]] ]] = mod_id
      }
    }

    if(!is.null(select_subtext)){
      choicesOpt = list(
        subtext = stringr::str_trunc(
           select_subtext,
           width= state[["MC"]][["formatting"]][["catalog_selection"]][["truncate"]]
        )
      )
    }
  } else {
    isgood = FALSE
    msgs   = c(state[["MC"]][["errors"]][["fetch_catalog_failed"]], model_catalog[["msgs"]])
  }


  # If there are no appendable elements we put a message in the pulldown.
  if(!hasappends){
    select_plain    = list()
    select_plain[[state[["MC"]][["formatting"]][["append_model"]][["no_models"]] ]] = 
      state[["MC"]][["formatting"]][["append_model"]][["no_models"]]
    choicesOpt      = NULL
  }

  res = list(
    isgood        = isgood,
    msgs          = msgs,
    hasappends    = hasappends,  
    select_plain  = select_plain,
    choicesOpt    = choicesOpt)

res}

#'@export
#'@title Makes an rxode2 Object
#'@description Creates an rxode2 object from a model (either rxode2 function
#'or a NONMEM file)
#'@param type Type of supplied model can be "rxode2", "NONMEM"
#'@param model List containing the relevant information about the model. This
#'will depend on the model types.
#'\itemize{
#'   \item{rxode2:} The supplied model is in the rxode2 format.
#'   \itemize{
#'     \item{fcn_def:} Character string containing function definition.
#'     \item{fcn_obj:} Name of the funciton object created in \code{fcn_def}.
#'   }
#'   \item{NONMEM:} The supplied model is in NONMEM format (either a control
#'   \itemize{
#'     \item{model_file:} Character string containing the NONMEM model file.
#'   }
#'}
#'@return Results of \code{FM_tc()} when running the model. This will include
#'a field \code{isgood} which is a boolean variable indicating success or
#'failure. See the documentation for \code{FM_tc()} for the format returned
#'when evaluation results in a failure and how to address those. When
#'successful the \code{capture} field will contain the following:
#'\itemize{
#'  \item{fcn_obj:} The function name.
#'  \item{rx_obj:} The built rxode2 object.
#'}
#'@examples
#' fcn_def = ' my_func = function ()
#'    {
#'        description <- "One compartment PK model with linear clearance"
#'        ini({
#'            lka <- 0.45
#'            label("Absorption rate (Ka)")
#'            lcl <- 1
#'            label("Clearance (CL)")
#'            lvc <- 3.45
#'            label("Central volume of distribution (V)")
#'            propSd <- c(0, 0.5)
#'            label("Proportional residual error (fraction)")
#'        })
#'        model({
#'            ka <- exp(lka)
#'            cl <- exp(lcl)
#'            vc <- exp(lvc)
#'            cp <- linCmt()
#'            cp ~ prop(propSd)
#'        })
#'
#'    }'
#' fcn_obj = "my_func"
#' model = list(fcn_def = fcn_def,
#'              fcn_obj = fcn_obj)
#'
#'
#' rx_res = mk_rx_obj("rxode2", model)
#'
#' # function object
#' rx_res[["capture"]][["fcn_obj"]]
#'
#' # rxode2 object
#' rx_res[["capture"]][["rx_obj"]]
mk_rx_obj   = function(type, model){

  found_rxode2    = Sys.getenv("ruminate_rxode2_found")
  found_nonmem2rx = Sys.getenv("ruminate_nonmem2rx_found")

  if(found_rxode2 == "TRUE" & found_nonmem2rx == "TRUE"){
    if(type %in% c("rxode2", "NONMEM")){
      if(type == "rxode2"){
        mc = c(
          model[["fcn_def"]],
          paste0("fcn_obj = ", model[["fcn_obj"]]),
          paste0("rx_obj  = rxode2::rxode2(fcn_obj)")
        )

        tcres = FM_tc(
          cmd     = paste0(mc, collapse="\n"),
          tc_env  = NULL,
          capture = c("rx_obj", "fcn_obj"))
      }
      if(type == "NONMEM"){
        cmds = c(
          'rx_obj = nonmem2rx::nonmem2rx(model_file, save=FALSE, determineError=FALSE)',
          'fun_obj = rx_obj$fun')

        tcres =
          FM_tc(cmd = paste0(cmds, collapse="\n"),
                tc_env = list(model_file = model[["model_file"]]),
                capture = c("rx_obj", "fun_obj"))
      }
    }else{
      tcres = list(
        isgood = FALSE,
        msgs   = c(paste0("Unknown model type: ", type), "mx_rx_obj()")
      )
    }
  }else{
    msgs = c()
    if(found_rxode2 == "FALSE"){
      msgs = c(msgs, "rxode2 package was not found.")
    }
    if(found_nonmem2rx == "FALSE"){
      msgs = c(msgs, "rnonmem2rx package was not found.")
    }

    tcres = list(
      isgood = FALSE,
      msgs   = c("Needed packages missing mx_rx_obj()")
    )
  }
tcres}

#'@export
#'@title Tests the Model Catalog
#'@description Reads in models in the catalog and attempts to build them.
#'@param state MB state from \code{MB_fetch_state()}
#'@param as_cran Boolean to indicate if you're running this on CRAN
#'@param verbose Boolean to indicate if messages should be displayed.
#'@return List with the following attributes:
#'\itemize{
#' \item{isgood:} Boolean varaible indicating if all the models in the catalog
#' passed the test.
#' \item{msgs:} Messages indicating if the test was successful or not.
#'}
#'@example inst/test_apps/MB_funcs.R
MB_test_catalog   = function(state, as_cran=FALSE, verbose=TRUE){

  msgs   = c()
  isgood = TRUE
  models = MB_fetch_catalog(state)

  #if( state[["MB"]][["suggested"]][["found"]]){
  if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
    if(models[["isgood"]]){
      model_summary = models[["summary"]]
      # If we're running it as cran we pair it down to a single model
      # to speed thigns up:
      if(as_cran){
        if(which(model_summary$Name == "PK_1cmt") > 0){
          # First we look for PK_1cmt to choose a simple exmaple
          model_summary = model_summary[model_summary$Name == "PK_1cmt" , ]
        } else {
          # If that doesn't exist we choose the first catalog entry
          model_summary = model_summary[1, ]
        }
      }

      # Now we walk through each model and attempt to build it:
      for(ridx in 1:nrow(model_summary)){
        model = list(
          fcn_def=model_summary[ridx, ]$Model,
          fcn_obj=model_summary[ridx, ]$Object)

        mod_type = model_summary[ridx, ]$Type

        rx_res   = mk_rx_obj(mod_type, model)
        if(rx_res[["isgood"]]){
          if(verbose){
            FM_le(state, model_summary[ridx,][["Name"]], entry_type="success")
          }
        }else{
          isgood = FALSE
          if(verbose){
            FM_le(state, model_summary[ridx,][["Name"]], entry_type="failure")
          }
        }
      }

    } else {
      isgood = FALSE
      msgs = c(msgs, "Unable to fetch the model catalog.")
    }
  } else {
    isgood = FALSE
  }


  if(!is.null(msgs)){
    if(verbose){
      FM_le(state, msgs)
    }
  }
  res = list(
   isgood = isgood,
   msgs   = msgs
  )

res}
