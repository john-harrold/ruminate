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
MB_Server <- function(id,
               id_ASM        = "ASM",
               FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml"),
               MOD_yaml_file = system.file(package = "ruminate",  "templates", "MB.yaml"),
               deployed      = FALSE,
               react_state   = NULL) {
  moduleServer(id, function(input, output, session) {


    #------------------------------------
    # Generating the model selection catalog
    output$select_model_catalog = renderUI({
      input$element_selection
      state = MB_fetch_state(id              = id,
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

         choicesOpt = list(subtext =  model_catalog[["select_subtext"]])

         uiele =
         shinyWidgets::pickerInput(
           selected   = current_ele[["ui"]][["catalog_selection"]],
           inputId    = NS(id, "catalog_selection"),
           label      = state[["MC"]][["labels"]][["catalog_selection"]],
           choices    = choices,
           width      = state[["MC"]][["formatting"]][["catalog_selection"]][["width"]],
           choicesOpt = choicesOpt)
      } else {
        if(is.null( model_catalog[["msgs"]])){
          uiele = "Unable to extract catalog"
        } else {
          uiele =   model_catalog[["msgs"]]
        }
      }

      uiele})

    #------------------------------------
    # Select the active model
    output$MB_ui_select_element = renderUI({
      input$button_clk_save
      input$button_clk_del
      input$button_clk_copy
      input$button_clk_new
      state = MB_fetch_state(id              = id,
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

      uiele})
    #------------------------------------
    # Current model name:
    output$MB_ui_text_element_name = renderUI({
      input$element_selection
      state = MB_fetch_state(id              = id,
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
    # Generated data reading code
    observe({
      state = MB_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)

      if(is.null(state[["MB"]][["code"]])){
        uiele = "# No code to generate"
      } else {
        uiele = state[["MB"]][["code"]]
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
    output$ui_mb_msg = renderText({
      state = MB_fetch_state(id              = id,
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
    output$ui_mb_compact  =  renderUI({
      state = MB_fetch_state(id              = id,
                             input           = input,
                             session         = session,
                             FM_yaml_file    = FM_yaml_file,
                             MOD_yaml_file   = MOD_yaml_file,
                             react_state     = react_state)


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
     # Uncomment this if your model has a components table
     #uiele_mb_elements = rhandsontable::rHandsontableOutput(NS(id, "hot_mb_elements"))
     #uiele_mb_elements_button = tagList(
     # shinyWidgets::dropdownButton(
     #   uiele_mb_elements,
     #   inline  = FALSE,
     #   right   = TRUE ,
     #   size    = "sm",
     #   circle  = FALSE,
     #   status  = "primary btn-custom-mb",
     #   icon    = icon("layer-group", lib="font-awesome"),
     #   tooltip = tooltipOptions(title = state[["MC"]][["tooltips"]][["elements"]]))
     #)

      uiele = tagList(
        div(style="display:inline-block", "Place model name, attributes and inputs here."),
        tags$br(),
        div(style="display:inline-block", htmlOutput(NS(id, "ui_mb_msg")))
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
               htmlOutput(NS(id, "ui_mb_new_btn"))
               ))

      # Appending the preview
      div_style = paste0("display:inline-block;vertical-align:top;",
        "width:",   state[["MC"]][["formatting"]][["preview"]][["width"]],  ";",
        "height: ", state[["MC"]][["formatting"]][["preview"]][["height"]])
      uiele_preview = div(style=div_style,
                          "Place your module model preview here.")
      uiele = tagList(
        uiele,
        uiele_preview,
        uiele_buttons_right,
        tags$br()
      )


      uiele = tagList( uiele,
        tags$br(),
        "Place module construction elements here."
      )
      uiele
    })

    #------------------------------------
    # Creating reaction if a variable has been specified
    if(!is.null(react_state)){
      # Here we list the ui inputs that will result in a state change:
      toListen <- reactive({
        list(
           # react_state[[id_ASM]])
             input$button_clk_new,
             input$button_clk_del,
             input$button_clk_copy,
             input$button_clk_save)
      })
      # This updates the reaction state:
      observeEvent(toListen(), {
        state = MB_fetch_state(id        = id,
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
    # Removing holds
    remove_hold_listen  <- reactive({
        list(
           # react_state[[id_ASM]])
           # input$button_clk_new,
           # input$button_clk_del,
           # input$button_clk_copy,
           # input$button_clk_save,
             input$element_selection,
             input$current_element)
      })
    observeEvent(remove_hold_listen(), {
      # Once the UI has been regenerated we
      # remove any holds for this module
      state = MB_fetch_state(id              = id,
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
#'@param input Shiny input variable
#'@param session Shiny session variable
#'@param FM_yaml_file App configuration file with FM as main section.
#'@param MOD_yaml_file  Module configuration file with MC as main section.
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
#'@examples
#' # Within shiny both session and input variables will exist,
#' # this creates examples here for testing purposes:
#' sess_res = MB_test_mksession(session=list())
#' session = sess_res$session
#' input   = sess_res$input
#'
#' # Configuration files
#' FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
#' MOD_yaml_file = system.file(package = "ruminate", "templates", "MB.yaml")
#'
#' # Creating an empty state object
#' state = MB_fetch_state(id              = "MB",
#'                        input           = input,
#'                        session         = session,
#'                        FM_yaml_file    = FM_yaml_file,
#'                        MOD_yaml_file   = MOD_yaml_file,
#'                        react_state     = NULL)
MB_fetch_state = function(id, input, session, FM_yaml_file, MOD_yaml_file, react_state){

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

         # initializing the previous ui values as well:
         if(is.null(state[["MB"]][["ui_prev"]][[ui_name]])){
           state[["MB"]][["ui_old"]][[ui_name]] = state[["MB"]][["ui"]][[ui_name]]
         }
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
  current_ele = MB_fetch_current_element(state)
  # There are scenarios where you wouldn't want to do this. Like when
  # switching elements in the ui. You would need to add some logic to
  # only update below conditionally.

  for(ui_name in state[["MB"]][["ui_ids"]]){
    if(!fetch_hold(state, ui_name)){
      if(ui_name %in% names(state[["MB"]][["button_counters"]])){
        # Button changes are compared to the button click tracking values
        change_detected =
          has_changed(ui_val  = state[["MB"]][["ui"]][[ui_name]],
                      old_val = state[["MB"]][["button_counters"]][[ui_name]])
        if(change_detected){
          formods::FM_le(state, paste0("button click: ", ui_name, " = ", state[["MB"]][["ui"]][[ui_name]]))

          # Saving the change:
          state[["MB"]][["button_counters"]][[ui_name]] =
            state[["MB"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)
        }
      }else{
        change_detected =
          has_changed(ui_val  = state[["MB"]][["ui"]][[ui_name]],
                      old_val = state[["MB"]][["ui_old"]][[ui_name]])
        if(change_detected){
          formods::FM_le(state, paste0("setting model : ", ui_name, " = ", paste(state[["MB"]][["ui"]][[ui_name]], collapse=", ")))

          # Saving the change:
          state[["MB"]][["ui_old"]][[ui_name]] = state[["MB"]][["ui"]][[ui_name]]

          # logging the changed ui name:
          changed_uis = c(changed_uis, ui_name)

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

    state = MB_set_current_element(
      state   = state,
      element = current_ele)
  }
  #---------------------------------------------
  # clip model
  if("button_clk_clip" %in% changed_uis){
    FM_le(state, "clip model")
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


    # NOTE: You may need to add other code here
    # to change other aspects about the old element
    # that is being copied.

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
  # new model
  if("button_clk_new" %in% changed_uis){
    FM_le(state, "new model")
    state = MB_new_element(state)
  }
  #---------------------------------------------
  # selected model changed
  if("element_selection" %in% changed_uis){
    state[["MB"]][["current_element"]] =
       state[["MB"]][["ui"]][["element_selection"]]

    # Setting the hold for all the other UI elements
    state = set_hold(state)
  }
  #---------------------------------------------
  # Passing any messages back to the user
  state = FM_set_ui_msg(state, msgs)

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
#' sess_res = MB_test_mksession(session=list())
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
                      "button_clk_new")

  # This contains all of the relevant ui_ids in the module
  ui_ids          = c(button_counters,
                      "element_selection",
                      "catalog_selection",
                      "element_name")

  # Making all the ui_ids holdable
  ui_hold         = ui_ids


  # These are the module ui elements that are associated with
  # the current element
  ui_ele          = c("catalog_selection")

  state = FM_init_state(
    FM_yaml_file    = FM_yaml_file,
    MOD_yaml_file   = MOD_yaml_file,
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

  # Pulling out the model sources
  state[["MB"]][["model_catalog"]]        =  MB_fetch_catalog(state)

  # Creating a default element:
  state = MB_new_element(state)

  FM_le(state, "State initialized")
state}

#'@export
#'@title Fetch Module Code
#'@description Fetches the code to generate results seen in the app
#'@param state MB state from \code{MB_fetch_state()}
#'@return Character object vector with the lines of code
#'@examples
#' # We need a module state:
#' sess_res = MB_test_mksession(session=list())
#' state = sess_res$state
#'
#' code = MB_fetch_code(state)
#'
#' cat(code)
MB_fetch_code = function(state){

  code = NULL

code}

#'@export
#'@title Append Report Elements
#'@description Description
#'@param state MB state from \code{MB_fetch_state()}
#'@param rpt Report with the current content of the report which will be appended to in
#'this function. For details on the structure see the documentation for \code{\link{formods::FM_generate_report}}.
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
#'@seealso \code{\link{formods::FM_generate_report}}
MB_append_report = function(state, rpt, rpttype, gen_code_only=FALSE){

  isgood    = TRUE
  hasrptele = FALSE
  code      = c()
  msgs      = c()


  # The MB module only supports the following report types:
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
#'@title Fetch Model Builder Module Datasets
#'@description Fetches the datasets contained in the module.
#'@param state MB state from \code{MB_fetch_state()}
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
#' sess_res = MB_test_mksession(session=list())
#' state = sess_res$state
#'
#' ds = MB_fetch_ds(state)
#'
#' ds
MB_fetch_ds = function(state){
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
  if(state[["MB"]][["isgood"]]){

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
#'@title Populate Session Data for Module Testing
#'@description Populates the supplied session variable for testing.
#'@param session Shiny session variable (in app) or a list (outside of app)
#'@param id An ID string that corresponds with the ID used to call the modules UI elements
#'@return list with the following elements
#' \itemize{
#'   \item{isgood:} Boolean indicating the exit status of the function.
#'   \item{session:} The value Shiny session variable (in app) or a list (outside of app) after initialization.
#'   \item{input:} The value of the shiny input at the end of the session initialization.
#'   \item{state:} App state.
#'   \item{rsc:} The \code{react_state} components.
#'}
#'@examples
#' sess_res = MB_test_mksession(session=list())
MB_test_mksession = function(session, id = "MB"){

  isgood = TRUE
  rsc    = list()
  input  = list()

  # Configuration files
  FM_yaml_file  = system.file(package = "formods", "templates", "formods.yaml")
  MOD_yaml_file = system.file(package = "ruminate", "templates", "MB.yaml")

  # Creating an empty state object
  state = MB_fetch_state(id              = "MB",
                         input           = input,
                         session         = session,
                         FM_yaml_file    = FM_yaml_file,
                         MOD_yaml_file   = MOD_yaml_file,
                         react_state     = NULL)

  res = list(
    isgood  = isgood,
    session = session,
    input   = input,
    state   = state,
    rsc     = rsc
  )
}

#'@export
#'@title New ===Module_Name=== model
#'@description Appends a new empty model to the MB state object
#'and makes this new model the active model.
#'@param state MB state from \code{MB_fetch_state()}
#'@return ===ZZ== state object containing a new model and that
#'model is set as the current active model. See the help for
#'\code{MB_fetch_state()} for ===ELEMENT== format.
#'@example inst/test_apps/MB_funcs.R
MB_new_element = function(state){

  # Incrementing the element counter
  state[["MB"]][["element_cntr"]] = state[["MB"]][["element_cntr"]] + 1

  # Creating a default element ID
  element_id = paste0("element_", state[["MB"]][["element_cntr"]])

  # Creating the object name for this element
  element_ds_object_name = paste0(state[["MC"]][["element_object_name"]],
                          "_", state[["MB"]][["element_cntr"]])
  # Extracting the model catalog:
  model_catalog = state[["MB"]][["model_catalog"]]

  # Default for a new element:
  element_def =
    list(
         # internal use only
         isgood                 = TRUE,
         ui                     =
           list(
                element_name        = paste0("Model ", state[["MB"]][["element_cntr"]]),
                catalog_selection   = model_catalog[["summary"]][1, "mod_id"]
                ),
         id                     = element_id,
         idx                    = state[["MB"]][["element_cntr"]],
         element_ds_object_name = element_ds_object_name,
         code_previous          = NULL,
         # user facing
         # This is used if you build the element in a layering method sort of
         # like how the ggplot figures in the FG module builds using different
         # ggplot commands (layers).
         elements_table         = NULL,
         # Generated on save
         checksum               = NULL,
         code                   = NULL,
         code_dw_only           = NULL)


  # This contains the code to generate the input dataset
  code_previous = c(
    paste0(
           element_ds_object_name,
           " = ",
            state[["MB"]][["UD"]][["object_name"]]))
  element_def[["code_previous"]] = code_previous

  # Dropping the new element into the state
  state[["MB"]][["elements"]][[element_id]] = element_def
  # Setting the new element as current
  state[["MB"]][["current_element"]]     = element_id

state}


#'@export
#'@title Fetches Current model
#'@description Takes a MB state and returns the current active
#'model
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
#'@return ===ZZ== state object with the current model set using the
#'supplied value.
#'@example inst/test_apps/MB_funcs.R
MB_set_current_element    = function(state, element){

  element_id = state[["MB"]][["current_element"]]

  state[["MB"]][["elements"]][[element_id]] = element

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
#'@title Fetches List of Available Models
#'@description Creates a catalog of the models available in the system file.
#'@param state MB state from \code{MB_fetch_state()}
#'@return List with the following attributes:
#'@example inst/test_apps/MB_funcs.R
MB_fetch_catalog   = function(state){

  isgood         = TRUE
  msgs           = c()
  model_summary  = NULL
  select_subtext = c()
  select_group   = list()
  select_plain   = list()

  # looking for packages to use conditionally below
  found_nlmixr2lib = formods::is_installed("nlmixr2lib")
  found_ubiquity   = formods::is_installed("ubiquity")


  mod_idx  = 1
  mod_srcs = state[["MC"]][["sources"]]

  if(length(mod_srcs)==0){
    isgood = FALSE
    msgs   = c(msgs, state[["MC"]][["labels"]][["catalog_empty"]])
  }

  if(isgood){
    for(mod_idx in 1:length(mod_srcs)){

      # This contains the current model source
      mod_src = mod_srcs[[mod_idx]][["source"]]

      #---------------------------------------
      # Appends all of the nlmixr2lib models
      if(mod_src[["type"]] == "nlmixr2lib"){
        if(found_nlmixr2lib){
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


              # Appending to the summary table
              model_summary = rbind(model_summary,
              data.frame(
                mod_id         = mod_id,
                Name           = mod_name,
                Object         = mod_name,
                Type           = "rxode2",
                Model          = paste(readLines(full_filename), collapse="\n"),
                Description    = mod_description
              )
              )

              # Appending to the selector elements
              #select_group[[mod_src[["name"]]]][[mod_id]] = mod_name
              #select_plain[[mod_id]]            = mod_name

              select_group[[mod_src[["name"]]]][[mod_name]] = mod_id
              select_plain[[mod_name]]                      = mod_id
              select_subtext                                = c(select_subtext, mod_description)

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
      if(mod_src[["type"]] == "ubiquity"){
      }
      #---------------------------------------
      if(mod_src[["type"]] == "NONMEM"){
      }
      #---------------------------------------

    }
  }

  if(length(mod_src) > 0){
    hasmdl = TRUE
  } else {
    # If there are no models we flip both of the is/has bits:
    hasmdl = FALSE
    isgood = FALSE
  }



  catalog = list(
    sources        = mod_src,
    summary        = model_summary,
    select_group   = select_group,
    select_plain   = select_plain,
    select_subtext = select_subtext,
    msgs           = msgs,
    hasmdl         = hasmdl,
    isgood         = isgood)

catalog}
