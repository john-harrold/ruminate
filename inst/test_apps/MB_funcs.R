library(ruminate)
sess_res = MB_test_mksession(session=list())
session = sess_res$session
input   = sess_res$input

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

# Creates a new empty element
state = MB_new_element(state)

# Delete the current element
state = MB_del_current_element(state)

# Fetch a list of the current element
element = MB_fetch_current_element(state)

# You can modify the element
element[["name"]] = "A more descriptive name"

# You can now place element back in the state
state = MB_set_current_element(state, element)

# This will provide a list of the available models
models = MB_fetch_catalog(state)
