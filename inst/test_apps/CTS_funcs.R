# For more information see the Clinical Trial Simulation vignette:
# https://ruminate.ubiquity.tools/articles/clinical_trial_simulation.html

# None of this will work if rxode2 isn't installed:
if(is_installed("rxode2")){

sess_res = CTS_test_mksession(session=list(), full_session=FALSE)
state = sess_res$state
session = sess_res$session
input   = sess_res$input

# Creates a new empty element
state = CTS_new_element(state)

# Delete the current element
state = CTS_del_current_element(state)

# Fetch a list of the current element
element = CTS_fetch_current_element(state)

# You can modify the element
element[["element_name"]] = "A more descriptive name"

# Simulating the current element
element = CTS_simulate_element(state, element)

# You can now place element back in the state
state = CTS_set_current_element(state, element)

# This will extract the code for the current module
code = CTS_fetch_code(state)
code


# This will update the checksum of the module state
state = CTS_update_checksum(state)

#'@example inst/test_apps/CTS_funcs.R
}
