library(ruminate)
# Module IDs                                                                   
id     = "NCA"                                                                 
# We need session and input variables to be define                             
 sess_res = NCA_test_mksession()
                                                                               
# Extracting the session and input variables                                   
session      = sess_res$session                                                
input        = sess_res$input                                                  
react_state  = list()                                                          
                                                                               
# We also need configuration files                                             
FM_yaml_file  = system.file(package = "formods",  "templates", "formods.yaml") 
MOD_yaml_file = system.file(package = "ruminate", "templates", "NCA.yaml")     
                                                                               
# Getting the current module state                                             
state = NCA_fetch_state(id             = id,                                   
                       input           = input,                                
                       session         = session,                              
                       FM_yaml_file    = FM_yaml_file,                         
                       MOD_yaml_file   = MOD_yaml_file,                        
                       react_state     = react_state)                          
                                                                               
# Pulls out the active analysis                                                
current_ana = NCA_fetch_current_ana(state)

# This will get the dataset associated with this analysis
ds = NCA_fetch_ana_ds(state, current_ana)

# After making changes you can update those in the state
state = NCA_set_current_ana(state, current_ana)

# You can use this to check the current analysis
current_ana = NCA_process_current_ana(state)

# This will pull out the code for the module
fc_res = NCA_fetch_code(state)

# This will use patterns defined for the site to detect
# columns. In this example we are detecting the id column:
id_col = NCA_find_col(
  patterns = state[["MC"]][["detect_col"]][["id"]],
  dscols   = names(ds))

# This creates a new analysis
state = NCA_new_ana(state)

# Switching back to the previous analysis
state = NCA_mkactive_ana(state, "NCA_1")

