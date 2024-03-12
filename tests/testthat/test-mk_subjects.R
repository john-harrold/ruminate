
if( Sys.getenv("ruminate_rxfamily_found") == "TRUE"){
  library(nlmixr2lib)
  nsub = 3
  for(ridx in 1:nrow(nlmixr2lib::modeldb)){
    model_name = nlmixr2lib::modeldb[ridx, ][["name"]]
    mobj      = suppressMessages(nlmixr2lib::modellib(name = model_name))
    mobj      = suppressMessages(rxode2(mobj))
    rxdetails = ruminate::fetch_rxinfo(mobj)
  
    # Making sure there is iiv
    if(is.null(rxdetails[["elements"]][["iiv"]])){
      mobj  = suppressMessages(addEta(mobj, c( rxdetails[["elements"]][["population"]][1])))
    }
  
    # Adding default values for covariates
    covs = NULL
    if(length(rxdetails[["elements"]][["covariates"]]) > 0){
      if(is.null(covs)){
        covs = list()
      }
      for(cname in rxdetails[["elements"]][["covariates"]]){
        covs[[cname]] = list(
             type     = "fixed",
             values   = c(1))
      }
  
      
    }
  
    mks_res = suppressMessages(mk_subjects(object=mobj, covs = covs, nsub=nsub))
  
    expect_true(mks_res[["isgood"]])
  
  }
}

