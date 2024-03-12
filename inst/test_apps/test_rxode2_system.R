# Dosing into Ac and At is in nmoles. So to dose 3 mg you would use 3e6/MW
my_model <- function() {
ini({
   # Typical Value of System Parameters 
   TV_F1           = fixed(c(.Machine$double.eps, 0.744, .Machine$double.xmax))
   TV_ka           =       c(.Machine$double.eps, 0.282, .Machine$double.xmax)
   TV_CL           =   log(c(.Machine$double.eps, 0.200, .Machine$double.xmax))
   TV_Vc           =   log(c(.Machine$double.eps, 3.61, .Machine$double.xmax))
   TV_Vp           =   log(c(.Machine$double.eps, 2.75, .Machine$double.xmax))
   TV_Q            =   log(c(.Machine$double.eps, 0.747, .Machine$double.xmax))
   TV_MW           = fixed(c(.Machine$double.eps, 140, .Machine$double.xmax))
   TV_BM_IC        =       c(.Machine$double.eps, 1000, .Machine$double.xmax)
   TV_kdeg_BM      = fixed(c(.Machine$double.eps, 0.1, .Machine$double.xmax))
   TV_Emax         =       c(.Machine$double.eps, 100.0, .Machine$double.xmax)
   TV_EC50         = fixed(c(.Machine$double.eps, 300, .Machine$double.xmax))

   # Between-subject variability:
     ETAka +     ETACL +     ETAVc +     ETAVp +     ETAQ +      ETABM_IC +  ETAEmax ~
   c(0.416,                                                                         
     0,          0.09875,                                                            
     0,          0.0786,     0.116,                                                 
     0,          0.0619,     0.0377,     0.0789,                                    
     0,          0,          0,          0,          0.699,                         
     0,          0,          0,          0,          0,          0.05,              
     0,          0,          0,          0,          0,          0,          0.10)  

   # Error model parameters
   prop_err_PK  =  c(.Machine$double.eps, 0.1, .Machine$double.xmax)
   add_err_PK   =  c(.Machine$double.eps, 0.1, .Machine$double.xmax)
   add_err_BM   =  c(.Machine$double.eps, 0.1, .Machine$double.xmax)

})
model({ 
   # System Parameters 
   F1          = TV_F1
   ka          = TV_ka*exp(ETAka)
   CL          = exp(TV_CL)*exp(ETACL)
   Vc          = exp(TV_Vc)*exp(ETAVc)
   # AMTIFY:Cc
   Cc          = Ac/(Vc)
   Vp          = exp(TV_Vp)*exp(ETAVp)
   Q           = exp(TV_Q)*exp(ETAQ)
   MW          = TV_MW
   BM_IC       = TV_BM_IC*exp(ETABM_IC)
   kdeg_BM     = TV_kdeg_BM
   Emax        = TV_Emax*exp(ETAEmax)
   EC50        = TV_EC50

   Dinf        = 0.0

   # Static Secondary Parameters 
   WTTV        = 70 
   CL_IND      = CL*(1.0+SEX_ID*.1)*(1.0+SUBTYPE_ID*.1) 
   kel         = CL_IND/Vc*((WT/WTTV))^(-0.35) 
   kcp         = Q/Vc*((WT/WTTV))^(-0.35) 
   kpc         = Q/Vp*((WT/WTTV))^(-0.35) 
   ksyn_BM     = kdeg_BM*BM_IC 

   # Dynamic Secondary Parameters 
   STIM        = 1.0+ Emax*Cc/(EC50+Cc) 

   # Defining ODEs
   d/dt(At)    = (-ka*At)
   d/dt(Ac)    = ((ka*At*F1/Vc  -kel*Cc - kcp*Cc  + kpc*Cp*Vp/Vc + Dinf/Vc))*Vc
   d/dt(Cp)    = (kcp*Cc*Vc/Vp - kpc*Cp)
   d/dt(BM)    = ksyn_BM*STIM-(kdeg_BM*BM)

   # Outputs and error models
   C_ng_ml     = Cc*MW
   C_ng_ml ~ add(add_err_PK) + prop(prop_err_PK)

   BM_obs      = BM
   BM_obs  ~ add(add_err_BM)

})
}
