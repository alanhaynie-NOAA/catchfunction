statusquo_catch <- function(ABC.DATA) {
    
    ABC.DATA$flex <-  1  # introduction of flatfish flex
    ABC.DATA$A80 <-  1  # introduction of A80
    ABC.DATA$sablefish.ai.over.3k <-  as.numeric(ABC.AI.203>3e3)  # sablefish upper bound
    ABC.DATA$pcod.over.280k <-  as.numeric(ABC.BSAI.202 >=  28e4) # pcod upper bound
    ABC.DATA$pollock.bs.UB <-  as.numeric(ABC.BS.201 > 1.2e6 & A80 ==  1) + as.numeric(ABC.BS.201 > 1.33e6 & A80 == 0) # consider a single UB?
    ABC.DATA$pollock.bs.ABC <-  (1-pollock.bs.UB)*ABC.BS.201 # Pollock ABC when not at UB
    ABC.DATA$SSL <-  1 # stellar sea lion closure
    ABC.DATA$solegone <-  1
    ABC.DATA$plaicegone <-  1
    ABC.DATA$AFA <-  1
    ABC.DATA$kamsplit <-  1
    ABC.DATA$pollockAIchange <-  1 
    
  ## SURSUR and SUROLS
    TAC.PRED.SUR <- predict.tac.function(model="SUR",fit_sur=tac_fit_sur,fit_nosur=tac_fit_nosur,ABC.DATA)
    CATCH.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_fit_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    CATCH.SURNOSUR <- predict.catch.function(model="NOSUR",fit_sur=catch_fit_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    
 # No First Year Data SURSUR and SUROLS
    TAC.NOFIRSTYEAR  <- predict.tac.function(model="SUR",fit_sur=tac_NOFIRSTYEAR_sur,fit_nosur=tac_fit_nosur,ABC.DATA)
    CATCH.NOFIRSTYEAR.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_NOFIRSTYEAR_sur,fit_nosur = catch_NOFIRSTYEAR_nosur,TAC.NOFIRSTYEAR )
    CATCH.NOFIRSTYEAR.SURNOSUR <- predict.catch.function(model="NOSUR",fit_sur=catch_NOFIRSTYEAR_sur,fit_nosur = catch_NOFIRSTYEAR_nosur,TAC.NOFIRSTYEAR )
        
        
    # create ensemble
    # 
    CATCH.PRED <-  (CATCH.SURSUR + CATCH.SURNOSUR + CATCH.NOFIRSTYEAR.SURNOSUR + CATCH.NOFIRSTYEAR.SURSUR)/4
    
    output <- CATCH.PRED[c("CATCH.BS.141",
                           "CATCH.BS.204",
                           "CATCH.BS.103",
                           "CATCH.BS.102",
                           "CATCH.BS.147",
                           "CATCH.BS.303",
                           "CATCH.BS.60",
                           "CATCH.BS.100",
                           "CATCH.BS.310",
                           "CATCH.BS.202",
                           "CATCH.BS.106",
                           "CATCH.BS.301",
                           "CATCH.BS.201",
                           "CATCH.BS.104",
                           "CATCH.BS.307",
                           "CATCH.BS.203",
                           "CATCH.BS.400",
                           "CATCH.BS.65",
                           "CATCH.BS.326",
                           "CATCH.BS.90",
                           "CATCH.BS.50",
                           "CATCH.BS.140")]
    return (output)
}