statusquo_catch <- function(ABC.DATA) {
    
    logABC.DATA <- log(ABC.DATA)
    
    logABC.DATA$flex <-  1  # introduction of flatfish flex
    logABC.DATA$A80 <-  1  # introduction of A80
    logABC.DATA$pollock.bs.UB <-  as.numeric(ABC.DATA$ABC.BS.201 > 1.2e6)
    logABC.DATA$po10 <- 1
    logABC.DATA$pre97 <- 0
    logABC.DATA$is93 <- 0
    logABC.DATA$WAISSLadj <- 1
    logABC.DATA$solegone <-  1
    logABC.DATA$plaicegone <-  1
    logABC.DATA$kamsplit <-  1
    logABC.DATA$AFA <-  1
    logABC.DATA$pollockAIchange <-  1 
    logABC.DATA$A28 <- 1
    logABC.DATA$atkadisp <- 0
    logABC.DATA$SSL <-  1 # stellar sea lion closure
    logABC.DATA$WAISSL <- 0
    logABC.DATA$A80.ask.POP <- 1
    logABC.DATA$is93 <- 0
    logABC.DATA$ABCboth.UB.150 <- as.numeric(ABC.DATA$ABC.BS.201 + ABC.DATA$ABC.BSAI.202 >= 1.5e6)
    

    #devtools::use_data(mean.BS.AI.ABCs, catch_BOTHBIND_nosur, catch_BOTHBIND_sur, tac_BOTHBIND_sur, tac_fit_nosur, tac_BOTHBIND_SIMPLER_sur, catch_BOTHBIND_SIMPLER_nosur, catch_BOTHBIND_SIMPLER_sur, internal = T, overwrite = T)
    

        TAC.BOTHBIND <- predict.tac.function(model="SUR",fit_sur=tac_BOTHBIND_sur,fit_nosur=tac_fit_nosur,logABC.DATA)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_BOTHBIND_sur,fit_nosur = catch_BOTHBIND_nosur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="NOSUR",fit_sur=catch_BOTHBIND_sur,fit_nosur = catch_BOTHBIND_nosur,TAC.BOTHBIND )

        TAC.BOTHBIND.SIMPLER <- predict.tac.function(model="SUR",fit_sur=tac_BOTHBIND_SIMPLER_sur,fit_nosur=tac_fit_nosur,logABC.DATA)
        
        CATCH.BOTHBIND.SIMPLER.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_BOTHBIND_SIMPLER_sur,fit_nosur = catch_BOTHBIND_SIMPLER_nosur,TAC.BOTHBIND.SIMPLER )
        CATCH.BOTHBIND.SIMPLER.SUROLS <- predict.catch.function(model="NOSUR",fit_sur=catch_BOTHBIND_SIMPLER_sur,fit_nosur = catch_BOTHBIND_SIMPLER_nosur,TAC.BOTHBIND.SIMPLER )
    

  ## SURSUR and SUROLS
    #TAC.PRED.SUR <- predict.tac.function(model="SUR",fit_sur=tac_fit_sur,fit_nosur=tac_fit_nosur,logABC.DATA)
    #CATCH.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_fit_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    #CATCH.SURNOSUR <- predict.catch.function(model="NOSUR",fit_sur=catch_fit_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    
 # No First Year Data SURSUR and SUROLS
    #TAC.NOFIRSTYEAR  <- predict.tac.function(model="SUR",fit_sur=tac_NOFIRSTYEAR_sur,fit_nosur=tac_fit_nosur,logABC.DATA)
    #CATCH.NOFIRSTYEAR.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_NOFIRSTYEAR_sur,fit_nosur = catch_NOFIRSTYEAR_nosur,TAC.NOFIRSTYEAR )
    #CATCH.NOFIRSTYEAR.SURNOSUR <- predict.catch.function(model="NOSUR",fit_sur=catch_NOFIRSTYEAR_sur,fit_nosur = catch_NOFIRSTYEAR_nosur,TAC.NOFIRSTYEAR )
  
  ## NOROCKSOLE (SURSUR)
    #TAC.NOROCKSOLE <- predict.tac.function(model="NOROCKSOLE",fit_sur=tac_NOROCKSOLE_sur,fit_nosur=tac_fit_nosur,logABC.DATA)
    #CATCH.NOROCKSOLE <- predict.catch.function(model="NOROCKSOLE",fit_sur=catch_NOROCKSOLE_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    
    ## FLATSUR (SURSUR)
    #TAC.FLATSUR <- predict.tac.function(model="FLATSUR",fit_sur=tac_FLATSUR_sur,fit_nosur=tac_fit_nosur,logABC.DATA)
    #CATCH.FLATSUR <- predict.catch.function(model="FLATSUR",fit_sur=catch_FLATSUR_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
      
        
    # create ensemble
    # 
    #CATCH.PRED <-  (CATCH.SURSUR + CATCH.SURNOSUR + CATCH.NOFIRSTYEAR.SURNOSUR + CATCH.NOFIRSTYEAR.SURSUR + CATCH.FLATSUR + CATCH.NOROCKSOLE)/6
    CATCH.PRED <- (CATCH.BOTHBIND.SURSUR + CATCH.BOTHBIND.SUROLS + CATCH.BOTHBIND.SIMPLER.SUROLS + CATCH.BOTHBIND.SIMPLER.SURSUR)/4
    
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