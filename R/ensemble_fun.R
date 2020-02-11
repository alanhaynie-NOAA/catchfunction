#' Title
#'
#' @param ABC.DATA 
#' @param scenario 
#' @param outputtype 
#'
#' @return
#' @export
#'
#' @examples
ensemble_fun <- function(ABC.DATA,scenario,outputtype) {
    
    if (scenario == 1 | scenario == 2 | scenario == 3) {
        FISH.DATA <- ABC.DATA
        FISH.DATA$ABCboth <- FISH.DATA$ABC.BSAI.202 + FISH.DATA$ABC.BS.201
        FISH.DATA$ABCboth.UB.150 <- as.numeric(FISH.DATA$ABC.BS.201 + FISH.DATA$ABC.BSAI.202 >= 1.5e6)
        FISH.DATA$pollock.bs.UB <-  as.numeric(FISH.DATA$ABC.BS.201 > 1.2e6)
    }   
    
    # These are all the indicators required in the regressions (I honestly think some of them are no longer needed, so this could probably
    # do to be cleaned up a bit, but its not hurting anyone.)
    # 
    # 1 means active in scenarios (2018+), 0 means inactive.  So for example flatfish flex is active from 2018 onwards; the strict SSL closure (WAISSL) from 2011-2014 is not.
    
    FISH.DATA$flex <-  1  # introduction of flatfish flex
    FISH.DATA$A80 <-  1  # introduction of A80
    FISH.DATA$po10 <- 1
    FISH.DATA$pre97 <- 0
    FISH.DATA$is93 <- 0
    FISH.DATA$WAISSLadj <- 1
    FISH.DATA$solegone <-  1
    FISH.DATA$plaicegone <-  1
    FISH.DATA$kamsplit <-  1
    FISH.DATA$AFA <-  1
    FISH.DATA$pollockAIchange <-  1 
    FISH.DATA$A28 <- 1
    FISH.DATA$atkadisp <- 0
    FISH.DATA$SSL <-  1 # stellar sea lion closure + limited reopening
    FISH.DATA$WAISSL <- 0 # stellar sea lion closure, no limited reopening
    FISH.DATA$A80.ask.POP <- 1
    FISH.DATA$A82 <- 1
    
    
    if (scenario == 1) {
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1, model="SUR",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="NOSUR",fit = catch_BOTHBIND_loglin_nosur,TAC.BOTHBIND )
        
        TAC.BOTHBIND.FLATSUR <- predict.tac.function(predictmethod = 1, model="FLATSUR",fit=tac_BOTHBIND_FLATSUR_loglin_sur,FISH.DATA)
        
        CATCH.BOTHBIND.FLATSUR.SURSUR <- predict.catch.function(model="FLATSUR",fit=catch_BOTHBIND_FLATSUR_loglin_sur,TAC.BOTHBIND.FLATSUR )
        
    }  else if (scenario == 2) {
        #whitefish dom
        TAC.BOTHBIND.FLATSUR <- predict.tac.function(predictmethod = 1, model="FLAT_WFDOM",fit=tac_BOTHBIND_FLATSUR_loglin_sur,FISH.DATA)
        TAC.BOTHBIND.FLATSUR$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND.FLATSUR$ABC.BS.201 + TAC.BOTHBIND.FLATSUR$ABC.BSAI.202 >= 1.50e6*1.1)
        
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1,model="SUR_WFDOM",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        TAC.BOTHBIND$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND$ABC.BS.201 + TAC.BOTHBIND$ABC.BSAI.202 >= 1.50e6*1.1)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="NOSUR",fit = catch_BOTHBIND_loglin_nosur,TAC.BOTHBIND )
        CATCH.BOTHBIND.FLATSUR.SURSUR <- predict.catch.function(model="FLATSUR",fit=catch_BOTHBIND_FLATSUR_loglin_sur,TAC.BOTHBIND.FLATSUR )
        
    } else if (scenario == 3) {
        #flatfish dom
        TAC.BOTHBIND.FLATSUR <- predict.tac.function(predictmethod = 1,model="FLAT_FFDOM",fit=tac_BOTHBIND_FLATSUR_loglin_sur,FISH.DATA)
        TAC.BOTHBIND.FLATSUR$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND.FLATSUR$ABC.BS.201 + TAC.BOTHBIND.FLATSUR$ABC.BSAI.202 >= 1.50e6*0.9)
        
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1,model="SUR_FFDOM",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        TAC.BOTHBIND$ABCboth.UB.150 <- as.numeric(TAC.BOTHBIND$ABC.BS.201 + TAC.BOTHBIND$ABC.BSAI.202 >= 1.50e6*0.9)
        
        CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR_FFDOM",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
        CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="NOSUR_FFDOM",fit = catch_BOTHBIND_loglin_nosur,TAC.BOTHBIND )
        CATCH.BOTHBIND.FLATSUR.SURSUR <- predict.catch.function(model="FLATSUR_FFDOM",fit=catch_BOTHBIND_FLATSUR_loglin_sur,TAC.BOTHBIND.FLATSUR )
    }
    
    
    
    # create ensemble
    # 
    if (outputtype %in% c("c","C","catch","CATCH","Catch")) {
        CATCH.PRED <- (CATCH.BOTHBIND.SURSUR + CATCH.BOTHBIND.SUROLS + CATCH.BOTHBIND.FLATSUR.SURSUR)/3
        
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
    } else if (outputtype %in% c("TAC","t","T")) {  
        TAC.PRED <- (TAC.BOTHBIND + TAC.BOTHBIND + TAC.BOTHBIND.FLATSUR)/3
        
        output <- TAC.PRED[c("TAC.BSAI.141",
                             "TAC.BSAI.204",
                             "TAC.BSAI.103",
                             "TAC.BS.102",
                             "TAC.BSAI.147",
                             "TAC.BSAI.303",
                             "TAC.BSAI.60",
                             "TAC.BSAI.100",
                             "TAC.BS.310",
                             "TAC.BSAI.202",
                             "TAC.BSAI.106",
                             "TAC.BS.301",
                             "TAC.BS.201",
                             "TAC.BSAI.104",
                             "TAC.BSAI.307",
                             "TAC.BS.203",
                             "TAC.BSAI.400",
                             "TAC.BSAI.65",
                             "TAC.BSAI.326",
                             "TAC.BSAI.90",
                             "TAC.BSAI.50",
                             "TAC.BSAI.140")]

       
    }
    
    return (output)
}
