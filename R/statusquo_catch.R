statusquo_catch <- function(ABC.DATA) {
    tac.fit <- BSAI_TAC_fit
    
    ABC.DATA <- ABC.DATA %>%
        mutate(flex = 1) %>% # introduction of flatfish flex
        mutate(A80 = 1) %>%  # introduction of A80
        mutate(sablefish.over.6k = as.numeric(ABC.203>6e3)) %>%  # sablefish upper bound
        mutate(pcod.over.280k = as.numeric(ABC.202 >= 28e4)) %>% # pcod upper bound
        mutate(pollock.UB = as.numeric(ABC.201 >= 1.25e6 & A80 == 1) + as.numeric(ABC.201 >= 1.4e6 & A80==0)) %>% # consider a single UB?
        mutate(pollock.ABC = (1-pollock.UB)*ABC.201) %>% # Pollock ABC when not at UB
        mutate(SSL = 1)%>% # stellar sea lion closure
        mutate(solegone = 1) %>%
        mutate(plaicegone = 1) %>%
        mutate(kamsplit = 1) #%>% filter(YEAR == 2016)
    
    ##
    ##
    ## Predict how the NPFMC will set TAC based on the set of ABCs given
    ## 
    ##
    
    TAC.PRED <- data.frame(TAC.60 = NA)
    # Octopus
    TAC.PRED$TAC.60 <- predict(tac.fit[[1]], ABC.DATA)
    # Sharks
    TAC.PRED$TAC.65 <- predict(tac.fit[[2]], ABC.DATA)
    # Skates
    TAC.PRED$TAC.90 <- predict(tac.fit[[3]], ABC.DATA)
    # Sculpin
    TAC.PRED$TAC.400 <- predict(tac.fit[[4]], ABC.DATA)
    # Shortraker
    TAC.PRED$TAC.326 <- ABC.DATA$ABC.326
    # Rougheye
    TAC.PRED$TAC.307 <- ABC.DATA$ABC.307
    # Kamchatka
    TAC.PRED$TAC.147 <- predict(tac.fit[[5]], ABC.DATA)
    # POP
    TAC.PRED$TAC.301 <- predict(tac.fit[[6]], ABC.DATA)
    # Northern [note: tac.fit here is tobit]
    TAC.PRED$TAC.303 <- min(1,predict(tac.fit[[7]], ABC.DATA)[1])*ABC.DATA$ABC.303
    # Other Rockfish
    TAC.PRED$TAC.310 <- predict(tac.fit[[8]], ABC.DATA)
    # Greenland turbot
    TAC.PRED$TAC.102 <- predict(tac.fit[[9]], ABC.DATA)
    # Plaice
    TAC.PRED$TAC.106 <- predict(tac.fit[[10]], ABC.DATA)
    # Other FLatfish
    TAC.PRED$TAC.100 <- predict(tac.fit[[11]], ABC.DATA)
    # Flathead sole
    TAC.PRED$TAC.103 <- predict(tac.fit[[12]], ABC.DATA)
    
    
    # Rest [ The rest of the species are very connected, and were estimated using
    # a seemingly unrelated regression which correlates the errors between the different
    # equations, increasing efficiency (accuracy). ]
    
    Pred.SUR <-  predict(tac.fit[[13]], ABC.DATA)
    
    # Sablefish
    TAC.PRED$TAC.203 <- min(Pred.SUR$sablefish.pred, ABC.DATA$ABC.203)
    # Pollock
    TAC.PRED$TAC.201 <- min(Pred.SUR$pollock.pred, ABC.DATA$ABC.201)
    # PCod
    TAC.PRED$TAC.202 <- min(Pred.SUR$Pcod.pred, ABC.DATA$ABC.202)
    # Yellowfin
    TAC.PRED$TAC.140 <- min(Pred.SUR$yellowfin.pred, ABC.DATA$ABC.140)
    # Rock Sole
    TAC.PRED$TAC.104 <- min(Pred.SUR$rocksole.pred, ABC.DATA$ABC.104)
    # Atka
    TAC.PRED$TAC.204 <- min(Pred.SUR$atka.pred, ABC.DATA$ABC.204)
    # Squid
    TAC.PRED$TAC.50 <- Pred.SUR$squid.pred 
    # Arrowtooth
    TAC.PRED$TAC.141 <- Pred.SUR$arrowtooth.pred
    
    TAC.PRED <- round(TAC.PRED)
    
    TAC.PRED$NETTAC <- rowSums(TAC.PRED, na.rm = TRUE)
    TAC.PRED$EXCEEDS.CAP <- TAC.PRED$NETTAC > 2e6
    TAC.PRED$SURPLUS <- as.numeric(TAC.PRED$NETTAC > 2e6)*(TAC.PRED$NETTAC - 2e6)
    # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
    TAC.PRED$TAC.201 <- TAC.PRED$TAC.201 - ceiling(TAC.PRED$SURPLUS*0.5)
    TAC.PRED$TAC.140 <- TAC.PRED$TAC.140 - floor(TAC.PRED$SURPLUS*0.5)
    
    ##
    ##
    ## Now predict how the fishery will catch their allocated TAC
    ##
    ##
    
    catch.fit <- BSAI_CATCH_fit
    
    CATCH.PRED <- data.frame(CATCH.60 = NA)
 
    # Octopus
    CATCH.PRED$CATCH.60 <- predict(catch.fit[[1]], TAC.PRED)
    # Sharks
    CATCH.PRED$CATCH.65 <- predict(catch.fit[[2]], TAC.PRED)
    # Skates
    CATCH.PRED$CATCH.90 <- predict(catch.fit[[3]], TAC.PRED)
    # Sculpin
    CATCH.PRED$CATCH.400 <- predict(catch.fit[[4]], TAC.PRED)
    # Kamchatka
    CATCH.PRED$CATCH.147 <- predict(catch.fit[[5]], TAC.PRED)
    # POP
    CATCH.PRED$CATCH.301 <- predict(catch.fit[[6]], TAC.PRED)
    # Northern 
    CATCH.PRED$CATCH.303 <- predict(catch.fit[[7]], TAC.PRED)
    # Other Rockfish
    CATCH.PRED$CATCH.310 <- predict(catch.fit[[8]], TAC.PRED)
    # Greenland turbot
    CATCH.PRED$CATCH.102 <- predict(catch.fit[[9]], TAC.PRED)
    # Plaice
    CATCH.PRED$CATCH.106 <- predict(catch.fit[[10]], TAC.PRED)
    # Sablefish
    CATCH.PRED$CATCH.203 <- predict(catch.fit[[11]], TAC.PRED)
    # Pollock
    CATCH.PRED$CATCH.201 <- predict(catch.fit[[12]], TAC.PRED)
    # PCod
    CATCH.PRED$CATCH.202 <- predict(catch.fit[[13]], TAC.PRED)
    # Yellowfin
    CATCH.PRED$CATCH.140 <- predict(catch.fit[[14]], TAC.PRED)
    # Rock Sole
    CATCH.PRED$CATCH.104 <- predict(catch.fit[[15]], TAC.PRED)
    # Atka
    CATCH.PRED$CATCH.204 <- predict(catch.fit[[16]], TAC.PRED)
    # Squid
    CATCH.PRED$CATCH.50 <- predict(catch.fit[[17]], TAC.PRED)
    # Arrowtooth
    CATCH.PRED$CATCH.141 <- predict(catch.fit[[18]], TAC.PRED)
    # Other Flatfish
    CATCH.PRED$CATCH.100 <- predict(catch.fit[[19]], TAC.PRED)
    # Flathead sole, 
    CATCH.PRED$CATCH.103 <- predict(catch.fit[[20]], TAC.PRED)
    # Shortraker
    CATCH.PRED$CATCH.326 <- predict(catch.fit[[21]], TAC.PRED)
    # Rougheye
    CATCH.PRED$CATCH.307 <- predict(catch.fit[[22]], TAC.PRED)
    
    CATCH.PRED$NETTAC <- rowSums(CATCH.PRED, na.rm = TRUE)
    CATCH.PRED$EXCEEDS.CAP <- CATCH.PRED$NETTAC > 2e6
    CATCH.PRED$SURPLUS <- as.numeric(CATCH.PRED$NETTAC > 2e6)*(CATCH.PRED$NETTAC - 2e6)
    # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
    CATCH.PRED$CATCH.201 <- CATCH.PRED$CATCH.201 - CATCH.PRED$SURPLUS*0.5
    CATCH.PRED$CATCH.140 <- CATCH.PRED$CATCH.140 - CATCH.PRED$SURPLUS*0.5
    #
    # Probably want to change this--it really depends what is causing the cap to 
    # go over, as that fishery would be shut down... pollock and yellowfin are of 
    # course (most?) likely.
    #

    output <- CATCH.PRED[c("CATCH.141",
                           "CATCH.204",
                           "CATCH.103",
                           "CATCH.102",
                           "CATCH.147",
                           "CATCH.303",
                           "CATCH.60",
                           "CATCH.100",
                           "CATCH.310",
                           "CATCH.202",
                           "CATCH.106",
                           "CATCH.301",
                           "CATCH.201",
                           "CATCH.104",
                           "CATCH.307",
                           "CATCH.203",
                           "CATCH.400",
                           "CATCH.65",
                           "CATCH.326",
                           "CATCH.90",
                           "CATCH.50",
                           "CATCH.140")]
    return (output)
}