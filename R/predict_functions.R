predict.tac.function <- function(model,fit_sur,fit_nosur,FISH.DATA){
# Preamble ####  
    if (model == "SUR"| model == "NOFIRSTYEAR") {
        SUR<- T
        NOSUR <- F
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- F
        FLATSUR <- F
    }
    
    if (model == "NOSUR" ) {
        SUR<- F
        NOSUR <- T
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- F
        FLATSUR <- F
    }
    

 
    if (model == "NOROCKSOLE") {
        SUR<- F
        NOSUR <- F
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- T
        FLATSUR <- F
        } 
    
    if (model == "FLATSUR") {
        SUR<- F
        NOSUR <- F
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- F
        FLATSUR <- T
    }

    # make predictions  ####
    if (SUR) {
        PREDICTIONS <- data.frame(TAC.60=0)
        
        Pred.SUR <-  predict(fit_sur[[15]], FISH.DATA)
        
        # Octopus
        PREDICTIONS$TAC.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS$TAC.65 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS$TAC.90 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS$TAC.400 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS$TAC.50 <- pmin(Pred.SUR$squid.pred, FISH.DATA$ABC.50)
        
        # Shortraker
        PREDICTIONS$TAC.326 <- pmin(FISH.DATA$ABC.326, FISH.DATA$ABC.326)
        # Rougheye
        PREDICTIONS$TAC.307 <- pmin(predict(fit_sur[[14]],FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS$TAC.310 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern [note: fit here is tobit]
        PREDICTIONS$TAC.303 <- pmin(1,predict(fit_sur[[7]], FISH.DATA)[,1])*FISH.DATA$ABC.303
        # POP
        PREDICTIONS$TAC.301 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.301)
        
        
        # Pollock
        PREDICTIONS$TAC.201 <- pmin(Pred.SUR$pollock.pred , FISH.DATA$ABC.201)
        # PCod
        PREDICTIONS$TAC.202 <- pmin(Pred.SUR$Pcod.pred , FISH.DATA$ABC.202)
        # Sablefish
        PREDICTIONS$TAC.203 <- pmin(Pred.SUR$sablefish.pred , FISH.DATA$ABC.203)
        # Atka
        PREDICTIONS$TAC.204 <- pmin(Pred.SUR$atka.pred , FISH.DATA$ABC.204)
        
        
        # Yellowfin
        PREDICTIONS$TAC.140 <- pmin(Pred.SUR$yellowfin.pred , FISH.DATA$ABC.140)
        # Arrowtooth
        PREDICTIONS$TAC.141 <- pmin(1,predict(fit_sur[[13]], FISH.DATA)[,1])*FISH.DATA$ABC.141
        # Kamchatka
        PREDICTIONS$TAC.147 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.147)
        
        # Other FLatfish
        PREDICTIONS$TAC.100 <- pmin(1,predict(fit_sur[[11]], FISH.DATA)[,1])*FISH.DATA$ABC.100
        # Greenland turbot
        PREDICTIONS$TAC.102 <- pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.102)
        # Flathead sole
        PREDICTIONS$TAC.103 <- pmin(1,predict(fit_sur[[12]], FISH.DATA)[,1])*FISH.DATA$ABC.103
        # Rock Sole
        PREDICTIONS$TAC.104 <- pmin(Pred.SUR$rocksole.pred , FISH.DATA$ABC.104)
        # Plaice
        PREDICTIONS$TAC.106 <- pmin(1,predict(fit_sur[[10]], FISH.DATA))*FISH.DATA$ABC.106
        
        PREDICTIONS[PREDICTIONS<0] <- 0 #nothing negative allowed..

        # Rest [ The rest of the species are very connected, and were estimated using
   
        PREDICTIONS$NETTAC <- rowSums(PREDICTIONS[,1:22], na.rm = TRUE)
        PREDICTIONS$EXCEEDS.CAP <- PREDICTIONS$NETTAC > 2e6
        PREDICTIONS$SURPLUS <- as.numeric(PREDICTIONS$NETTAC > 2e6)*(PREDICTIONS$NETTAC - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS$TAC.201 <- PREDICTIONS$TAC.201 - PREDICTIONS$SURPLUS*0.5
        PREDICTIONS$TAC.140 <- PREDICTIONS$TAC.140 - PREDICTIONS$SURPLUS*0.5
    }
    
    if (NOSUR) { 
        PREDICTIONS_NOSUR <- data.frame(TAC.60=0)  #PTAC stands for predicted TAC
        
        # Octopus
        PREDICTIONS_NOSUR$TAC.60 <- pmin(predict(fit_nosur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS_NOSUR$TAC.65 <- pmin(predict(fit_nosur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS_NOSUR$TAC.90 <- pmin(predict(fit_nosur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS_NOSUR$TAC.400 <- pmin(predict(fit_nosur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS_NOSUR$TAC.50 <- pmin(predict(fit_nosur[[19]], FISH.DATA), FISH.DATA$ABC.50)
        
        # Shortraker
        PREDICTIONS_NOSUR$TAC.326 <- pmin(FISH.DATA$ABC.326, FISH.DATA$ABC.326)
        # Rougheye
        PREDICTIONS_NOSUR$TAC.307 <- pmin(predict(fit_nosur[[21]],FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS_NOSUR$TAC.310 <- pmin(predict(fit_nosur[[8]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern [note: fit_nosur here is tobit]
        PREDICTIONS_NOSUR$TAC.303 <- pmin(1,predict(fit_nosur[[7]], FISH.DATA)[,1])*FISH.DATA$ABC.303
        # POP
        PREDICTIONS_NOSUR$TAC.301 <- pmin(predict(fit_nosur[[6]], FISH.DATA), FISH.DATA$ABC.301)
        
        # Pollock
        PREDICTIONS_NOSUR$TAC.201 <- pmin(predict(fit_nosur[[14]], FISH.DATA), FISH.DATA$ABC.201)
        # PCod
        PREDICTIONS_NOSUR$TAC.202 <- pmin(predict(fit_nosur[[15]], FISH.DATA), FISH.DATA$ABC.202)
        # Sablefish
        PREDICTIONS_NOSUR$TAC.203 <- pmin(predict(fit_nosur[[13]], FISH.DATA), FISH.DATA$ABC.203)
        # Atka
        PREDICTIONS_NOSUR$TAC.204 <- pmin(predict(fit_nosur[[18]], FISH.DATA), FISH.DATA$ABC.204)
        
        # Yellowfin
        PREDICTIONS_NOSUR$TAC.140 <- pmin(predict(fit_nosur[[16]], FISH.DATA) , FISH.DATA$ABC.140)
        # Arrowtooth
        PREDICTIONS_NOSUR$TAC.141 <- pmin(1,predict(fit_nosur[[20]], FISH.DATA)[,1])*FISH.DATA$ABC.141
        # Kamchatka
        PREDICTIONS_NOSUR$TAC.147 <- pmin(predict(fit_nosur[[5]], FISH.DATA), FISH.DATA$ABC.147)
        
        # Other FLatfish
        PREDICTIONS_NOSUR$TAC.100 <- pmin(1,predict(fit_nosur[[11]], FISH.DATA)[,1])*FISH.DATA$ABC.100
        # Greenland turbot
        PREDICTIONS_NOSUR$TAC.102 <- pmin(predict(fit_nosur[[9]], FISH.DATA), FISH.DATA$ABC.102)
        # Flathead sole
        PREDICTIONS_NOSUR$TAC.103 <- pmin(1,predict(fit_nosur[[12]], FISH.DATA)[,1])*FISH.DATA$ABC.103
        # Rock Sole
        PREDICTIONS_NOSUR$TAC.104 <- pmin(predict(fit_nosur[[17]], FISH.DATA), FISH.DATA$ABC.104)
        # Plaice
        PREDICTIONS_NOSUR$TAC.106 <- pmin(1,predict(fit_sur[[10]], FISH.DATA))*FISH.DATA$ABC.106
        
        PREDICTIONS_NOSUR[PREDICTIONS_NOSUR<0] <- 0 #nothing negative allowed..
        
        PREDICTIONS_NOSUR$NETTAC <- rowSums(PREDICTIONS_NOSUR[,1:22], na.rm = TRUE)
        PREDICTIONS_NOSUR$EXCEEDS.CAP <- PREDICTIONS_NOSUR$NETTAC > 2e6
        PREDICTIONS_NOSUR$SURPLUS <- as.numeric(PREDICTIONS_NOSUR$NETTAC > 2e6)*(PREDICTIONS_NOSUR$NETTAC - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_NOSUR$TAC.201 <- PREDICTIONS_NOSUR$TAC.201 - PREDICTIONS_NOSUR$SURPLUS*0.5
        PREDICTIONS_NOSUR$TAC.140 <- PREDICTIONS_NOSUR$TAC.140 - PREDICTIONS_NOSUR$SURPLUS*0.5
    }
    
    if (NOROCKSOLE) {
        PREDICTIONS <- data.frame(TAC.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR <-  predict(fit_sur[[15]], FISH.DATA)
        
        
        # Octopus
        PREDICTIONS$TAC.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS$TAC.65 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS$TAC.90 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS$TAC.400 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS$TAC.50 <- pmin(Pred.SUR$squid.pred, FISH.DATA$ABC.50)
        
        # Shortraker
        PREDICTIONS$TAC.326 <- FISH.DATA$ABC.326
        # Rougheye
        PREDICTIONS$TAC.307 <- pmin(predict(fit_sur[[14]],FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS$TAC.310 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern [note: fit here is tobit]
        PREDICTIONS$TAC.303 <- pmin(1,predict(fit_sur[[7]], FISH.DATA)[,1])*FISH.DATA$ABC.303
        # POP
        PREDICTIONS$TAC.301 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.301)
        
        
        # Pollock
        PREDICTIONS$TAC.201 <- pmin(Pred.SUR$pollock.pred , FISH.DATA$ABC.201)
        # PCod
        PREDICTIONS$TAC.202 <- pmin(Pred.SUR$Pcod.pred , FISH.DATA$ABC.202)
        # Sablefish
        PREDICTIONS$TAC.203 <- pmin(Pred.SUR$sablefish.pred , FISH.DATA$ABC.203)
        # Atka
        PREDICTIONS$TAC.204 <- pmin(Pred.SUR$atka.pred , FISH.DATA$ABC.204)
        
        
        # Yellowfin
        PREDICTIONS$TAC.140 <- pmin(Pred.SUR$yellowfin.pred , FISH.DATA$ABC.140)
        # Arrowtooth
        PREDICTIONS$TAC.141 <- pmin(1,predict(fit_sur[[13]], FISH.DATA)[,1])*FISH.DATA$ABC.141
        # Kamchatka
        PREDICTIONS$TAC.147 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.147)
        
        # Other FLatfish
        PREDICTIONS$TAC.100 <- pmin(1,predict(fit_sur[[11]], FISH.DATA)[,1])*FISH.DATA$ABC.100
        # Greenland turbot
        PREDICTIONS$TAC.102 <- pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.102)
        # Flathead sole
        PREDICTIONS$TAC.103 <- pmin(1,predict(fit_sur[[12]], FISH.DATA)[,1])*FISH.DATA$ABC.103
        # Rock Sole
        PREDICTIONS$TAC.104 <- pmin(predict(fit_sur[[16]], FISH.DATA), FISH.DATA$ABC.104)
        # Plaice
        PREDICTIONS$TAC.106 <- pmin(1,predict(fit_sur[[10]], FISH.DATA))*FISH.DATA$ABC.106
        
        PREDICTIONS[PREDICTIONS<0] <- 0 #nothing negative allowed..
        # Rest [ The rest of the species are very connected, and were estimated using
   
        PREDICTIONS$NETTAC <- rowSums(PREDICTIONS[,1:22], na.rm = TRUE)
        PREDICTIONS$EXCEEDS.CAP <- PREDICTIONS$NETTAC > 2e6
        PREDICTIONS$SURPLUS <- as.numeric(PREDICTIONS$NETTAC > 2e6)*(PREDICTIONS$NETTAC - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS$TAC.201 <- PREDICTIONS$TAC.201 - PREDICTIONS$SURPLUS*0.5
        PREDICTIONS$TAC.140 <- PREDICTIONS$TAC.140 - PREDICTIONS$SURPLUS*0.5
    }
    
    if (FLATSUR) {
        PREDICTIONS <- data.frame(TAC.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR <-  predict(fit_sur[[12]], FISH.DATA)
        Pred.SUR.flat <- predict(fit_sur[[13]], FISH.DATA)
        
        
        # Octopus
        PREDICTIONS$TAC.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS$TAC.65 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS$TAC.90 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS$TAC.400 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS$TAC.50 <- pmin(Pred.SUR$squid.pred , FISH.DATA$ABC.50)
        
        # Shortraker
        PREDICTIONS$TAC.326 <- FISH.DATA$ABC.326
        # Rougheye
        PREDICTIONS$TAC.307 <- pmin(predict(fit_sur[[8]],FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS$TAC.310 <- pmin(predict(fit_sur[[9]],FISH.DATA), FISH.DATA$ABC.310)
        # Northern 
        PREDICTIONS$TAC.303 <- pmin(1,predict(fit_sur[[10]], FISH.DATA)[,1])*FISH.DATA$ABC.303
        # POP
        PREDICTIONS$TAC.301 <- pmin(predict(fit_sur[[11]],FISH.DATA), FISH.DATA$ABC.301)
        
        
        # Pollock
        PREDICTIONS$TAC.201 <- pmin(Pred.SUR$pollock.pred , FISH.DATA$ABC.201)
        # PCod
        PREDICTIONS$TAC.202 <- pmin(Pred.SUR$Pcod.pred , FISH.DATA$ABC.202)
        # Sablefish
        PREDICTIONS$TAC.203 <- pmin(Pred.SUR$sablefish.pred , FISH.DATA$ABC.203)
        # Atka
        PREDICTIONS$TAC.204 <- pmin(Pred.SUR$atka.pred , FISH.DATA$ABC.204)
        
        
        # Yellowfin
        PREDICTIONS$TAC.140 <- pmin(Pred.SUR$yellowfin.pred , FISH.DATA$ABC.140)
        # Arrowtooth
        PREDICTIONS$TAC.141 <- pmin(Pred.SUR.flat$arrowtooth.pred, FISH.DATA$ABC.141)
        # Kamchatka
        PREDICTIONS$TAC.147 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.147)
        
        # Other FLatfish
        PREDICTIONS$TAC.100 <- pmin(Pred.SUR.flat$Oflat.pred, FISH.DATA$ABC.100)
        # Greenland turbot
        PREDICTIONS$TAC.102 <- pmin(Pred.SUR.flat$Greenland.pred, FISH.DATA$ABC.102)
        # Flathead sole
        PREDICTIONS$TAC.103 <- pmin(1,predict(fit_sur[[7]], FISH.DATA)[,1])*FISH.DATA$ABC.103
        # Rock Sole
        PREDICTIONS$TAC.104 <- pmin(Pred.SUR.flat$rocksole.pred , FISH.DATA$ABC.104)
        # Plaice
        PREDICTIONS$TAC.106 <- pmin(1,predict(fit_sur[[6]], FISH.DATA))*FISH.DATA$ABC.106
        
        PREDICTIONS[PREDICTIONS<0] <- 0 #nothing negative allowed..
        # Rest [ The rest of the species are very connected, and were estimated using
   
        PREDICTIONS$NETTAC <- rowSums(PREDICTIONS[,1:22], na.rm = TRUE)
        PREDICTIONS$EXCEEDS.CAP <- PREDICTIONS$NETTAC > 2e6
        PREDICTIONS$SURPLUS <- as.numeric(PREDICTIONS$NETTAC > 2e6)*(PREDICTIONS$NETTAC - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS$TAC.201 <- PREDICTIONS$TAC.201 - PREDICTIONS$SURPLUS*0.5
        PREDICTIONS$TAC.140 <- PREDICTIONS$TAC.140 - PREDICTIONS$SURPLUS*0.5
    }
    
    
## Return predictions ####
    
    if (SUR | NOROCKSOLE | FLATSUR) {
        PREDICTIONS <- PREDICTIONS %>% mutate(YEAR = 1)
        FISH.DATA <- FISH.DATA %>% mutate(YEAR = 1)
        output <- full_join(PREDICTIONS,FISH.DATA, by = "YEAR")  %>% select(-YEAR)
        return(output)
    }
    
    if (NOSUR) {
        PREDICTIONS_NOSUR <- PREDICTIONS_NOSUR %>% mutate(YEAR = 1)
        FISH.DATA <- FISH.DATA %>% mutate(YEAR = 1)
        output <- full_join(PREDICTIONS_NOSUR,FISH.DATA, by = "YEAR")  %>% select(-YEAR)
        return(output)
    }
}

predict.catch.function <- function(model,fit_sur,fit_nosur,FISH.DATA) {
# Preamble ####   
     
        if (model == "SUR" | model == "NOFIRSTYEAR") {
        SUR<- T
        NOSUR <- F
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- F
        FLATSUR <- F
    }
    
    if (model == "NOSUR") {
        SUR<- F
        NOSUR <- T
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- F
        FLATSUR <- F
    }
    

    
    if (model == "NOROCKSOLE") {
        SUR<- F
        NOSUR <- F
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- T
        FLATSUR <- F
        } 
    
    if (model == "FLATSUR") {
        SUR<- F
        NOSUR <- F
        ADHOC <- F
        ADHOCMEAN <- F
        ADHOCNOIND <- F
        COMPARE <- F
        NOROCKSOLE <- F
        FLATSUR <- T
    }
    
    

# Make Predictions ####    
    if (NOSUR) {
        PREDICTIONS_NOSUR <- data.frame(CATCH.60=0)   #PTAC stands for predicted TAC
        
                 # Octopus
        PREDICTIONS_NOSUR$CATCH.60 <- pmin(predict(fit_nosur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS_NOSUR$CATCH.65 <- pmin(predict(fit_nosur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS_NOSUR$CATCH.90 <- pmin(predict(fit_nosur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS_NOSUR$CATCH.400 <- pmin(predict(fit_nosur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS_NOSUR$CATCH.50 <- pmin(predict(fit_nosur[[17]], FISH.DATA), FISH.DATA$ABC.50)
        
        
        # Shortraker
        PREDICTIONS_NOSUR$CATCH.326 <- pmin(predict(fit_nosur[[21]], FISH.DATA), FISH.DATA$ABC.326)
        # Rougheye
        PREDICTIONS_NOSUR$CATCH.307 <- pmin(predict(fit_nosur[[22]], FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS_NOSUR$CATCH.310 <- pmin(predict(fit_nosur[[8]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern
        PREDICTIONS_NOSUR$CATCH.303 <- pmin(predict(fit_nosur[[7]], FISH.DATA), FISH.DATA$ABC.303)
        # POP
        PREDICTIONS_NOSUR$CATCH.301 <- pmin(predict(fit_nosur[[6]], FISH.DATA), FISH.DATA$ABC.301)
        
        # Pollock
        PREDICTIONS_NOSUR$CATCH.201 <- pmin(predict(fit_nosur[[12]], FISH.DATA), FISH.DATA$TAC.201)
        # PCod
        PREDICTIONS_NOSUR$CATCH.202 <- pmin(predict(fit_nosur[[13]], FISH.DATA), FISH.DATA$TAC.202)
        # Sablefish
        PREDICTIONS_NOSUR$CATCH.203 <- pmin(predict(fit_nosur[[11]], FISH.DATA), FISH.DATA$TAC.203)
        # Atka
        PREDICTIONS_NOSUR$CATCH.204 <- pmin(predict(fit_nosur[[16]], FISH.DATA), FISH.DATA$TAC.204)
        
        # Yellowfin
        PREDICTIONS_NOSUR$CATCH.140 <- pmin(predict(fit_nosur[[14]], FISH.DATA), FISH.DATA$TAC.140)
        # Arrowtooth
        PREDICTIONS_NOSUR$CATCH.141 <- pmin(predict(fit_nosur[[18]], FISH.DATA), FISH.DATA$ABC.141)
        # Kamchatka
        PREDICTIONS_NOSUR$CATCH.147 <- pmin(predict(fit_nosur[[5]], FISH.DATA), FISH.DATA$ABC.147)

        # Other Flatfish
        PREDICTIONS_NOSUR$CATCH.100 <- pmin(predict(fit_nosur[[19]], FISH.DATA), FISH.DATA$ABC.100)
        # Greenland turbot
        PREDICTIONS_NOSUR$CATCH.102 <- pmin(predict(fit_nosur[[9]], FISH.DATA), FISH.DATA$ABC.102)
        # Flathead sole,
        PREDICTIONS_NOSUR$CATCH.103 <- pmin(predict(fit_nosur[[20]], FISH.DATA), FISH.DATA$ABC.103)        
        # Rock Sole
        PREDICTIONS_NOSUR$CATCH.104 <- pmin(predict(fit_nosur[[15]], FISH.DATA), FISH.DATA$ABC.104)      
        # Plaice
        PREDICTIONS_NOSUR$CATCH.106 <- pmin(predict(fit_nosur[[10]], FISH.DATA), FISH.DATA$ABC.106)

        PREDICTIONS_NOSUR[PREDICTIONS_NOSUR<0] <- 0 #nothing negative allowed..
        
        PREDICTIONS_NOSUR$NETCATCH <- rowSums(PREDICTIONS_NOSUR[,1:22], na.rm = TRUE)
        PREDICTIONS_NOSUR$EXCEEDS.CAP <- PREDICTIONS_NOSUR$NETCATCH > 2e6
        PREDICTIONS_NOSUR$SURPLUS <- as.numeric(PREDICTIONS_NOSUR$NETCATCH > 2e6)*(PREDICTIONS_NOSUR$NETCATCH - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_NOSUR$CATCH.201 <- PREDICTIONS_NOSUR$CATCH.201 - PREDICTIONS_NOSUR$SURPLUS*0.5
        PREDICTIONS_NOSUR$CATCH.140 <- PREDICTIONS_NOSUR$CATCH.140 - PREDICTIONS_NOSUR$SURPLUS*0.5  
        
        
    }  
    if (SUR) {
        PREDICTIONS_wSUR <- data.frame(CATCH.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR.A80 <- predict(fit_sur[[16]], FISH.DATA)
        Pred.SUR.AFA <- predict(fit_sur[[17]], FISH.DATA)
        
         # Octopus
        PREDICTIONS_wSUR$CATCH.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS_wSUR$CATCH.65 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS_wSUR$CATCH.90 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS_wSUR$CATCH.400 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS_wSUR$CATCH.50 <- pmin(Pred.SUR.AFA$squid.pred, FISH.DATA$ABC.50)
        
        
        # Shortraker
        PREDICTIONS_wSUR$CATCH.326 <- pmin(predict(fit_sur[[13]], FISH.DATA), FISH.DATA$ABC.326)
        # Rougheye
        PREDICTIONS_wSUR$CATCH.307 <- pmin(predict(fit_sur[[14]], FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS_wSUR$CATCH.310 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern
        PREDICTIONS_wSUR$CATCH.303 <- pmin(predict(fit_sur[[7]], FISH.DATA), FISH.DATA$ABC.303)
        # POP
        PREDICTIONS_wSUR$CATCH.301 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.301)
        
        # Pollock
        PREDICTIONS_wSUR$CATCH.201 <- pmin(Pred.SUR.AFA$pollock.pred, FISH.DATA$TAC.201)
        # PCod
        PREDICTIONS_wSUR$CATCH.202 <- pmin(Pred.SUR.AFA$Pcod.pred, FISH.DATA$TAC.202)
        # Sablefish
        PREDICTIONS_wSUR$CATCH.203 <- pmin(Pred.SUR.A80$sablefish.pred, FISH.DATA$TAC.203)
        # Atka
        PREDICTIONS_wSUR$CATCH.204 <- pmin(Pred.SUR.A80$atka.pred, FISH.DATA$TAC.204)
        
        # Yellowfin
        PREDICTIONS_wSUR$CATCH.140 <- pmin(Pred.SUR.A80$yellowfin.pred, FISH.DATA$TAC.140)
        # Arrowtooth
        PREDICTIONS_wSUR$CATCH.141 <- pmin(predict(fit_sur[[15]], FISH.DATA), FISH.DATA$ABC.141)
        # Kamchatka
        PREDICTIONS_wSUR$CATCH.147 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.147)

        # Other Flatfish
        PREDICTIONS_wSUR$CATCH.100 <- pmin(predict(fit_sur[[11]], FISH.DATA), FISH.DATA$ABC.100)
        # Greenland turbot
        PREDICTIONS_wSUR$CATCH.102 <- pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.102)
        # Flathead sole,
        PREDICTIONS_wSUR$CATCH.103 <- pmin(predict(fit_sur[[12]], FISH.DATA), FISH.DATA$ABC.103)        
        # Rock Sole
        PREDICTIONS_wSUR$CATCH.104 <- pmin(Pred.SUR.A80$rocksole.pred, FISH.DATA$ABC.104)      
        # Plaice
        PREDICTIONS_wSUR$CATCH.106 <- pmin(predict(fit_sur[[10]], FISH.DATA), FISH.DATA$ABC.106)

        
        PREDICTIONS_wSUR[PREDICTIONS_wSUR<0] <- 0 #nothing negative allowed..

        PREDICTIONS_wSUR$NETCATCH <- rowSums(PREDICTIONS_wSUR[,1:22], na.rm = TRUE)
        PREDICTIONS_wSUR$EXCEEDS.CAP <- PREDICTIONS_wSUR$NETCATCH > 2e6
        PREDICTIONS_wSUR$SURPLUS <- as.numeric(PREDICTIONS_wSUR$NETCATCH > 2e6)*(PREDICTIONS_wSUR$NETCATCH - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_wSUR$CATCH.201 <- PREDICTIONS_wSUR$CATCH.201 - PREDICTIONS_wSUR$SURPLUS*0.5
        PREDICTIONS_wSUR$CATCH.140 <- PREDICTIONS_wSUR$CATCH.140 - PREDICTIONS_wSUR$SURPLUS*0.5
        
    }

    if (NOROCKSOLE) {
        PREDICTIONS_wSUR <- data.frame(CATCH.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR.A80 <- predict(fit_sur[[16]], FISH.DATA)
        Pred.SUR.AFA <- predict(fit_sur[[17]], FISH.DATA)
        
        # Octopus
        PREDICTIONS_wSUR$CATCH.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS_wSUR$CATCH.65 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS_wSUR$CATCH.90 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS_wSUR$CATCH.400 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS_wSUR$CATCH.50 <- pmin(Pred.SUR.AFA$squid.pred, FISH.DATA$ABC.50)
        
        
        # Shortraker
        PREDICTIONS_wSUR$CATCH.326 <- pmin(predict(fit_sur[[13]], FISH.DATA), FISH.DATA$ABC.326)
        # Rougheye
        PREDICTIONS_wSUR$CATCH.307 <- pmin(predict(fit_sur[[14]], FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS_wSUR$CATCH.310 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern
        PREDICTIONS_wSUR$CATCH.303 <- pmin(predict(fit_sur[[7]], FISH.DATA), FISH.DATA$ABC.303)
        # POP
        PREDICTIONS_wSUR$CATCH.301 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.301)
        
        # Pollock
        PREDICTIONS_wSUR$CATCH.201 <- pmin(Pred.SUR.AFA$pollock.pred, FISH.DATA$TAC.201)
        # PCod
        PREDICTIONS_wSUR$CATCH.202 <- pmin(Pred.SUR.AFA$Pcod.pred, FISH.DATA$TAC.202)
        # Sablefish
        PREDICTIONS_wSUR$CATCH.203 <- pmin(Pred.SUR.A80$sablefish.pred, FISH.DATA$TAC.203)
        # Atka
        PREDICTIONS_wSUR$CATCH.204 <- pmin(Pred.SUR.A80$atka.pred, FISH.DATA$TAC.204)
        
        # Yellowfin
        PREDICTIONS_wSUR$CATCH.140 <- pmin(Pred.SUR.A80$yellowfin.pred, FISH.DATA$TAC.140)
        # Arrowtooth
        PREDICTIONS_wSUR$CATCH.141 <- pmin(predict(fit_sur[[15]], FISH.DATA), FISH.DATA$ABC.141)
        # Kamchatka
        PREDICTIONS_wSUR$CATCH.147 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.147)

        # Other Flatfish
        PREDICTIONS_wSUR$CATCH.100 <- pmin(predict(fit_sur[[11]], FISH.DATA), FISH.DATA$ABC.100)
        # Greenland turbot
        PREDICTIONS_wSUR$CATCH.102 <- pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.102)
        # Flathead sole,
        PREDICTIONS_wSUR$CATCH.103 <- pmin(predict(fit_sur[[12]], FISH.DATA), FISH.DATA$ABC.103)        
        # Rock Sole
        PREDICTIONS_wSUR$CATCH.104 <- pmin(predict(fit_sur[[18]], FISH.DATA), FISH.DATA$ABC.104)      
        # Plaice
        PREDICTIONS_wSUR$CATCH.106 <- pmin(predict(fit_sur[[10]], FISH.DATA), FISH.DATA$ABC.106)
        
        PREDICTIONS_wSUR[PREDICTIONS_wSUR<0] <- 0 #nothing negative allowed..

        PREDICTIONS_wSUR$NETCATCH <- rowSums(PREDICTIONS_wSUR[,1:22], na.rm = TRUE)
        PREDICTIONS_wSUR$EXCEEDS.CAP <- PREDICTIONS_wSUR$NETCATCH > 2e6
        PREDICTIONS_wSUR$SURPLUS <- as.numeric(PREDICTIONS_wSUR$NETCATCH > 2e6)*(PREDICTIONS_wSUR$NETCATCH - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_wSUR$CATCH.201 <- PREDICTIONS_wSUR$CATCH.201 - PREDICTIONS_wSUR$SURPLUS*0.5
        PREDICTIONS_wSUR$CATCH.140 <- PREDICTIONS_wSUR$CATCH.140 - PREDICTIONS_wSUR$SURPLUS*0.5
        
    }
    
    if (FLATSUR) {
        PREDICTIONS_wSUR <- data.frame(CATCH.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR.A80 <- predict(fit_sur[[13]], FISH.DATA)
        Pred.SUR.AFA <- predict(fit_sur[[14]], FISH.DATA)
        Pred.SUR.flat <- predict(fit_sur[[15]], FISH.DATA)
        
        # Octopus
        PREDICTIONS_wSUR$CATCH.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.60)
        # Sharks
        PREDICTIONS_wSUR$CATCH.65 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.65)
        # Skates
        PREDICTIONS_wSUR$CATCH.90 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.90)
        # Sculpin
        PREDICTIONS_wSUR$CATCH.400 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.400)
        # Squid
        PREDICTIONS_wSUR$CATCH.50 <- pmin(Pred.SUR.AFA$squid.pred, FISH.DATA$ABC.50)
        
        
         # Shortraker
        PREDICTIONS_wSUR$CATCH.326 <- pmin(predict(fit_sur[[12]], FISH.DATA), FISH.DATA$ABC.326)
        # Rougheye
        PREDICTIONS_wSUR$CATCH.307 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.307)
        # Other Rockfish
        PREDICTIONS_wSUR$CATCH.310 <- pmin(predict(fit_sur[[11]], FISH.DATA), FISH.DATA$ABC.310)
        # Northern
        PREDICTIONS_wSUR$CATCH.303 <- pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.303)
        # POP
        PREDICTIONS_wSUR$CATCH.301 <- pmin(predict(fit_sur[[10]], FISH.DATA), FISH.DATA$ABC.301)
        
        # Pollock
        PREDICTIONS_wSUR$CATCH.201 <- pmin(Pred.SUR.AFA$pollock.pred, FISH.DATA$TAC.201)
        # PCod
        PREDICTIONS_wSUR$CATCH.202 <- pmin(Pred.SUR.AFA$Pcod.pred, FISH.DATA$TAC.202)
        # Sablefish
        PREDICTIONS_wSUR$CATCH.203 <- pmin(Pred.SUR.A80$sablefish.pred, FISH.DATA$TAC.203)
        # Atka
        PREDICTIONS_wSUR$CATCH.204 <- pmin(Pred.SUR.A80$atka.pred, FISH.DATA$TAC.204)
        
        # Yellowfin
        PREDICTIONS_wSUR$CATCH.140 <- pmin(Pred.SUR.A80$yellowfin.pred, FISH.DATA$TAC.140)
        # Arrowtooth
        PREDICTIONS_wSUR$CATCH.141 <- pmin(Pred.SUR.flat$arrowtooth.pred, FISH.DATA$ABC.141)
        # Kamchatka
        PREDICTIONS_wSUR$CATCH.147 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.147)

        # Other Flatfish
        PREDICTIONS_wSUR$CATCH.100 <- pmin(Pred.SUR.flat$Oflat.pred , FISH.DATA$ABC.100)
        # Greenland turbot
        PREDICTIONS_wSUR$CATCH.102 <- pmin(Pred.SUR.flat$Greenland.pred , FISH.DATA$ABC.102)
        # Flathead sole,
        PREDICTIONS_wSUR$CATCH.103 <- pmin(predict(fit_sur[[7]], FISH.DATA), FISH.DATA$ABC.103)        
        # Rock Sole
        PREDICTIONS_wSUR$CATCH.104 <- pmin(Pred.SUR.flat$rocksole.pred, FISH.DATA$ABC.104)      
        # Plaice
        PREDICTIONS_wSUR$CATCH.106 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.106)

        PREDICTIONS_wSUR[PREDICTIONS_wSUR<0] <- 0 #nothing negative allowed..

        PREDICTIONS_wSUR$NETCATCH <- rowSums(PREDICTIONS_wSUR[,1:22], na.rm = TRUE)
        PREDICTIONS_wSUR$EXCEEDS.CAP <- PREDICTIONS_wSUR$NETCATCH > 2e6
        PREDICTIONS_wSUR$SURPLUS <- as.numeric(PREDICTIONS_wSUR$NETCATCH > 2e6)*(PREDICTIONS_wSUR$NETCATCH - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_wSUR$CATCH.201 <- PREDICTIONS_wSUR$CATCH.201 - PREDICTIONS_wSUR$SURPLUS*0.5
        PREDICTIONS_wSUR$CATCH.140 <- PREDICTIONS_wSUR$CATCH.140 - PREDICTIONS_wSUR$SURPLUS*0.5
        
    }
    

    
    if (SUR | NOROCKSOLE | FLATSUR) {
        return(PREDICTIONS_wSUR)
    }
    
    if (NOSUR) {
        return(PREDICTIONS_NOSUR)
    }
}