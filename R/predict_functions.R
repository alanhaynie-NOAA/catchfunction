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
        PREDICTIONS <- data.frame(TAC.BSAI.60=0)
        
        Pred.SUR <-  predict(fit_sur[[16]], FISH.DATA)
        
        
        # Octopus
        PREDICTIONS$TAC.BSAI.60 <- pmin(predict(fit_sur[[1]], FISH.DATA),FISH.DATA$ABC.BSAI.60)
        # Sharks
        PREDICTIONS$TAC.BSAI.65 <- pmin(predict(fit_sur[[2]], FISH.DATA),FISH.DATA$ABC.BSAI.65)
        # Skates
        PREDICTIONS$TAC.BSAI.90 <- pmin(predict(fit_sur[[3]], FISH.DATA),FISH.DATA$ABC.BSAI.90)
        # Sculpin
        PREDICTIONS$TAC.BSAI.400 <- pmin(predict(fit_sur[[4]], FISH.DATA),FISH.DATA$ABC.BSAI.400)
        # Squid
        PREDICTIONS$TAC.BSAI.50 <- pmin(Pred.SUR$squid.pred,FISH.DATA$ABC.BSAI.50)
        
        # Shortraker
        PREDICTIONS$TAC.BSAI.326 <- FISH.DATA$ABC.BSAI.326
        # Rougheye
        PREDICTIONS$TAC.BSAI.307 <- pmin(predict(fit_sur[[15]],FISH.DATA),FISH.DATA$ABC.BSAI.307)
        # Other Rockfish
        PREDICTIONS$TAC.BS.310 <- pmin(predict(fit_sur[[8]], FISH.DATA),FISH.DATA$ABC.BS.310)
        PREDICTIONS$TAC.AI.310 <- FISH.DATA$ABC.AI.310
        # Northern [note: fit here is tobit]
        #PREDICTIONS$TAC.BSAI.303 <- pmin(1,predict(fit_sur[[7]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.303
        PREDICTIONS$TAC.BSAI.303 <- pmin(predict(fit_nosur[[7]], FISH.DATA), FISH.DATA$ABC.BSAI.303)
        # POP
        PREDICTIONS$TAC.BS.301 <- pmin(predict(fit_sur[[6]], FISH.DATA),FISH.DATA$ABC.BS.301)
        PREDICTIONS$TAC.AI.301 <- FISH.DATA$ABC.AI.301
        
        # Pollock
        PREDICTIONS$TAC.BS.201 <- pmin(Pred.SUR$pollock.pred ,FISH.DATA$ABC.BS.201)
        PREDICTIONS$TAC.AI.201 <- pmin(predict(fit_sur[[18]], FISH.DATA),FISH.DATA$ABC.AI.201)
        # PCod
        PREDICTIONS$TAC.BSAI.202 <- pmin(Pred.SUR$Pcod.pred ,FISH.DATA$ABC.BSAI.202)
        # Sablefish
        PREDICTIONS$TAC.BS.203 <- pmin(Pred.SUR$sablefish.pred ,FISH.DATA$ABC.BS.203)
        PREDICTIONS$TAC.AI.203 <- pmin(predict(fit_sur[[17]], FISH.DATA),FISH.DATA$ABC.AI.203)
        # Atka
        PREDICTIONS$TAC.BSAI.204 <- pmin(Pred.SUR$atka.pred ,FISH.DATA$ABC.BSAI.204)
        
        
        # Yellowfin
        PREDICTIONS$TAC.BSAI.140 <- pmin(Pred.SUR$yellowfin.pred ,FISH.DATA$ABC.BSAI.140)
        # Arrowtooth
        PREDICTIONS$TAC.BSAI.141 <- pmin(1,predict(fit_sur[[14]], FISH.DATA)[1])*FISH.DATA$ABC.BSAI.141
        # Kamchatka
        PREDICTIONS$TAC.BSAI.147 <- pmin(predict(fit_sur[[5]], FISH.DATA),FISH.DATA$ABC.BSAI.147)
        
        # Other FLatfish
        PREDICTIONS$TAC.BSAI.100 <- pmin(1,predict(fit_sur[[12]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.100
        # Greenland turbot
        PREDICTIONS$TAC.BS.102 <-  pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.BS.102)
        PREDICTIONS$TAC.AI.102 <-  pmin(predict(fit_sur[[10]], FISH.DATA), FISH.DATA$ABC.AI.102)
        # Flathead sole
        PREDICTIONS$TAC.BSAI.103 <- pmin(1,predict(fit_sur[[13]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.103
        # Rock Sole
        PREDICTIONS$TAC.BSAI.104 <- pmin(Pred.SUR$rocksole.pred , FISH.DATA$ABC.BSAI.104)
        # Plaice
        PREDICTIONS$TAC.BSAI.106 <- pmin(1,predict(fit_sur[[11]], FISH.DATA))*FISH.DATA$ABC.BSAI.106
        
        PREDICTIONS[PREDICTIONS<0] <- 0 #nothing negative allowed..

        # Rest [ The rest of the species are very connected, and were estimated using
   
        PREDICTIONS$NETTAC <- rowSums(PREDICTIONS[,1:22], na.rm = TRUE)
        PREDICTIONS$EXCEEDS.CAP <- PREDICTIONS$NETTAC > 2e6
        PREDICTIONS$SURPLUS <- as.numeric(PREDICTIONS$NETTAC > 2e6)*(PREDICTIONS$NETTAC - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS$TAC.BS.201 <- PREDICTIONS$TAC.BS.201 - PREDICTIONS$SURPLUS*0.5
        PREDICTIONS$TAC.BSAI.140 <- PREDICTIONS$TAC.BSAI.140 - PREDICTIONS$SURPLUS*0.5
    }
    
    if (NOSUR) { 
        PREDICTIONS_NOSUR <- data.frame(TAC.BSAI.60=0)  #PTAC stands for predicted TAC
        
        # Octopus
        PREDICTIONS_NOSUR$TAC.BSAI.60 <- pmin(predict(fit_nosur[[1]], FISH.DATA), FISH.DATA$ABC.BSAI.60)
        # Sharks
        PREDICTIONS_NOSUR$TAC.BSAI.65 <- pmin(predict(fit_nosur[[2]], FISH.DATA), FISH.DATA$ABC.BSAI.65)
        # Skates
        PREDICTIONS_NOSUR$TAC.BSAI.90 <- pmin(predict(fit_nosur[[3]], FISH.DATA), FISH.DATA$ABC.BSAI.90)
        # Sculpin
        PREDICTIONS_NOSUR$TAC.BSAI.400 <- pmin(predict(fit_nosur[[4]], FISH.DATA), FISH.DATA$ABC.BSAI.400)
        # Squid
        PREDICTIONS_NOSUR$TAC.BSAI.50 <- pmin(predict(fit_nosur[[22]], FISH.DATA), FISH.DATA$ABC.BSAI.50)
        
        # Shortraker
        PREDICTIONS_NOSUR$TAC.BSAI.326 <- FISH.DATA$ABC.BSAI.326
        # Rougheye
        PREDICTIONS_NOSUR$TAC.BSAI.307 <- pmin(predict(fit_nosur[[24]],FISH.DATA), FISH.DATA$ABC.BSAI.307)
        # Other Rockfish
        PREDICTIONS_NOSUR$TAC.BS.310 <- pmin(predict(fit_nosur[[8]], FISH.DATA), FISH.DATA$ABC.BS.310)
        PREDICTIONS_NOSUR$TAC.AI.310 <- FISH.DATA$ABC.AI.310
        # Northern [note: fit_nosur here is tobit]
        PREDICTIONS_NOSUR$TAC.BSAI.303 <- pmin(predict(fit_nosur[[7]], FISH.DATA), FISH.DATA$ABC.BSAI.303) # pmin(1,predict(fit_nosur[[7]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.303
        # POP
        PREDICTIONS_NOSUR$TAC.BS.301 <- pmin(predict(fit_nosur[[6]], FISH.DATA), FISH.DATA$ABC.BS.301)
        PREDICTIONS_NOSUR$TAC.AI.301 <- FISH.DATA$ABC.AI.301
        
        # Pollock
        PREDICTIONS_NOSUR$TAC.BS.201 <- pmin(predict(fit_nosur[[16]], FISH.DATA), FISH.DATA$ABC.BS.201)
        PREDICTIONS_NOSUR$TAC.AI.201 <- pmin(predict(fit_sur[[17]], FISH.DATA),FISH.DATA$ABC.BSAI.201)
        # PCod
        PREDICTIONS_NOSUR$TAC.BSAI.202 <- pmin(predict(fit_nosur[[18]], FISH.DATA), FISH.DATA$ABC.BSAI.202)
        # Sablefish
        PREDICTIONS_NOSUR$TAC.BS.203 <- pmin(predict(fit_nosur[[14]], FISH.DATA), FISH.DATA$ABC.BS.203)
        PREDICTIONS_NOSUR$TAC.AI.203 <- pmin(predict(fit_nosur[[15]], FISH.DATA), FISH.DATA$ABC.AI.203)
        # Atka
        PREDICTIONS_NOSUR$TAC.BSAI.204 <- pmin(predict(fit_nosur[[21]], FISH.DATA), FISH.DATA$ABC.BSAI.204)
      #  PREDICTIONS_NOSUR$TAC.BSAI.204 <- pmin(1,predict(fit_nosur[[21]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.204
        
        # Yellowfin
        PREDICTIONS_NOSUR$TAC.BSAI.140 <- pmin(predict(fit_nosur[[19]], FISH.DATA) , FISH.DATA$ABC.BSAI.140)
        # Arrowtooth
        PREDICTIONS_NOSUR$TAC.BSAI.141 <- pmin(1,predict(fit_nosur[[23]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.141
        # Kamchatka
        PREDICTIONS_NOSUR$TAC.BSAI.147 <- pmin(predict(fit_nosur[[5]], FISH.DATA), FISH.DATA$ABC.BSAI.147)
        
        # Other FLatfish
        PREDICTIONS_NOSUR$TAC.BSAI.100 <- pmin(1,predict(fit_nosur[[12]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.100
        # Greenland turbot
        PREDICTIONS_NOSUR$TAC.BS.102 <- pmin(predict(fit_nosur[[9]], FISH.DATA), FISH.DATA$ABC.BS.102)
        PREDICTIONS_NOSUR$TAC.AI.102 <- pmin(predict(fit_nosur[[10]], FISH.DATA), FISH.DATA$ABC.AI.102)
        # Flathead sole
        PREDICTIONS_NOSUR$TAC.BSAI.103 <- pmin(1,predict(fit_nosur[[13]], FISH.DATA)[,1])*FISH.DATA$ABC.BSAI.103
        # Rock Sole
        PREDICTIONS_NOSUR$TAC.BSAI.104 <- pmin(predict(fit_nosur[[20]], FISH.DATA), FISH.DATA$ABC.BSAI.104)
        # Plaice
        PREDICTIONS_NOSUR$TAC.BSAI.106 <- predict(fit_sur[[11]], FISH.DATA)*FISH.DATA$ABC.BSAI.106
        
        PREDICTIONS_NOSUR[PREDICTIONS_NOSUR<0] <- 0 #nothing negative allowed..
        
        
        PREDICTIONS_NOSUR$NETTAC <- rowSums(PREDICTIONS_NOSUR[,1:22], na.rm = TRUE)
        PREDICTIONS_NOSUR$EXCEEDS.CAP <- PREDICTIONS_NOSUR$NETTAC > 2e6
        PREDICTIONS_NOSUR$SURPLUS <- as.numeric(PREDICTIONS_NOSUR$NETTAC > 2e6)*(PREDICTIONS_NOSUR$NETTAC - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_NOSUR$TAC.BS.201 <- PREDICTIONS_NOSUR$TAC.BS.201 - PREDICTIONS_NOSUR$SURPLUS*0.5
        PREDICTIONS_NOSUR$TAC.BSAI.140 <- PREDICTIONS_NOSUR$TAC.BSAI.140 - PREDICTIONS_NOSUR$SURPLUS*0.5
    }
    

    
    
## Return predictions ####
    
    if (SUR) {
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
        PREDICTIONS_NOSUR <- data.frame(CATCH.BS.60=0)   #PTAC stands for predicted TAC
        
         # Octopus
        PREDICTIONS_NOSUR$CATCH.BS.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.BSAI.60)
        PREDICTIONS_NOSUR$CATCH.AI.60 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.BSAI.60)
        # Sharks
        PREDICTIONS_NOSUR$CATCH.BS.65 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.BSAI.65)
        PREDICTIONS_NOSUR$CATCH.AI.65 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.BSAI.65)
        # Skates
        PREDICTIONS_NOSUR$CATCH.BS.90 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.BSAI.90)
        PREDICTIONS_NOSUR$CATCH.AI.90 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.BSAI.90)
        # Sculpin
        PREDICTIONS_NOSUR$CATCH.BS.400 <- pmin(predict(fit_sur[[7]], FISH.DATA), FISH.DATA$ABC.BSAI.400)
        PREDICTIONS_NOSUR$CATCH.AI.400 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.BSAI.400)
        # Squid
        PREDICTIONS_NOSUR$CATCH.BS.50 <- pmin(predict(fit_nosur[[36]], FISH.DATA), FISH.DATA$ABC.BSAI.50)
        PREDICTIONS_NOSUR$CATCH.AI.50 <- pmin(predict(fit_nosur[[37]], FISH.DATA), FISH.DATA$ABC.BSAI.50)
        
        
        # Shortraker
        PREDICTIONS_NOSUR$CATCH.BS.326 <- pmin(predict(fit_sur[[24]], FISH.DATA), FISH.DATA$ABC.BSAI.326)
        PREDICTIONS_NOSUR$CATCH.AI.326 <- pmin(predict(fit_sur[[25]], FISH.DATA), FISH.DATA$ABC.BSAI.326)
        # Rougheye
        PREDICTIONS_NOSUR$CATCH.BS.307 <- pmin(predict(fit_sur[[26]], FISH.DATA), FISH.DATA$ABC.BSAI.307)
        PREDICTIONS_NOSUR$CATCH.AI.307 <- pmin(predict(fit_sur[[27]], FISH.DATA), FISH.DATA$ABC.BSAI.307)
        # Other Rockfish
        PREDICTIONS_NOSUR$CATCH.BS.310 <- pmin(predict(fit_sur[[15]], FISH.DATA), FISH.DATA$ABC.BS.310)
        PREDICTIONS_NOSUR$CATCH.AI.310 <- pmin(predict(fit_sur[[16]], FISH.DATA), FISH.DATA$ABC.AI.310)
        # Northern
        PREDICTIONS_NOSUR$CATCH.BS.303 <- pmin(predict(fit_sur[[13]], FISH.DATA), FISH.DATA$ABC.BSAI.303)
        PREDICTIONS_NOSUR$CATCH.AI.303 <- pmin(predict(fit_sur[[14]], FISH.DATA), FISH.DATA$ABC.BSAI.303)
        # POP
        PREDICTIONS_NOSUR$CATCH.BS.301 <- pmin(predict(fit_sur[[11]], FISH.DATA), FISH.DATA$ABC.BS.301)
        PREDICTIONS_NOSUR$CATCH.AI.301 <- pmin(predict(fit_sur[[12]], FISH.DATA), FISH.DATA$ABC.AI.301)
        
        # Pollock
        PREDICTIONS_NOSUR$CATCH.BS.201 <- pmin(predict(fit_nosur[[34]], FISH.DATA), FISH.DATA$ABC.BS.201)
        PREDICTIONS_NOSUR$CATCH.AI.201 <- pmin(predict(fit_nosur[[35]], FISH.DATA), FISH.DATA$ABC.AI.201)
        # PCod
        PREDICTIONS_NOSUR$CATCH.BS.202 <- pmin(predict(fit_nosur[[32]], FISH.DATA), FISH.DATA$TAC.BSAI.202)
        PREDICTIONS_NOSUR$CATCH.AI.202 <- pmin(predict(fit_nosur[[33]], FISH.DATA), FISH.DATA$TAC.BSAI.202)
        # Sablefish
        PREDICTIONS_NOSUR$CATCH.BS.203 <- pmin(predict(fit_nosur[[39]], FISH.DATA), FISH.DATA$TAC.BS.203)
        PREDICTIONS_NOSUR$CATCH.AI.203 <- pmin(predict(fit_nosur[[40]], FISH.DATA), FISH.DATA$TAC.AI.203)
        # Atka
        PREDICTIONS_NOSUR$CATCH.BS.204 <- pmin(predict(fit_nosur[[30]], FISH.DATA), FISH.DATA$TAC.BSAI.204)
        PREDICTIONS_NOSUR$CATCH.AI.204 <- pmin(predict(fit_nosur[[31]], FISH.DATA), FISH.DATA$TAC.BSAI.204)
        #PREDICTIONS_NOSUR$CATCH.AI.204 <- predict(fit_nosur[[31]], FISH.DATA)*FISH.DATA$TAC.BSAI.204
        
        # Yellowfin
        PREDICTIONS_NOSUR$CATCH.BS.140 <- pmin(predict(fit_nosur[[38]], FISH.DATA), FISH.DATA$TAC.BSAI.140) # ACTUALLY BSAI--AI = 0
        # Arrowtooth
        PREDICTIONS_NOSUR$CATCH.BS.141 <- pmin(predict(fit_nosur[[28]], FISH.DATA), FISH.DATA$ABC.BSAI.141)
        PREDICTIONS_NOSUR$CATCH.AI.141 <- pmin(predict(fit_nosur[[29]], FISH.DATA), FISH.DATA$ABC.BSAI.141)
        # Kamchatka
        PREDICTIONS_NOSUR$CATCH.BS.147 <- pmin(predict(fit_nosur[[9]], FISH.DATA), FISH.DATA$ABC.BSAI.147)
        PREDICTIONS_NOSUR$CATCH.AI.147 <- pmin(predict(fit_nosur[[10]], FISH.DATA), FISH.DATA$ABC.BSAI.147)
        
        # Other Flatfish
        PREDICTIONS_NOSUR$CATCH.BS.100 <- pmin(predict(fit_sur[[20]], FISH.DATA), FISH.DATA$ABC.BSAI.100)
        PREDICTIONS_NOSUR$CATCH.AI.100 <- pmin(predict(fit_sur[[21]], FISH.DATA), FISH.DATA$ABC.BSAI.100)
        # Greenland turbot
        PREDICTIONS_NOSUR$CATCH.BS.102 <- pmin(predict(fit_sur[[17]], FISH.DATA), FISH.DATA$ABC.BS.102)
        PREDICTIONS_NOSUR$CATCH.AI.102 <- pmin(predict(fit_sur[[18]], FISH.DATA), FISH.DATA$ABC.AI.102)
        # Flathead sole,
        PREDICTIONS_NOSUR$CATCH.BS.103 <- pmin(predict(fit_sur[[22]], FISH.DATA), FISH.DATA$ABC.BSAI.103)
        PREDICTIONS_NOSUR$CATCH.AI.103 <- pmin(predict(fit_sur[[23]], FISH.DATA), FISH.DATA$ABC.BSAI.103)        
        # Rock Sole
        PREDICTIONS_NOSUR$CATCH.BS.104 <- pmin(predict(fit_nosur[[41]], FISH.DATA), FISH.DATA$ABC.BSAI.104)
        PREDICTIONS_NOSUR$CATCH.AI.104 <- pmin(predict(fit_nosur[[42]], FISH.DATA), FISH.DATA$ABC.BSAI.104)      
        # Plaice
        PREDICTIONS_NOSUR$CATCH.BS.106 <- pmin(predict(fit_sur[[19]], FISH.DATA), FISH.DATA$ABC.BSAI.106)# ACTUALLY BSAI--AI = 0
        
        PREDICTIONS_NOSUR[PREDICTIONS_NOSUR<0] <- 0 #nothing negative allowed..
        
        PREDICTIONS_NOSUR$NETCATCH <- rowSums(PREDICTIONS_NOSUR[,2:23], na.rm = TRUE)
        PREDICTIONS_NOSUR$EXCEEDS.CAP <- PREDICTIONS_NOSUR$NETCATCH > 2e6
        PREDICTIONS_NOSUR$SURPLUS <- as.numeric(PREDICTIONS_NOSUR$NETCATCH > 2e6)*(PREDICTIONS_NOSUR$NETCATCH - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_NOSUR$CATCH.BS.201 <- PREDICTIONS_NOSUR$CATCH.BS.201 - PREDICTIONS_NOSUR$SURPLUS*0.5
        PREDICTIONS_NOSUR$CATCH.BS.140 <- PREDICTIONS_NOSUR$CATCH.BS.140 - PREDICTIONS_NOSUR$SURPLUS*0.5 
        
        
    }  
    if (SUR) {
        PREDICTIONS_wSUR <- data.frame(CATCH.BS.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR.A80 <- predict(fit_sur[[32]], FISH.DATA)
        Pred.SUR.AFA <- predict(fit_sur[[33]], FISH.DATA)
        
        # Octopus
        PREDICTIONS_wSUR$CATCH.BS.60 <- pmin(predict(fit_sur[[1]], FISH.DATA), FISH.DATA$ABC.BSAI.60)
        PREDICTIONS_wSUR$CATCH.AI.60 <- pmin(predict(fit_sur[[2]], FISH.DATA), FISH.DATA$ABC.BSAI.60)
        # Sharks
        PREDICTIONS_wSUR$CATCH.BS.65 <- pmin(predict(fit_sur[[3]], FISH.DATA), FISH.DATA$ABC.BSAI.65)
        PREDICTIONS_wSUR$CATCH.AI.65 <- pmin(predict(fit_sur[[4]], FISH.DATA), FISH.DATA$ABC.BSAI.65)
        # Skates
        PREDICTIONS_wSUR$CATCH.BS.90 <- pmin(predict(fit_sur[[5]], FISH.DATA), FISH.DATA$ABC.BSAI.90)
        PREDICTIONS_wSUR$CATCH.AI.90 <- pmin(predict(fit_sur[[6]], FISH.DATA), FISH.DATA$ABC.BSAI.90)
        # Sculpin
        PREDICTIONS_wSUR$CATCH.BS.400 <- pmin(predict(fit_sur[[7]], FISH.DATA), FISH.DATA$ABC.BSAI.400)
        PREDICTIONS_wSUR$CATCH.AI.400 <- pmin(predict(fit_sur[[8]], FISH.DATA), FISH.DATA$ABC.BSAI.400)
        # Squid
        PREDICTIONS_wSUR$CATCH.BS.50 <- pmin(Pred.SUR.AFA$squid.pred, FISH.DATA$ABC.BSAI.50)
        PREDICTIONS_wSUR$CATCH.AI.50 <- pmin(predict(fit_sur[[38]], FISH.DATA), FISH.DATA$ABC.BSAI.50)
        
        
        # Shortraker
        PREDICTIONS_wSUR$CATCH.BS.326 <- pmin(predict(fit_sur[[24]], FISH.DATA), FISH.DATA$ABC.BSAI.326)
        PREDICTIONS_wSUR$CATCH.AI.326 <- pmin(predict(fit_sur[[25]], FISH.DATA), FISH.DATA$ABC.BSAI.326)
        # Rougheye
        PREDICTIONS_wSUR$CATCH.BS.307 <- pmin(predict(fit_sur[[26]], FISH.DATA), FISH.DATA$ABC.BSAI.307)
        PREDICTIONS_wSUR$CATCH.AI.307 <- pmin(predict(fit_sur[[27]], FISH.DATA), FISH.DATA$ABC.BSAI.307)
        # Other Rockfish
        PREDICTIONS_wSUR$CATCH.BS.310 <- pmin(predict(fit_sur[[15]], FISH.DATA), FISH.DATA$ABC.BS.310)
        PREDICTIONS_wSUR$CATCH.AI.310 <- pmin(predict(fit_sur[[16]], FISH.DATA), FISH.DATA$ABC.AI.310)
        # Northern
        PREDICTIONS_wSUR$CATCH.BS.303 <- pmin(predict(fit_sur[[13]], FISH.DATA), FISH.DATA$ABC.BSAI.303)
        PREDICTIONS_wSUR$CATCH.AI.303 <- pmin(predict(fit_sur[[14]], FISH.DATA), FISH.DATA$ABC.BSAI.303)
        # POP
        PREDICTIONS_wSUR$CATCH.BS.301 <- pmin(predict(fit_sur[[11]], FISH.DATA), FISH.DATA$ABC.BS.301)
        PREDICTIONS_wSUR$CATCH.AI.301 <- pmin(predict(fit_sur[[12]], FISH.DATA), FISH.DATA$ABC.AI.301)
        
        # Pollock
        PREDICTIONS_wSUR$CATCH.BS.201 <- pmin(Pred.SUR.AFA$pollock.pred, FISH.DATA$ABC.BS.201)
        PREDICTIONS_wSUR$CATCH.AI.201 <- pmin(predict(fit_sur[[37]], FISH.DATA), FISH.DATA$ABC.AI.201)
        # PCod
        PREDICTIONS_wSUR$CATCH.BS.202 <- pmin(Pred.SUR.A80$PCod.pred, FISH.DATA$TAC.BSAI.202)
        PREDICTIONS_wSUR$CATCH.AI.202 <- pmin(predict(fit_sur[[36]], FISH.DATA), FISH.DATA$TAC.BSAI.202)
        # Sablefish
        PREDICTIONS_wSUR$CATCH.BS.203 <- pmin(Pred.SUR.A80$sablefish.pred, FISH.DATA$TAC.BS.203)
        PREDICTIONS_wSUR$CATCH.AI.203 <- pmin(predict(fit_sur[[34]], FISH.DATA), FISH.DATA$TAC.AI.203)
        # Atka
        PREDICTIONS_wSUR$CATCH.BS.204 <- pmin(predict(fit_sur[[30]], FISH.DATA), FISH.DATA$ABC.BSAI.204)
        PREDICTIONS_wSUR$CATCH.AI.204 <- pmin(predict(fit_sur[[31]], FISH.DATA), FISH.DATA$ABC.BSAI.204)
        
        # Yellowfin
        PREDICTIONS_wSUR$CATCH.BS.140 <- pmin(Pred.SUR.A80$yellowfin.pred, FISH.DATA$TAC.BSAI.140) # No AI
        # Arrowtooth
        PREDICTIONS_wSUR$CATCH.BS.141 <- pmin(predict(fit_sur[[28]], FISH.DATA), FISH.DATA$ABC.BSAI.141)
        PREDICTIONS_wSUR$CATCH.AI.141 <- pmin(predict(fit_sur[[29]], FISH.DATA), FISH.DATA$ABC.BSAI.141)
        # Kamchatka
        PREDICTIONS_wSUR$CATCH.BS.147 <- pmin(predict(fit_sur[[9]], FISH.DATA), FISH.DATA$ABC.BSAI.147)
        PREDICTIONS_wSUR$CATCH.AI.147 <- pmin(predict(fit_sur[[10]], FISH.DATA), FISH.DATA$ABC.BSAI.147)
        
        # Other Flatfish
        PREDICTIONS_wSUR$CATCH.BS.100 <- pmin(predict(fit_sur[[20]], FISH.DATA), FISH.DATA$ABC.BSAI.100)
        PREDICTIONS_wSUR$CATCH.AI.100 <- pmin(predict(fit_sur[[21]], FISH.DATA), FISH.DATA$ABC.BSAI.100)
        # Greenland turbot
        PREDICTIONS_wSUR$CATCH.BS.102 <- pmin(predict(fit_sur[[17]], FISH.DATA), FISH.DATA$ABC.BS.102)
        PREDICTIONS_wSUR$CATCH.AI.102 <- pmin(predict(fit_sur[[18]], FISH.DATA), FISH.DATA$ABC.AI.102)
        # Flathead sole,
        PREDICTIONS_wSUR$CATCH.BS.103 <- pmin(predict(fit_sur[[22]], FISH.DATA), FISH.DATA$ABC.BSAI.103)   
        PREDICTIONS_wSUR$CATCH.AI.103 <- pmin(predict(fit_sur[[23]], FISH.DATA), FISH.DATA$ABC.BSAI.103)       
        # Rock Sole
        PREDICTIONS_wSUR$CATCH.BS.104 <- pmin(Pred.SUR.A80$rocksole.pred , FISH.DATA$ABC.BSAI.104)  
        PREDICTIONS_wSUR$CATCH.AI.104 <- pmin(predict(fit_sur[[35]], FISH.DATA), FISH.DATA$ABC.BSAI.104)     
        # Plaice
        PREDICTIONS_wSUR$CATCH.BS.106 <- pmin(predict(fit_sur[[19]], FISH.DATA), FISH.DATA$ABC.BSAI.106) # No AI
        
        
        PREDICTIONS_wSUR[PREDICTIONS_wSUR<0] <- 0 #nothing negative allowed..
        
        
        PREDICTIONS_wSUR$NETCATCH <- rowSums(PREDICTIONS_wSUR[,2:23], na.rm = TRUE)
        PREDICTIONS_wSUR$EXCEEDS.CAP <- PREDICTIONS_wSUR$NETCATCH > 2e6
        PREDICTIONS_wSUR$SURPLUS <- as.numeric(PREDICTIONS_wSUR$NETCATCH > 2e6)*(PREDICTIONS_wSUR$NETCATCH - 2e6)
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        PREDICTIONS_wSUR$CATCH.BS.201 <- PREDICTIONS_wSUR$CATCH.BS.201 - PREDICTIONS_wSUR$SURPLUS*0.5
        PREDICTIONS_wSUR$CATCH.BS.140 <- PREDICTIONS_wSUR$CATCH.BS.140 - PREDICTIONS_wSUR$SURPLUS*0.5
        
    }

   

    
    if (SUR) {
        return(PREDICTIONS_wSUR)
    }
    
    if (NOSUR) {
        return(PREDICTIONS_NOSUR)
    }
}