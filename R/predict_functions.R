predict.tac.function <- function(predictmethod ,model,fit,FISH.DATA){
    # Preamble ####  
    # 
    SUR<- F
    FLATSUR <- F
    SUR_FFDOM <- F
    SUR_WFDOM <- F
    FLAT_FFDOM <- F
    FLAT_WFDOM <- F
    
    if (model == "SUR") {
        SUR <- T
    }
    
    if (model == "SUR_FFDOM" ) {
        SUR_FFDOM <- T
    }
    
    if (model == "SUR_WFDOM") {
        SUR_WFDOM <- T
    } 
    
    if (model == "FLAT_FFDOM" ) {
        FLAT_FFDOM <- T
    }
    
    if (model == "FLAT_WFDOM") {
        FLAT_WFDOM <- T
    } 
    
    if (model == "FLATSUR") {
        FLATSUR <- T
    }
    
    # Define CAP function ####
    
    TAC.CAPFUNCTION <- function(DT) {
        DT <- exp(DT)
        # If prediction exceeds cap, trim down from the LARGEST stocks
        NETTAC <- rowSums(DT, na.rm = TRUE)
        SURPLUS <- as.numeric(NETTAC > 2e6)*(NETTAC - 2e6)
        DT <- DT[order(DT, decreasing = T)]
        # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        TEMP <- DT/NETTAC
        TEMP$TAC.BS.203 <- 0
        TEMP$TAC.AI.203 <- 0
        TEMP$TAC.BSAI.326 <- 0
        TEMP$TAC.BSAI.303 <- 0
        TOTALRAT <- sum(TEMP, na.rm = TRUE)
        TEMP <- TEMP/TOTALRAT
        output <- DT - SURPLUS*TEMP
        output <- log(output)         
        return(output)
    }
    
    # make predictions  ####
    if (SUR | SUR_FFDOM | SUR_WFDOM) {
        PREDICTIONS <- data.frame(TAC.BSAI.60=0)
        
        Pred.SUR <-  predict(fit[[16]], FISH.DATA)
        
        if (predictmethod  == 1 ) {
            # Octopus
            PREDICTIONS$TAC.BSAI.60 <- pmin(predict(fit[[1]], FISH.DATA), log(FISH.DATA$ABC.BSAI.60))
            # Sharks
            PREDICTIONS$TAC.BSAI.65 <- pmin(predict(fit[[2]], FISH.DATA),log(FISH.DATA$ABC.BSAI.65))
            # Skates
            PREDICTIONS$TAC.BSAI.90 <- pmin(predict(fit[[3]], FISH.DATA),log(FISH.DATA$ABC.BSAI.90))
            # Sculpin
            PREDICTIONS$TAC.BSAI.400 <- pmin(predict(fit[[4]], FISH.DATA),log(FISH.DATA$ABC.BSAI.400))
            # Squid
            PREDICTIONS$TAC.BSAI.50 <- pmin(Pred.SUR$squid.pred,log(FISH.DATA$ABC.BSAI.50))
            
            # Shortraker
            PREDICTIONS$TAC.BSAI.326 <-log(FISH.DATA$ABC.BSAI.326)
            # Rougheye
            PREDICTIONS$TAC.BSAI.307 <- pmin(predict(fit[[15]],FISH.DATA),log(FISH.DATA$ABC.BSAI.307))
            # Other Rockfish
            PREDICTIONS$TAC.BS.310 <- pmin(predict(fit[[8]], FISH.DATA),log(FISH.DATA$ABC.BS.310))
            PREDICTIONS$TAC.AI.310 <-log(FISH.DATA$ABC.AI.310)
            # Northern 
            PREDICTIONS$TAC.BSAI.303 <- pmin(predict(fit[[7]], FISH.DATA), log(FISH.DATA$ABC.BSAI.303))
            # POP
            PREDICTIONS$TAC.BS.301 <- pmin(predict(fit[[6]], FISH.DATA),log(FISH.DATA$ABC.BS.301))
            PREDICTIONS$TAC.AI.301 <-log(FISH.DATA$ABC.AI.301)
            
            # Pollock
            PREDICTIONS$TAC.BS.201 <- pmin(Pred.SUR$pollock.pred ,log(FISH.DATA$ABC.BS.201))
            PREDICTIONS$TAC.AI.201 <- pmin(predict(fit[[18]], FISH.DATA),log(FISH.DATA$ABC.AI.201))
            # PCod
            PREDICTIONS$TAC.BSAI.202 <- pmin(Pred.SUR$Pcod.pred ,log(FISH.DATA$ABC.BSAI.202))
            # Sablefish
            PREDICTIONS$TAC.BS.203 <- pmin(Pred.SUR$sablefish.pred ,log(FISH.DATA$ABC.BS.203))
            PREDICTIONS$TAC.AI.203 <- pmin(predict(fit[[17]], FISH.DATA),log(FISH.DATA$ABC.AI.203))
            # Atka
            PREDICTIONS$TAC.BSAI.204 <- pmin(Pred.SUR$atka.pred ,log(FISH.DATA$ABC.BSAI.204))
            
            # Yellowfin
            PREDICTIONS$TAC.BSAI.140 <- pmin(Pred.SUR$yellowfin.pred ,log(FISH.DATA$ABC.BSAI.140))
            # Arrowtooth
            PREDICTIONS$TAC.BSAI.141 <- pmin(predict(fit[[14]], FISH.DATA), log(FISH.DATA$ABC.BSAI.141))
            # Kamchatka
            PREDICTIONS$TAC.BSAI.147 <- pmin(predict(fit[[5]], FISH.DATA),log(FISH.DATA$ABC.BSAI.147))
            
            # Other FLatfish
            PREDICTIONS$TAC.BSAI.100 <- pmin(predict(fit[[12]], FISH.DATA), log(FISH.DATA$ABC.BSAI.100))
            # Greenland turbot
            PREDICTIONS$TAC.BS.102 <-  pmin(predict(fit[[9]], FISH.DATA), log(FISH.DATA$ABC.BS.102))
            PREDICTIONS$TAC.AI.102 <-  pmin(predict(fit[[10]], FISH.DATA), log(FISH.DATA$ABC.AI.102))
            # Flathead sole
            PREDICTIONS$TAC.BSAI.103 <- pmin(predict(fit[[13]], FISH.DATA), log(FISH.DATA$ABC.BSAI.103))
            # Rock Sole
            PREDICTIONS$TAC.BSAI.104 <- pmin(predict(fit[[19]], FISH.DATA) , log(FISH.DATA$ABC.BSAI.104))
            # Plaice
            PREDICTIONS$TAC.BSAI.106 <- pmin(predict(fit[[11]], FISH.DATA), log(FISH.DATA$ABC.BSAI.106))
        } else if (predictmethod  == 1.1) {
            # Octopus
            PREDICTIONS$TAC.BSAI.60 <- pmin(predict(fit[[1]], FISH.DATA),FISH.DATA$ABC.BSAI.60)
            # Sharks
            PREDICTIONS$TAC.BSAI.65 <- pmin(predict(fit[[2]], FISH.DATA),FISH.DATA$ABC.BSAI.65)
            # Skates
            PREDICTIONS$TAC.BSAI.90 <- pmin(predict(fit[[3]], FISH.DATA),FISH.DATA$ABC.BSAI.90)
            # Sculpin
            PREDICTIONS$TAC.BSAI.400 <- pmin(predict(fit[[4]], FISH.DATA),FISH.DATA$ABC.BSAI.400)
            # Squid
            PREDICTIONS$TAC.BSAI.50 <- pmin(Pred.SUR$squid.pred,FISH.DATA$ABC.BSAI.50)
            
            #Shortraker
            PREDICTIONS$TAC.BSAI.326 <- FISH.DATA$ABC.BSAI.326
            
            # Rougheye
            PREDICTIONS$TAC.BSAI.307 <- pmin(predict(fit[[15]],FISH.DATA),FISH.DATA$ABC.BSAI.307)
            # Other Rockfish
            PREDICTIONS$TAC.BS.310 <- pmin(predict(fit[[8]], FISH.DATA),FISH.DATA$ABC.BS.310)
            PREDICTIONS$TAC.AI.310 <- FISH.DATA$ABC.AI.310
            
            # Northern 
            PREDICTIONS$TAC.BSAI.303 <- pmin(predict(fit[[7]], FISH.DATA), FISH.DATA$ABC.BSAI.303)
            # POP
            PREDICTIONS$TAC.BS.301 <- pmin(predict(fit[[6]], FISH.DATA),FISH.DATA$ABC.BS.301)
            PREDICTIONS$TAC.AI.301 <- FISH.DATA$ABC.AI.301
            
            
            # Pollock
            PREDICTIONS$TAC.BS.201 <- pmin(Pred.SUR$pollock.pred ,FISH.DATA$ABC.BS.201)
            PREDICTIONS$TAC.AI.201 <- pmin(predict(fit[[18]], FISH.DATA),FISH.DATA$ABC.AI.201)
            # PCod
            PREDICTIONS$TAC.BSAI.202 <- pmin(Pred.SUR$Pcod.pred ,FISH.DATA$ABC.BSAI.202)
            # Sablefish
            PREDICTIONS$TAC.BS.203 <- pmin(Pred.SUR$sablefish.pred ,FISH.DATA$ABC.BS.203)
            PREDICTIONS$TAC.AI.203 <- pmin(predict(fit[[17]], FISH.DATA),FISH.DATA$ABC.AI.203)
            # Atka
            PREDICTIONS$TAC.BSAI.204 <- pmin(Pred.SUR$atka.pred ,FISH.DATA$ABC.BSAI.204)
            
            # Yellowfin
            PREDICTIONS$TAC.BSAI.140 <- pmin(Pred.SUR$yellowfin.pred ,FISH.DATA$ABC.BSAI.140)
            # Arrowtooth
            PREDICTIONS$TAC.BSAI.141 <- pmin(predict(fit[[14]], FISH.DATA), FISH.DATA$ABC.BSAI.141)
            # Kamchatka
            PREDICTIONS$TAC.BSAI.147 <- pmin(predict(fit[[5]], FISH.DATA),FISH.DATA$ABC.BSAI.147)
            
            # Other FLatfish
            PREDICTIONS$TAC.BSAI.100 <- pmin(predict(fit[[12]], FISH.DATA), FISH.DATA$ABC.BSAI.100)
            # Greenland turbot
            PREDICTIONS$TAC.BS.102 <-  pmin(predict(fit[[9]], FISH.DATA), FISH.DATA$ABC.BS.102)
            PREDICTIONS$TAC.AI.102 <-  pmin(predict(fit[[10]], FISH.DATA), FISH.DATA$ABC.AI.102)
            # Flathead sole
            PREDICTIONS$TAC.BSAI.103 <- pmin(predict(fit[[13]], FISH.DATA), FISH.DATA$ABC.BSAI.103)
            # Rock Sole
            
            PREDICTIONS$TAC.BSAI.104 <- pmin(predict(fit[[19]], FISH.DATA), FISH.DATA$ABC.BSAI.104)
            # Plaice
            PREDICTIONS$TAC.BSAI.106 <- pmin(predict(fit[[11]], FISH.DATA), FISH.DATA$ABC.BSAI.106)
        }
        
        
        PREDICTIONS[is.na(PREDICTIONS)] <- -Inf  # assume that any NaN comes from -Inf*0
        # This is crude, admittedly.. But this was the only time I was getting 
        # NaN, and going line by line to implement TAC = 0 if ABC = 0 is quite messy
        # 
        PREDICTIONS <- TAC.CAPFUNCTION(PREDICTIONS)
        if (predictmethod  == 1) {
            PREDICTIONS <- exp(PREDICTIONS)
        } 
    }
    
    if (FLATSUR | FLAT_FFDOM | FLAT_WFDOM) {
        PREDICTIONS <- data.frame(TAC.BSAI.60=0)
        
        Pred.SUR <-  predict(fit[[12]], FISH.DATA)
        Pred.SUR.flat <- predict(fit[[13]], FISH.DATA)
        
        if (predictmethod  == 1 ) { 
            # Octopus
            PREDICTIONS$TAC.BSAI.60 <- pmin(predict(fit[[1]], FISH.DATA),log(FISH.DATA$ABC.BSAI.60))
            # Sharks
            PREDICTIONS$TAC.BSAI.65 <- pmin(predict(fit[[2]], FISH.DATA),log(FISH.DATA$ABC.BSAI.65))
            # Skates
            PREDICTIONS$TAC.BSAI.90 <- pmin(predict(fit[[3]], FISH.DATA),log(FISH.DATA$ABC.BSAI.90))
            # Sculpin
            PREDICTIONS$TAC.BSAI.400 <- pmin(predict(fit[[4]], FISH.DATA),log(FISH.DATA$ABC.BSAI.400))
            # Squid
            PREDICTIONS$TAC.BSAI.50 <- pmin(Pred.SUR.flat$squid.pred ,log(FISH.DATA$ABC.BSAI.50))
            
            # Shortraker
            PREDICTIONS$TAC.BSAI.326 <- log(FISH.DATA$ABC.BSAI.326)
            # Rougheye
            PREDICTIONS$TAC.BSAI.307 <- pmin(predict(fit[[8]],FISH.DATA),log(FISH.DATA$ABC.BSAI.307))
            # Other Rockfish
            PREDICTIONS$TAC.BS.310 <- pmin(predict(fit[[9]],FISH.DATA),log(FISH.DATA$ABC.BS.310))
            PREDICTIONS$TAC.AI.310 <-log(FISH.DATA$ABC.AI.310)
            # Northern 
            PREDICTIONS$TAC.BSAI.303 <- pmin(predict(fit[[10]], FISH.DATA),log(FISH.DATA$ABC.BSAI.303))
            # POP
            PREDICTIONS$TAC.BS.301 <- pmin(predict(fit[[11]],FISH.DATA),log(FISH.DATA$ABC.BS.301))
            PREDICTIONS$TAC.AI.301 <- log(FISH.DATA$ABC.AI.301)
            
            # Pollock
            PREDICTIONS$TAC.BS.201 <- pmin(Pred.SUR.flat$pollock.pred ,log(FISH.DATA$ABC.BS.201))
            PREDICTIONS$TAC.AI.201 <- pmin(predict(fit[[16]],FISH.DATA),log(FISH.DATA$ABC.AI.201))
            # PCod
            PREDICTIONS$TAC.BSAI.202 <- pmin(Pred.SUR.flat$Pcod.pred ,log(FISH.DATA$ABC.BSAI.202))
            # Sablefish
            PREDICTIONS$TAC.BS.203 <- pmin(Pred.SUR.flat$sablefish.pred ,log(FISH.DATA$ABC.BS.203))
            PREDICTIONS$TAC.AI.203 <- pmin(predict(fit[[15]],FISH.DATA),log(FISH.DATA$ABC.AI.203))
            # Atka
            PREDICTIONS$TAC.BSAI.204 <- pmin(Pred.SUR.flat$atka.pred ,log(FISH.DATA$ABC.BSAI.204))
            
            
            # Yellowfin
            PREDICTIONS$TAC.BSAI.140 <- pmin(Pred.SUR.flat$yellowfin.pred ,log(FISH.DATA$ABC.BSAI.140))
            # Arrowtooth
            PREDICTIONS$TAC.BSAI.141 <- pmin(Pred.SUR.flat$arrowtooth.pred,log(FISH.DATA$ABC.BSAI.141))
            # Kamchatka
            PREDICTIONS$TAC.BSAI.147 <- pmin(predict(fit[[5]], FISH.DATA),log(FISH.DATA$ABC.BSAI.147))
            
            # Other FLatfish
            PREDICTIONS$TAC.BSAI.100 <- pmin(Pred.SUR.flat$Oflat.pred,log(FISH.DATA$ABC.BSAI.100))
            # Greenland turbot
            PREDICTIONS$TAC.BS.102 <- pmin(predict(fit[[17]],FISH.DATA),log(FISH.DATA$ABC.BS.102))
            PREDICTIONS$TAC.AI.102 <- pmin(predict(fit[[14]],FISH.DATA),log(FISH.DATA$ABC.AI.102))
            # Flathead sole
            PREDICTIONS$TAC.BSAI.103 <- pmin(predict(fit[[7]],FISH.DATA),log(FISH.DATA$ABC.BSAI.103))
            # Rock Sole
            PREDICTIONS$TAC.BSAI.104 <- pmin(predict(fit[[18]], FISH.DATA),log(FISH.DATA$ABC.BSAI.104))
            # Plaice
            PREDICTIONS$TAC.BSAI.106 <- pmin(predict(fit[[6]], FISH.DATA),log(FISH.DATA$ABC.BSAI.106))
        } else if (predictmethod  == 1.1) {
            # Octopus
            PREDICTIONS$TAC.BSAI.60 <- pmin(predict(fit[[1]], FISH.DATA), FISH.DATA$ABC.BSAI.60)
            # Sharks
            PREDICTIONS$TAC.BSAI.65 <- pmin(predict(fit[[2]], FISH.DATA), FISH.DATA$ABC.BSAI.65)
            # Skates
            PREDICTIONS$TAC.BSAI.90 <- pmin(predict(fit[[3]], FISH.DATA), FISH.DATA$ABC.BSAI.90)
            # Sculpin
            PREDICTIONS$TAC.BSAI.400 <- pmin(predict(fit[[4]], FISH.DATA), FISH.DATA$ABC.BSAI.400)
            # Squid
            PREDICTIONS$TAC.BSAI.50 <- pmin(Pred.SUR.flat$squid.pred , FISH.DATA$ABC.BSAI.50)
            
            # Shortraker
            
            PREDICTIONS$TAC.BSAI.326 <- FISH.DATA$ABC.BSAI.326
            
            # Rougheye
            PREDICTIONS$TAC.BSAI.307 <- pmin(predict(fit[[8]],FISH.DATA), FISH.DATA$ABC.BSAI.307)
            # Other Rockfish
            PREDICTIONS$TAC.BS.310 <- pmin(predict(fit[[9]],FISH.DATA), FISH.DATA$ABC.BS.310)
            
            PREDICTIONS$TAC.AI.310 <- FISH.DATA$ABC.AI.310
            
            # Northern 
            PREDICTIONS$TAC.BSAI.303 <- pmin(predict(fit[[10]], FISH.DATA),FISH.DATA$ABC.BSAI.303)
            # POP
            PREDICTIONS$TAC.BS.301 <- pmin(predict(fit[[11]],FISH.DATA), FISH.DATA$ABC.BS.301)
            
            PREDICTIONS$TAC.AI.301 <- FISH.DATA$ABC.AI.301
            
            
            # Pollock
            PREDICTIONS$TAC.BS.201 <- pmin(Pred.SUR$pollock.pred , FISH.DATA$ABC.BS.201)
            PREDICTIONS$TAC.AI.201 <- pmin(predict(fit[[16]],FISH.DATA), FISH.DATA$ABC.AI.201)
            # PCod
            PREDICTIONS$TAC.BSAI.202 <- pmin(Pred.SUR.flat$Pcod.pred , FISH.DATA$ABC.BSAI.202)
            # Sablefish
            PREDICTIONS$TAC.BS.203 <- pmin(Pred.SUR.flat$sablefish.pred , FISH.DATA$ABC.BS.203)
            PREDICTIONS$TAC.AI.203 <- pmin(predict(fit[[15]],FISH.DATA), FISH.DATA$ABC.AI.203)
            # Atka
            PREDICTIONS$TAC.BSAI.204 <- pmin(Pred.SUR.flat$atka.pred , FISH.DATA$ABC.BSAI.204)
            
            
            # Yellowfin
            PREDICTIONS$TAC.BSAI.140 <- pmin(Pred.SUR.flat$yellowfin.pred , FISH.DATA$ABC.BSAI.140)
            # Arrowtooth
            PREDICTIONS$TAC.BSAI.141 <- pmin(Pred.SUR.flat$arrowtooth.pred, FISH.DATA$ABC.BSAI.141)
            # Kamchatka
            PREDICTIONS$TAC.BSAI.147 <- pmin(predict(fit[[5]], FISH.DATA), FISH.DATA$ABC.BSAI.147)
            
            # Other FLatfish
            PREDICTIONS$TAC.BSAI.100 <- pmin(Pred.SUR.flat$Oflat.pred, FISH.DATA$ABC.BSAI.100)
            # Greenland turbot
            PREDICTIONS$TAC.BS.102 <- pmin(predict(fit[[17]],FISH.DATA), FISH.DATA$ABC.BS.102)
            PREDICTIONS$TAC.AI.102 <- pmin(predict(fit[[14]],FISH.DATA), FISH.DATA$ABC.AI.102)
            # Flathead sole
            PREDICTIONS$TAC.BSAI.103 <- pmin(predict(fit[[7]],FISH.DATA), FISH.DATA$ABC.BSAI.103)
            # Rock Sole
            
            PREDICTIONS$TAC.BSAI.104 <- pmin(predict(fit[[18]], FISH.DATA), FISH.DATA$ABC.BSAI.104)
            # Plaice
            PREDICTIONS$TAC.BSAI.106 <- pmin(predict(fit[[6]], FISH.DATA),FISH.DATA$ABC.BSAI.106)
        }
        
        PREDICTIONS[is.na(PREDICTIONS)] <- -Inf  # assume that any NaN comes from -Inf*0
        # This is crude, admittedly.. But this was the only time I was getting 
        # NaN, and going line by line to implement TAC = 0 if ABC = 0 is quite messy
        # 
        # Apply 2MT cap explicitly
        PREDICTIONS <- TAC.CAPFUNCTION(PREDICTIONS)
        PREDICTIONS <- exp(PREDICTIONS)
        
        
    }
    
    if (SUR_FFDOM | FLAT_FFDOM) {
        # goal, to decrease pollock + cod tac by up to a factor of 0.1, and give that tac to flatfish
        FFABCS <- FISH.DATA[ ,c("ABC.BSAI.140","ABC.BSAI.104","ABC.BSAI.147","ABC.BSAI.106","ABC.BS.102","ABC.AI.102","ABC.BSAI.100","ABC.BSAI.103","ABC.BSAI.141")]

        PREDICTIONS$FFABC <- rowSums(FFABCS, na.rm = T)
        FFTACS <- PREDICTIONS[ ,c("TAC.BSAI.140","TAC.BSAI.104","TAC.BSAI.147","TAC.BSAI.106","TAC.BS.102","TAC.AI.102","TAC.BSAI.100","TAC.BSAI.103","TAC.BSAI.141")]
        PREDICTIONS$FFTAC <- rowSums(FFTACS,na.rm=T)
        # goal will the minimum of 0.1*oldTAC; oldTAC - (1-0.1)*1.5e6; and flatfishABC-flatfishTAC
        # Aka the minimum of the fraction of the whitefish tac we want to take; the whitefish tac available to take; and the flatfish abc available to turn to tac
        PREDICTIONS$goal <- pmin(0.1*(PREDICTIONS$TAC.BS.201 + PREDICTIONS$TAC.BSAI.202), pmax((PREDICTIONS$TAC.BS.201 + PREDICTIONS$TAC.BSAI.202) - 1.50e6*(1-0.1),0), pmax(PREDICTIONS$FFABC - PREDICTIONS$FFTAC, 0))
        # decrease cod and pollock, if applicable (goal may be 0)
        PREDICTIONS$whitefishtac <- PREDICTIONS$TAC.BS.201 + PREDICTIONS$TAC.BSAI.202
        PREDICTIONS$TAC.BS.201 <- PREDICTIONS$TAC.BS.201 - (PREDICTIONS$TAC.BS.201/PREDICTIONS$whitefishtac)*PREDICTIONS$goal
        PREDICTIONS$TAC.BSAI.202 <- PREDICTIONS$TAC.BSAI.202 - (PREDICTIONS$TAC.BSAI.202/PREDICTIONS$whitefishtac)*PREDICTIONS$goal
        # increase flatfish, if applicable, up to ABC
        for (rownum in 1:nrow(PREDICTIONS)) {
            goalamt <- PREDICTIONS$goal[rownum]
            dt <- PREDICTIONS[rownum,c("TAC.BSAI.140","TAC.BSAI.104","TAC.BSAI.147","TAC.BSAI.106","TAC.BS.102","TAC.AI.102","TAC.BSAI.100","TAC.BSAI.103","TAC.BSAI.141")] # just to keep things clean..
            abcdt <- FFABCS[rownum, ]
            while (goalamt > 0) {
                # calculate the distance (from tac to abc) to tac ratio for each flatfish.
                disttoabc <- abcdt - dt 
                disttoabc_to_tac_ratio <- disttoabc/dt
                disttoabc_to_tac_ratio[disttoabc_to_tac_ratio < 1e-9] <- 0 # close enough to zero
                # the minimum, nonzero, ratio is the one that will determine how the goal amt is spread across the species, this round
                if (min(disttoabc_to_tac_ratio, na.rm = T) < -1e-6) stop('x is negative!! something went horribly wrong')
                x <- min(disttoabc_to_tac_ratio[disttoabc_to_tac_ratio > 0], na.rm = T)
                xvec <- disttoabc_to_tac_ratio
                xvec[xvec > 0] <- x
                xvec[is.na(xvec)] <- 0
                xvec[xvec < 0] <- 0
                if (sum(xvec*dt)==0 & goalamt > 1e-9) stop('infinite loop')
                # check that another loop is needed at all.  
                if (sum(xvec*dt) < goalamt) {
                    # decrease remaining goalamt to distribute
                    goalamt <- goalamt - sum(xvec*dt)
                    # increase all TAC proportionally, by the allowable distance given tac<abc constraint
                    dt <- dt + xvec*dt
                    # and go back to the beginning and do it again until goalamt = 0
                    if (goalamt < -1e-6) stop('goalamt is negative, but it shouldnt be.  something went horribly wrong') else if (goalamt < 1e-9) {goalamt <- 0} # if it's between 0 and -1e-9 just set it to 0; it's close enough.
                } else {
                    dtvec <- dt
                    dtvec[xvec == 0] <- 0 # so you don't go above abc!
                    # increase all TAC proportionally, by the remaining goalamt
                    dt <- dt + (dtvec/sum(dtvec))*goalamt
                    # calculate remaining goalamt
                    goalamt <- goalamt - sum((dtvec/sum(dtvec))*goalamt)
                    if (goalamt > 1e-9) stop('goalamt should be 0, but its not.  something went horribly wrong') else goalamt <- 0 #numerically it'll get super close but due to rounding you have to manually set to 0
                }
            }
            # put the results into the prediction dataframe and move on to the next row
            PREDICTIONS[rownum,c('TAC.BSAI.140','TAC.BSAI.104','TAC.BSAI.147','TAC.BSAI.106','TAC.BS.102','TAC.AI.102','TAC.BSAI.100','TAC.BSAI.103','TAC.BSAI.141')] <- dt[c('TAC.BSAI.140','TAC.BSAI.104','TAC.BSAI.147','TAC.BSAI.106','TAC.BS.102','TAC.AI.102','TAC.BSAI.100','TAC.BSAI.103','TAC.BSAI.141')]
            
        }
        
    }
    
    if (SUR_WFDOM | SUR_WFDOM ) {
        # goal, to increase pollock + cod tac by up to a factor of 10%, and take that tac from flatfish
        # allow up to 50% reduction in flatfish
        
        FFABCS <- FISH.DATA[ ,c("ABC.BSAI.140","ABC.BSAI.104","ABC.BSAI.147","ABC.BSAI.106","ABC.BS.102","ABC.AI.102","ABC.BSAI.100","ABC.BSAI.103","ABC.BSAI.141")]
        PREDICTIONS$FFABC <- rowSums(FFABCS, na.rm = T)
        FFTACS <- PREDICTIONS[ ,c("TAC.BSAI.140","TAC.BSAI.104","TAC.BSAI.147","TAC.BSAI.106","TAC.BS.102","TAC.AI.102","TAC.BSAI.100","TAC.BSAI.103","TAC.BSAI.141")]
        PREDICTIONS$FFTAC <- rowSums(FFTACS,na.rm=T)
        
        # goal will the minimum of 0.1*oldTAC; whitefishABC-whitefishTAC; and 
        # Aka the minimum of the fraction of the whitefish tac we want to add; the max flatfish reduction allowed; and the whitefish abc available to turn to tac
        PREDICTIONS$goal <- pmin(0.1*(PREDICTIONS$TAC.BS.201 + PREDICTIONS$TAC.BSAI.202), 0.5*PREDICTIONS$FFTAC, pmax(FISH.DATA$ABC.BS.201 + FISH.DATA$ABC.BSAI.202 - PREDICTIONS$TAC.BS.201 - PREDICTIONS$TAC.BSAI.202,0))
        
        # increase cod and pollock, if applicable (goal may be 0), up to ABC
        for (rownum in 1:nrow(PREDICTIONS)) { 
            goalamt <- PREDICTIONS$goal[rownum]
            dt <- PREDICTIONS[rownum,c("TAC.BS.201","TAC.BSAI.202") ]# just to keep things clean..
            abcdt <- FISH.DATA[rownum,c("ABC.BS.201","ABC.BSAI.202")]
            while (goalamt > 0) {
                # calculate the distance (from tac to abc) to tac ratio for each flatfish.
                disttoabc_to_tac_ratio <- (abcdt - dt)/dt
                disttoabc_to_tac_ratio[disttoabc_to_tac_ratio < 1e-9] <- 0 # close enough to zero
                # the minimum, nonzero, ratio is the one that will determine how the goal amt is spread across the species, this round
                if (min(disttoabc_to_tac_ratio, na.rm = T) < -1e-6) stop('x is negative!! something went horribly wrong')  # close enough to zero--a little coarser, was running in to trouble here 
                xvec <- disttoabc_to_tac_ratio
                xvec[xvec > 0] <- min(disttoabc_to_tac_ratio[disttoabc_to_tac_ratio > 0], na.rm = T)
                xvec[is.na(xvec)] <- 0
                xvec[xvec < 0] <- 0
                if (sum(xvec*dt)==0 & goalamt > 1e-9) stop('infinite loop')
                # check that another loop is needed at all.  
                if (sum(xvec*dt) < goalamt) {
                    # decrease remaining goalamt to distribute
                    goalamt <- goalamt - sum(xvec*dt)
                    # increase all TAC proportionally, by the allowable distance given tac<abc constraint
                    dt <- dt + xvec*dt
                    # and go back to the beginning and do it again until goalamt = 0
                    if (goalamt < -1e-6) stop('goalamt is negative, but it shouldnt be.  something went horribly wrong') else if (goalamt < 1e-8) {goalamt <- 0} # if it's between 1e-9 and -1e-9 just set it to 0; it's close enough.
                } else {
                    dtvec <- dt
                    dtvec[xvec == 0] <- 0 # so you don't go above abc!
                    # increase all TAC proportionally, by the remaining goalamt
                    dt <- dt + (dtvec/sum(dtvec))*goalamt
                    # calculate remaining goalamt
                    goalamt <- goalamt - sum((dtvec/sum(dtvec))*goalamt)
                    if (goalamt > 1e-9) stop('goalamt should be 0, but its not.  something went horribly wrong') else goalamt <- 0 #numerically it'll get super close but due to rounding you have to manually set to 0
                }
            }
            PREDICTIONS[rownum,c('TAC.BSAI.202','TAC.BS.201')] <- dt[c('TAC.BSAI.202','TAC.BS.201')]
        }
        
        
        
        # decrease flatfish, if applicable, down to 0.5*TAC.X
        dt <- PREDICTIONS[ ,c("TAC.BSAI.140","TAC.BSAI.104","TAC.BSAI.147","TAC.BSAI.106","TAC.BS.102","TAC.AI.102","TAC.BSAI.100","TAC.BSAI.103","TAC.BSAI.141")] # just to keep things clean..
        dt <- dt - PREDICTIONS$goal*dt/rowSums(dt, na.rm=T)
        
        # put the results into the prediction dataframe and move on to the next row
        # # this line doesnt work, fix it (right now it replaces everything, just wnat to replace flatfish/whitefish as appropriate!)
        PREDICTIONS[ ,c('TAC.BSAI.140','TAC.BSAI.104','TAC.BSAI.147','TAC.BSAI.106','TAC.BS.102','TAC.AI.102','TAC.BSAI.100','TAC.BSAI.103','TAC.BSAI.141')] <- dt[ ,c('TAC.BSAI.140','TAC.BSAI.104','TAC.BSAI.147','TAC.BSAI.106','TAC.BS.102','TAC.AI.102','TAC.BSAI.100','TAC.BSAI.103','TAC.BSAI.141')]
        
        
        
    }
    
    ## Return predictions ####
    PREDICTIONS$YEAR <- 1
    FISH.DATA$YEAR <- 1
    output <- merge(PREDICTIONS,FISH.DATA, by = "YEAR")
    
    return(output)
    
}

predict.catch.function <- function(model,fit,FISH.DATA) {
    # Preamble ####   
    SUR<- F
    NOSUR <- F
    FLATSUR <- F
    SUR_FFDOM<- F
    NOSUR_FFDOM <- F
    FLATSUR_FFDOM <- F
    
    if (model == "SUR") {
        SUR <- T
    }
    
    if (model == "NOSUR" ) {
        NOSUR <- T
    }
    
    if (model == "FLATSUR") {
        FLATSUR <- T
    }
    
    if (model == "FLATSUR_FFDOM") {
        FLATSUR_FFDOM <- T
    }
    
    if (model == "SUR_FFDOM") {
        SUR_FFDOM <- T
    }
    
    if (model == "NOSUR_FFDOM" ) {
        NOSUR_FFDOM <- T
    }
    

    
    # Define CAP function ####
    
    CATCH.CAPFUNCTION <- function(DT) {
        DT <- exp(DT)
        # DT$CATCH.AI.106 <- NA*DT$CATCH.AI.201
        # DT$CATCH.AI.140 <- DT$CATCH.AI.106
        # # If prediction exceeds cap, trim down from the LARGEST stocks
        # NETTAC <- rowSums(DT, na.rm = TRUE)
        # SURPLUS <- as.numeric(NETTAC > 2e6)*(NETTAC - 2e6)
        # DT <- DT[order(DT, decreasing = T)]
        # # If prediction exceeds cap, trim down pollock and yellowfin, 50/50
        # TEMP <- DT/NETTAC
        # TOTALRAT <- sum(TEMP[1:3])
        # TEMP[1:3] <- TEMP[1:3]/TOTALRAT 
        # TEMP[4:29] <- 0
        # output <- DT - SURPLUS*TEMP
        # 
        return(DT)
    }
    
        bsaicatchnogreaterthan <- function(bsvec,aivec,maxbsvec,maxaivec,maxbsaivec){
        for (i in 1:length(bsvec)) {
            bs <- exp(bsvec[i])
            ai <- exp(aivec[i])
            if (is.na(bs)) bs <- 0
            if (is.na(ai)) ai <- 0
            maxbsai <- maxbsaivec[i] 
            maxbs <- maxbsvec[i]
            maxai <- maxaivec[i]
            if (is.na(maxbsai)) {
                bsvec[i] <- -Inf
                aivec[i] <- -Inf
            } else if (bs + ai > maxbsai) { # if adjustment is necessary
                bsai <- bs + ai # current sum
                # replace the old bs/ai estimate with adjusted estimate
                if (bs - ai > 0 & bs > maxbs) { # if the bering sea is the one that may 'take' from the ai
                    bs <- min(maxbs,maxbsai)
                    ai <- maxbsai - bs
                } else if (ai - bs > 0 & ai > maxai) {
                    ai <- min(maxai*maxbsai/(maxai+maxbs),ai*maxbsai/(ai+bs),maxai)
                    bs <- maxbsai - ai
                } else {
                    ai <- maxbsai*ai/bsai
                    bs <- maxbsai*bs/bsai
                }
                
                bsvec[i] <- log(bs)
                aivec[i] <- log(ai)
            } 
            
        }
        
        return(list(bsvec,aivec))
    }
    
    checkeach_maxbsai <- function(PREDICTIONS) {
        # Octopus
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.60,PREDICTIONS$CATCH.AI.60, FISH.DATA$ABC.BS.60,FISH.DATA$ABC.AI.60, FISH.DATA$ABC.BSAI.60)
        PREDICTIONS$CATCH.BS.60 <- result[[1]]
        PREDICTIONS$CATCH.AI.60 <- result[[2]]
        # Sharks
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.65,PREDICTIONS$CATCH.AI.65,FISH.DATA$ABC.BS.65,FISH.DATA$ABC.AI.65,FISH.DATA$ABC.BSAI.65)
        PREDICTIONS$CATCH.BS.65 <- result[[1]]
        PREDICTIONS$CATCH.AI.65 <- result[[2]]
        # Skates
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.90,PREDICTIONS$CATCH.AI.90, FISH.DATA$ABC.BS.90, FISH.DATA$ABC.AI.90, FISH.DATA$ABC.BSAI.90)
        PREDICTIONS$CATCH.BS.90 <- result[[1]]
        PREDICTIONS$CATCH.AI.90 <- result[[2]]
        #Sculpin
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.400,PREDICTIONS$CATCH.AI.400, FISH.DATA$ABC.BS.400, FISH.DATA$ABC.AI.400, FISH.DATA$ABC.BSAI.400)
        PREDICTIONS$CATCH.BS.400 <- result[[1]]
        PREDICTIONS$CATCH.AI.400 <- result[[2]]
        #Squid
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.50,PREDICTIONS$CATCH.AI.50, FISH.DATA$ABC.BS.50, FISH.DATA$ABC.AI.50, FISH.DATA$ABC.BSAI.50)
        PREDICTIONS$CATCH.BS.50 <- result[[1]]
        PREDICTIONS$CATCH.AI.50 <- result[[2]]
        
        # Shortraker
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.326,PREDICTIONS$CATCH.AI.326,FISH.DATA$ABC.BS.326,FISH.DATA$ABC.AI.326,FISH.DATA$TAC.BSAI.326)
        PREDICTIONS$CATCH.BS.326 <- result[[1]]
        PREDICTIONS$CATCH.AI.326 <- result[[2]]
        # Rougheye
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.307,PREDICTIONS$CATCH.AI.307, FISH.DATA$ABC.BS.307,FISH.DATA$ABC.AI.307,FISH.DATA$TAC.BSAI.307)
        PREDICTIONS$CATCH.BS.307 <- result[[1]]
        PREDICTIONS$CATCH.AI.307 <- result[[2]]
        # Other rock fish should be handled without this
        # Northern
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.303,PREDICTIONS$CATCH.AI.303, FISH.DATA$ABC.BS.303,FISH.DATA$ABC.AI.303,FISH.DATA$TAC.BSAI.303)
        PREDICTIONS$CATCH.BS.303 <- result[[1]]
        PREDICTIONS$CATCH.AI.303 <- result[[2]]
        # POP should be handled without this
        
        # Pollock should be handled without this
        # PCod
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.202,PREDICTIONS$CATCH.AI.202,FISH.DATA$ABC.BS.202,FISH.DATA$ABC.AI.202,FISH.DATA$TAC.BSAI.202)
        PREDICTIONS$CATCH.BS.202 <- result[[1]]
        PREDICTIONS$CATCH.AI.202 <- result[[2]]
        # Sablefish should be handled without this
        # Atka 
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.204,PREDICTIONS$CATCH.AI.204,FISH.DATA$ABC.BS.204,FISH.DATA$ABC.AI.204,FISH.DATA$TAC.BSAI.204)
        PREDICTIONS$CATCH.BS.204 <- result[[1]]
        PREDICTIONS$CATCH.AI.204 <- result[[2]]
        
        # Yellowfin should be handled without this (no AI)
        # Arrowtooth
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.141,PREDICTIONS$CATCH.AI.141,FISH.DATA$ABC.BS.141,FISH.DATA$ABC.AI.141, pmin(3700 + FISH.DATA$TAC.BSAI.141, FISH.DATA$ABC.BSAI.141))
        PREDICTIONS$CATCH.BS.141 <- result[[1]]
        PREDICTIONS$CATCH.AI.141 <- result[[2]]
        # Kamchatka
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.147,PREDICTIONS$CATCH.AI.147,FISH.DATA$ABC.BS.147,FISH.DATA$ABC.AI.147, FISH.DATA$TAC.BSAI.147)
        PREDICTIONS$CATCH.BS.147 <- result[[1]]
        PREDICTIONS$CATCH.AI.147 <- result[[2]]
        
        # Other Flatfish
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.100,PREDICTIONS$CATCH.AI.100,FISH.DATA$ABC.BS.100,FISH.DATA$ABC.AI.100,pmin(pmax(FISH.DATA$TAC.BSAI.100,2e3), FISH.DATA$ABC.BSAI.100))
        PREDICTIONS$CATCH.BS.100 <- result[[1]]
        PREDICTIONS$CATCH.AI.100 <- result[[2]]
        # Greenland Turbot should be handled without this
        # Flathead Sole
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.103,PREDICTIONS$CATCH.AI.103,FISH.DATA$ABC.BS.103,FISH.DATA$ABC.AI.103,FISH.DATA$TAC.BSAI.103)
        PREDICTIONS$CATCH.BS.103 <- result[[1]]
        PREDICTIONS$CATCH.AI.103 <- result[[2]]
        # Rock Sole
        result <- bsaicatchnogreaterthan(PREDICTIONS$CATCH.BS.104,PREDICTIONS$CATCH.AI.104,FISH.DATA$ABC.BS.104,FISH.DATA$ABC.AI.104,pmin(pmax(35e3,FISH.DATA$TAC.BSAI.104), FISH.DATA$ABC.BSAI.104))
        PREDICTIONS$CATCH.BS.104 <- result[[1]]
        PREDICTIONS$CATCH.AI.104 <- result[[2]]
        # Plaice should be handled without this (no AI)
        # 
        return(PREDICTIONS)
    }
    
    # Make Predictions ####    
    if (NOSUR | NOSUR_FFDOM) {
        PREDICTIONS <- data.frame(CATCH.BS.60=0)   #PTAC stands for predicted TAC
        
           # Octopus
        PREDICTIONS$CATCH.BS.60 <- predict(fit[[1]], FISH.DATA)
        PREDICTIONS$CATCH.AI.60 <- predict(fit[[2]], FISH.DATA)
        # Sharks
        PREDICTIONS$CATCH.BS.65 <- predict(fit[[3]], FISH.DATA)
        PREDICTIONS$CATCH.AI.65 <- predict(fit[[4]], FISH.DATA)
        # Skates
        PREDICTIONS$CATCH.BS.90 <- predict(fit[[5]], FISH.DATA)
        PREDICTIONS$CATCH.AI.90 <- predict(fit[[6]], FISH.DATA)
        # Sculpin
        PREDICTIONS$CATCH.BS.400 <- predict(fit[[7]], FISH.DATA)
        PREDICTIONS$CATCH.AI.400 <- predict(fit[[8]], FISH.DATA)
        # Squid
        PREDICTIONS$CATCH.BS.50 <- predict(fit[[36]], FISH.DATA)
        PREDICTIONS$CATCH.AI.50 <- predict(fit[[37]], FISH.DATA)
        
        # Shortraker
        PREDICTIONS$CATCH.BS.326 <- predict(fit[[24]], FISH.DATA)
        PREDICTIONS$CATCH.AI.326 <- predict(fit[[25]], FISH.DATA)
        # Rougheye
        PREDICTIONS$CATCH.BS.307 <- predict(fit[[26]], FISH.DATA)
        PREDICTIONS$CATCH.AI.307 <- predict(fit[[27]], FISH.DATA)
        # Other Rockfish
        PREDICTIONS$CATCH.BS.310 <- pmin(predict(fit[[15]], FISH.DATA),log(FISH.DATA$ABC.BS.310))
        PREDICTIONS$CATCH.AI.310 <- pmin(predict(fit[[16]], FISH.DATA),log(FISH.DATA$ABC.AI.310))
        # Northern
        PREDICTIONS$CATCH.BS.303 <- predict(fit[[13]], FISH.DATA)
        PREDICTIONS$CATCH.AI.303 <- predict(fit[[14]], FISH.DATA)
        # POP
        PREDICTIONS$CATCH.BS.301 <- pmin(predict(fit[[11]], FISH.DATA),log(FISH.DATA$ABC.BS.301))
        PREDICTIONS$CATCH.AI.301 <- pmin(predict(fit[[12]], FISH.DATA),log(FISH.DATA$ABC.AI.301))
        
        # Pollock
        PREDICTIONS$CATCH.BS.201 <- pmin(predict(fit[[34]], FISH.DATA),pmin(log(FISH.DATA$ABC.BS.201),log(FISH.DATA$TAC.BS.201) + log(FISH.DATA$TAC.BS.201)))
        PREDICTIONS$CATCH.AI.201 <- pmin(predict(fit[[35]], FISH.DATA),log(FISH.DATA$ABC.AI.201))
        # PCod
        PREDICTIONS$CATCH.BS.202 <- predict(fit[[32]], FISH.DATA)
        PREDICTIONS$CATCH.AI.202 <- predict(fit[[33]], FISH.DATA)
        # Sablefish
        PREDICTIONS$CATCH.BS.203 <- pmin(predict(fit[[39]], FISH.DATA),log(FISH.DATA$TAC.BS.203))
        PREDICTIONS$CATCH.AI.203 <- pmin(predict(fit[[40]], FISH.DATA),log(FISH.DATA$TAC.AI.203))
        # Atka
        PREDICTIONS$CATCH.BS.204 <- predict(fit[[30]], FISH.DATA)
        PREDICTIONS$CATCH.AI.204 <- predict(fit[[31]], FISH.DATA)
        
        # Yellowfin
        PREDICTIONS$CATCH.BS.140 <- pmin(predict(fit[[38]], FISH.DATA),pmin(log(FISH.DATA$TAC.BSAI.140 + 3700), log(FISH.DATA$ABC.BSAI.140))) # No AI
        # Arrowtooth
        PREDICTIONS$CATCH.BS.141 <- predict(fit[[28]], FISH.DATA)
        PREDICTIONS$CATCH.AI.141 <- predict(fit[[29]], FISH.DATA)
        # Kamchatka
        PREDICTIONS$CATCH.BS.147 <- predict(fit[[9]], FISH.DATA)
        PREDICTIONS$CATCH.AI.147 <- predict(fit[[10]], FISH.DATA)
        
        # Other Flatfish
        PREDICTIONS$CATCH.BS.100 <- predict(fit[[20]], FISH.DATA)
        PREDICTIONS$CATCH.AI.100 <- predict(fit[[21]], FISH.DATA)
        # Greenland turbot
        PREDICTIONS$CATCH.BS.102 <- pmin(predict(fit[[17]], FISH.DATA),log(FISH.DATA$TAC.BS.102))
        PREDICTIONS$CATCH.AI.102 <- pmin(predict(fit[[18]], FISH.DATA),log(FISH.DATA$TAC.AI.102))
        # Flathead sole,
        PREDICTIONS$CATCH.BS.103 <- predict(fit[[22]], FISH.DATA)
        PREDICTIONS$CATCH.AI.103 <- predict(fit[[23]], FISH.DATA)   
        # Rock Sole
        PREDICTIONS$CATCH.BS.104 <- predict(fit[[41]], FISH.DATA)
        PREDICTIONS$CATCH.AI.104 <- predict(fit[[42]], FISH.DATA) 
        # Plaice
        PREDICTIONS$CATCH.BS.106 <- pmin(predict(fit[[19]], FISH.DATA),pmin(pmax(log(FISH.DATA$TAC.BSAI.106), 13.3e3), log(FISH.DATA$ABC.BSAI.106))) # No AI
        
    }  
    if (SUR | SUR_FFDOM) {
        PREDICTIONS <- data.frame(CATCH.BS.60=0)  #PTAC stands for predicted TAC
        
          
        Pred.SUR.A80 <- predict(fit[[32]], FISH.DATA)
        Pred.SUR.AFA <- predict(fit[[33]], FISH.DATA)
        
        # Octopus
        PREDICTIONS$CATCH.BS.60 <- predict(fit[[1]], FISH.DATA)
        PREDICTIONS$CATCH.AI.60 <- predict(fit[[2]], FISH.DATA)
        # Sharks
        PREDICTIONS$CATCH.BS.65 <- predict(fit[[3]], FISH.DATA)
        PREDICTIONS$CATCH.AI.65 <- predict(fit[[4]], FISH.DATA)
        # Skates
        PREDICTIONS$CATCH.BS.90 <- predict(fit[[5]], FISH.DATA)
        PREDICTIONS$CATCH.AI.90 <- predict(fit[[6]], FISH.DATA)
        # Sculpin
        PREDICTIONS$CATCH.BS.400 <- predict(fit[[7]], FISH.DATA)
        PREDICTIONS$CATCH.AI.400 <- predict(fit[[8]], FISH.DATA)
        # Squid
        PREDICTIONS$CATCH.BS.50 <- Pred.SUR.AFA$squid.pred
        PREDICTIONS$CATCH.AI.50 <- predict(fit[[38]], FISH.DATA)
        
        
        # Shortraker
        PREDICTIONS$CATCH.BS.326 <- predict(fit[[24]], FISH.DATA)
        PREDICTIONS$CATCH.AI.326 <- predict(fit[[25]], FISH.DATA)
        # Rougheye
        PREDICTIONS$CATCH.BS.307 <- predict(fit[[26]], FISH.DATA)
        PREDICTIONS$CATCH.AI.307 <- predict(fit[[27]], FISH.DATA)
        # Other Rockfish
        PREDICTIONS$CATCH.BS.310 <- predict(fit[[15]], FISH.DATA)
        PREDICTIONS$CATCH.AI.310 <- predict(fit[[16]], FISH.DATA)
        # Northern
        PREDICTIONS$CATCH.BS.303 <- predict(fit[[13]], FISH.DATA)
        PREDICTIONS$CATCH.AI.303 <- predict(fit[[14]], FISH.DATA)
        # POP
        PREDICTIONS$CATCH.BS.301 <- pmin(predict(fit[[11]], FISH.DATA),log(FISH.DATA$ABC.BS.301))
        PREDICTIONS$CATCH.AI.301 <- pmin(predict(fit[[12]], FISH.DATA),log(FISH.DATA$ABC.AI.301))
        
        # Pollock
        PREDICTIONS$CATCH.BS.201 <- pmin(Pred.SUR.AFA$pollock.pred,pmin(log(FISH.DATA$ABC.BS.201),log(FISH.DATA$TAC.BS.201) + log(FISH.DATA$TAC.BS.201)))
        PREDICTIONS$CATCH.AI.201 <- pmin(predict(fit[[37]], FISH.DATA),log(FISH.DATA$TAC.AI.201))
        # PCod
        PREDICTIONS$CATCH.BS.202 <- Pred.SUR.A80$PCod.pred
        PREDICTIONS$CATCH.AI.202 <- predict(fit[[36]], FISH.DATA)
        # Sablefish
        PREDICTIONS$CATCH.BS.203 <- pmin(Pred.SUR.A80$sablefish.pred,log(FISH.DATA$TAC.BS.203))
        PREDICTIONS$CATCH.AI.203 <- pmin(predict(fit[[34]], FISH.DATA),log(FISH.DATA$TAC.AI.203))
        # Atka
        PREDICTIONS$CATCH.BS.204 <- predict(fit[[30]], FISH.DATA)
        PREDICTIONS$CATCH.AI.204 <- predict(fit[[31]], FISH.DATA)
        
        # Yellowfin
        PREDICTIONS$CATCH.BS.140 <- pmin(Pred.SUR.A80$yellowfin.pred,pmin(log(FISH.DATA$TAC.BSAI.140 + 3700), log(FISH.DATA$ABC.BSAI.140))) # No AI
        # Arrowtooth
        PREDICTIONS$CATCH.BS.141 <- predict(fit[[28]], FISH.DATA)
        PREDICTIONS$CATCH.AI.141 <- predict(fit[[29]], FISH.DATA)
        # Kamchatka
        PREDICTIONS$CATCH.BS.147 <- predict(fit[[9]], FISH.DATA)
        PREDICTIONS$CATCH.AI.147 <- predict(fit[[10]], FISH.DATA)
        
        # Other Flatfish
        PREDICTIONS$CATCH.BS.100 <- predict(fit[[20]], FISH.DATA)
        PREDICTIONS$CATCH.AI.100 <- predict(fit[[21]], FISH.DATA)
        # Greenland turbot
        PREDICTIONS$CATCH.BS.102 <- pmin(predict(fit[[17]], FISH.DATA),log(FISH.DATA$TAC.BS.102))
        PREDICTIONS$CATCH.AI.102 <- pmin(predict(fit[[18]], FISH.DATA),log(FISH.DATA$TAC.AI.102))
        # Flathead sole,
        PREDICTIONS$CATCH.BS.103 <- predict(fit[[22]], FISH.DATA)
        PREDICTIONS$CATCH.AI.103 <- predict(fit[[23]], FISH.DATA)
        # Rock Sole
        PREDICTIONS$CATCH.BS.104 <- predict(fit[[39]], FISH.DATA) 
        PREDICTIONS$CATCH.AI.104 <- predict(fit[[35]], FISH.DATA)
        # Plaice
        PREDICTIONS$CATCH.BS.106 <- pmin(predict(fit[[19]], FISH.DATA),pmin(pmax(log(FISH.DATA$TAC.BSAI.106), 13.3e3), log(FISH.DATA$ABC.BSAI.106))) # No AI
        
        
    }
    if (FLATSUR |FLATSUR_FFDOM) {
        PREDICTIONS <- data.frame(CATCH.BS.60=0)  #PTAC stands for predicted TAC
        
        Pred.SUR.A80 <- predict(fit[[30]], FISH.DATA)
        Pred.SUR.AFA <- predict(fit[[31]], FISH.DATA)
        Pred.SUR.flat <- predict(fit[[32]],FISH.DATA)
        
        # Octopus
        PREDICTIONS$CATCH.BS.60 <- predict(fit[[1]], FISH.DATA)
        PREDICTIONS$CATCH.AI.60 <- predict(fit[[2]], FISH.DATA)
        # Sharks
        PREDICTIONS$CATCH.BS.65 <- predict(fit[[3]], FISH.DATA)
        PREDICTIONS$CATCH.AI.65 <- predict(fit[[4]], FISH.DATA)
        # Skates
        PREDICTIONS$CATCH.BS.90 <- predict(fit[[5]], FISH.DATA)
        PREDICTIONS$CATCH.AI.90 <- predict(fit[[6]], FISH.DATA)
        # Sculpin
        PREDICTIONS$CATCH.BS.400 <- predict(fit[[7]], FISH.DATA)
        PREDICTIONS$CATCH.AI.400 <- predict(fit[[8]], FISH.DATA)
        # Squid
        PREDICTIONS$CATCH.BS.50 <- Pred.SUR.AFA$squid.pred
        PREDICTIONS$CATCH.AI.50 <- predict(fit[[37]], FISH.DATA)
        
        
        # Shortraker
        PREDICTIONS$CATCH.BS.326 <- predict(fit[[23]], FISH.DATA)
        PREDICTIONS$CATCH.AI.326 <- predict(fit[[24]], FISH.DATA)
        # Rougheye
        PREDICTIONS$CATCH.BS.307 <- predict(fit[[25]], FISH.DATA)
        PREDICTIONS$CATCH.AI.307 <- predict(fit[[26]], FISH.DATA)
        # Other Rockfish
        PREDICTIONS$CATCH.BS.310 <- pmin(predict(fit[[15]], FISH.DATA),log(FISH.DATA$ABC.BS.310))
        PREDICTIONS$CATCH.AI.310 <- pmin(predict(fit[[16]], FISH.DATA),log(FISH.DATA$ABC.AI.310))
        # Northern
        PREDICTIONS$CATCH.BS.303 <- predict(fit[[13]], FISH.DATA)
        PREDICTIONS$CATCH.AI.303 <- predict(fit[[14]], FISH.DATA)
        # POP
        PREDICTIONS$CATCH.BS.301 <- pmin(predict(fit[[11]], FISH.DATA),log(FISH.DATA$ABC.BS.301))
        PREDICTIONS$CATCH.AI.301 <- pmin(predict(fit[[12]], FISH.DATA),log(FISH.DATA$ABC.AI.301))
        
        # Pollock
        PREDICTIONS$CATCH.BS.201 <- pmin(Pred.SUR.AFA$pollock.pred,pmin(log(FISH.DATA$ABC.BS.201),log(FISH.DATA$TAC.BS.201) + log(FISH.DATA$TAC.BS.201)))
        PREDICTIONS$CATCH.AI.201 <- pmin(predict(fit[[36]], FISH.DATA),log(FISH.DATA$ABC.AI.201))
        # PCod
        PREDICTIONS$CATCH.BS.202 <- Pred.SUR.flat$PCod.pred
        PREDICTIONS$CATCH.AI.202 <- predict(fit[[35]], FISH.DATA)
        # Sablefish
        PREDICTIONS$CATCH.BS.203 <- pmin(Pred.SUR.flat$sablefish.pred,log(FISH.DATA$TAC.BS.203))
        PREDICTIONS$CATCH.AI.203 <- pmin(predict(fit[[33]], FISH.DATA),log(FISH.DATA$TAC.AI.203))
        # Atka
        PREDICTIONS$CATCH.BS.204 <- predict(fit[[28]], FISH.DATA)
        PREDICTIONS$CATCH.AI.204 <- predict(fit[[29]], FISH.DATA)
        
        # Yellowfin
        PREDICTIONS$CATCH.BS.140 <- pmin(Pred.SUR.flat$yellowfin.pred,pmin(log(FISH.DATA$TAC.BSAI.140 + 3700), log(FISH.DATA$ABC.BSAI.140))) # No AI
        # Arrowtooth
        PREDICTIONS$CATCH.BS.141 <- Pred.SUR.flat$arrowtooth.pred
        PREDICTIONS$CATCH.AI.141 <- predict(fit[[27]], FISH.DATA)
        # Kamchatka
        PREDICTIONS$CATCH.BS.147 <- predict(fit[[9]], FISH.DATA)
        PREDICTIONS$CATCH.AI.147 <- predict(fit[[10]], FISH.DATA)
        
        # Other Flatfish
        PREDICTIONS$CATCH.BS.100 <- Pred.SUR.flat$Oflat.pred
        PREDICTIONS$CATCH.AI.100 <- predict(fit[[20]], FISH.DATA)
        # Greenland turbot
        PREDICTIONS$CATCH.BS.102 <- pmin(predict(fit[[17]], FISH.DATA),log(FISH.DATA$TAC.BS.102))
        PREDICTIONS$CATCH.AI.102 <- pmin(predict(fit[[18]], FISH.DATA),log(FISH.DATA$TAC.AI.102))
        # Flathead sole,
        PREDICTIONS$CATCH.BS.103 <- predict(fit[[21]], FISH.DATA)  
        PREDICTIONS$CATCH.AI.103 <- predict(fit[[22]], FISH.DATA)     
        # Rock Sole
        PREDICTIONS$CATCH.BS.104 <- predict(fit[[38]], FISH.DATA)
        #PREDICTIONS$CATCH.BS.104 <- Pred.SUR.flat$rocksole.pred,log(FISH.DATA$ABC.BSAI.104))
        PREDICTIONS$CATCH.AI.104 <- predict(fit[[34]], FISH.DATA)  
        # Plaice
        PREDICTIONS$CATCH.BS.106 <- pmin(predict(fit[[19]], FISH.DATA),pmin(pmax(log(FISH.DATA$TAC.BSAI.106), 13.3e3), log(FISH.DATA$ABC.BSAI.106))) # No AI
    }
    if (NOSUR_FFDOM | SUR_FFDOM | FLATSUR_FFDOM) {
        # Yellowfin
        PREDICTIONS$CATCH.BS.140 <- pmin(log(1.3) + PREDICTIONS$CATCH.BS.140,pmin(log(FISH.DATA$TAC.BSAI.140 + 3700), log(FISH.DATA$ABC.BSAI.140))) # No AI
        # Arrowtooth
        PREDICTIONS$CATCH.BS.141 <- log(1.3) + PREDICTIONS$CATCH.BS.141
        PREDICTIONS$CATCH.AI.141 <- log(1.3) + PREDICTIONS$CATCH.AI.141
        # Kamchatka
        PREDICTIONS$CATCH.BS.147 <- log(1.3) + PREDICTIONS$CATCH.BS.147
        PREDICTIONS$CATCH.AI.147 <- log(1.3) + PREDICTIONS$CATCH.AI.147
        
        # Other Flatfish
        PREDICTIONS$CATCH.BS.100 <- log(1.3) + PREDICTIONS$CATCH.BS.100
        PREDICTIONS$CATCH.AI.100 <- log(1.3) + PREDICTIONS$CATCH.AI.100
        # Greenland turbot
        PREDICTIONS$CATCH.BS.102 <- log(1.3) + PREDICTIONS$CATCH.BS.102
        PREDICTIONS$CATCH.AI.102 <- log(1.3) + PREDICTIONS$CATCH.AI.102
        # Flathead sole,
        PREDICTIONS$CATCH.BS.103 <- log(1.3) + PREDICTIONS$CATCH.BS.103
        PREDICTIONS$CATCH.AI.103 <- log(1.3) + PREDICTIONS$CATCH.AI.103
        # Rock Sole
        PREDICTIONS$CATCH.BS.104 <- log(1.3) + PREDICTIONS$CATCH.BS.104
        PREDICTIONS$CATCH.AI.104 <- log(1.3) + PREDICTIONS$CATCH.AI.104
        # Plaice
        PREDICTIONS$CATCH.BS.106 <- pmin(log(1.3) + PREDICTIONS$CATCH.BS.106,pmin(pmax(log(FISH.DATA$TAC.BSAI.106), 13.3e3), log(FISH.DATA$ABC.BSAI.106))) # No AI

    }
    
    
    PREDICTIONS <- checkeach_maxbsai(PREDICTIONS)  # make sure no bs+ai exceeds its abc
    PREDICTIONS <- exp(PREDICTIONS)
    
    return(PREDICTIONS)
    
    
}
