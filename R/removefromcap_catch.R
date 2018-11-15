removefromcap_catch <- function(ABC.DATA,scenario,spptomult,improvedcatchscale) {
    
    FISH.DATA <- ABC.DATA
    FISH.DATA$ABCboth <- FISH.DATA$ABC.BSAI.202 + FISH.DATA$ABC.BS.201
    FISH.DATA$ABCboth.UB.150 <- as.numeric(FISH.DATA$ABC.BS.201 + FISH.DATA$ABC.BSAI.202 >= 1.5e6)
    FISH.DATA$pollock.bs.UB <-  as.numeric(FISH.DATA$ABC.BS.201 > 1.2e6)
    
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
    FISH.DATA$SSL <-  1 # stellar sea lion closure
    FISH.DATA$WAISSL <- 0
    FISH.DATA$A80.ask.POP <- 1
    FISH.DATA$is93 <- 0
    FISH.DATA$A82 <- 1
    
    # I start by giving myself a list of all the species numbers
    allspp <- c("141",
                "204",
                "103",
                "102",
                "147",
                "303",
                "60",
                "100",
                "310",
                "202",
                "106",
                "301",
                "201",
                "104",
                "307",
                "203",
                "400",
                "65",
                "326",
                "90",
                "50",
                "140")
    # And their corresponding names, alphabetically.  
    # Honestly this could be saved in sysdata and loaded but it's not bad to have 
    # handy in case if you forget what number = what species
    sppnames <- c("Arrowtooth", 
                  "Atka", 
                  "Flathead", 
                  "Greenland", 
                  "Kamchatka", 
                  "Northern", 
                  "Octopus", 
                  "OtherFlat", 
                  "OtherRock", 
                  "PCod", 
                  "Plaice", 
                  "POP", 
                  "Pollock", 
                  "Rock", 
                  "Rougheye", 
                  "Sablefish", 
                  "Sculpin", 
                  "Shark",
                  "Shortraker", 
                  "Skate", 
                  "Squid", 
                  "Yellowfin")
    
    # calculate TAC.
    #   rule 1: if pollock has been removed from 2mmt cap, assume everything else is TAC = ABC
    if (is.element("pollock",spptomult)) {
        FISH.DATA$TAC.BSAI.201 <- 0
        TAC.BOTHBIND <- FISH.DATA  
        TAC.BOTHBIND.FLATSUR <- FISH.DATA
        
        # check sum < 2mmt
        check2mmt <- function(DT_orig) {
            DT <- DT_orig %>% select(starts_with("TAC"))
            NETTAC <- rowSums(DT, na.rm = TRUE)
            if (NETTAC > 2e6) {
                # first bring down the least valuable/zero value bycatch species
                # These are first because these species really are almost nuisance
                # catches.
                TAC.BOTHBIND <- predict.tac.function(predictmethod = 1, model="SUR",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
                DT_orig$TAC.BSAI.60 <- TAC.BOTHBIND$TAC.BSAI.60 # octopus
                DT_orig$TAC.BSAI.65 <- TAC.BOTHBIND$TAC.BSAI.65 # sharks
                DT_orig$TAC.BSAI.90 <- TAC.BOTHBIND$TAC.BSAI.90 #skates
                DT_orig$TAC.BSAI.400 <- TAC.BOTHBIND$TAC.BSAI.400 #skates
            }
            #check again
            DT <- DT_orig %>% select(starts_with("TAC"))
            NETTAC <- rowSums(DT, na.rm = TRUE)
            if (NETTAC > 2e6) {
                # if still over 2mmt, next bring down species for various reasons (see below)
                DT_orig$TAC.BSAI.50 <- TAC.BOTHBIND$TAC.BSAI.50 # squid -- pollock bycatch. Valuable (because pollock) but still pure bycatch
                DT_orig$TAC.BSAI.141 <- TAC.BOTHBIND$TAC.BSAI.141 # arrowtooth -- halibut will continue to be limiting
                DT_orig$TAC.BSAI.104 <- TAC.BOTHBIND$TAC.BSAI.104 # rock sole -- worry about market limits
            }
            #check again
            DT <- DT_orig %>% select(starts_with("TAC"))
            NETTAC <- rowSums(DT, na.rm = TRUE)
            if (NETTAC > 2e6) {
                # if still over 2mmt, next bring down basically everything that doesnt 
                # tend to go up with pollock or isn't directly listed as a targeted species 
                # 
                # 
                # non-AFA trawl cp targets: yfin, rock sole, flathead sole,atka, pop
                # while cod isn't technically a target, its effectively one in terms of its value (as a bycatch)
                # species that go up w. pollock going down: arrowtooth, flathead sole, rock sole, yellowfin
                
                DT_orig$TAC.BSAI.326 <- TAC.BOTHBIND$TAC.BSAI.326 # shortraker
                DT_orig$TAC.BSAI.307 <- TAC.BOTHBIND$TAC.BSAI.307 # rougheye
                DT_orig$TAC.BS.310 <- TAC.BOTHBIND$TAC.BS.310 # orock BS
                DT_orig$TAC.AI.310 <- TAC.BOTHBIND$TAC.AI.310 # orock AI
                DT_orig$TAC.BSAI.303 <- TAC.BOTHBIND$TAC.BSAI.303 # northern
                
                DT_orig$TAC.BSAI.147 <- TAC.BOTHBIND$TAC.BSAI.147 # kamchatka
                DT_orig$TAC.BSAI.100 <- TAC.BOTHBIND$TAC.BSAI.307 # oflat
                DT_orig$TAC.BS.102 <- TAC.BOTHBIND$TAC.BS.102 # greenland BS
                DT_orig$TAC.AI.102 <- TAC.BOTHBIND$TAC.AI.102 # greenland AI
                DT_orig$TAC.BSAI.106 <- TAC.BOTHBIND$TAC.AI.106 # plaice
                
            }
            #honestly at this point if we still have nettac > 2mmt something very weird is going on...
            DT <- DT_orig %>% select(starts_with("TAC"))
            NETTAC <- rowSums(DT, na.rm = TRUE)
            if (NETTAC > 2e6) {stop('NETTAC still over 2e6 even though pollock removed.  Line 82 removefromcap_catch.R')}
        }
    } else {
        TAC.BOTHBIND <- predict.tac.function(predictmethod = 1, model="SUR",fit=tac_BOTHBIND_loglin_sur,FISH.DATA)
        TAC.BOTHBIND.FLATSUR <- predict.tac.function(predictmethod = 1, model="FLATSUR",fit=tac_BOTHBIND_FLATSUR_loglin_sur,FISH.DATA)
        
        for (i in 1:length(spptomult)) {
            eval(parse(text = paste("TAC.BOTHBIND$TAC.BSAI.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
            eval(parse(text = paste("TAC.BOTHBIND$TAC.BS.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
            eval(parse(text = paste("TAC.BOTHBIND$TAC.AI.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
        } 
        for (i in 1:length(spptomult)) {
            eval(parse(text = paste("TAC.BOTHBIND.FLATSUR$TAC.BSAI.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
            eval(parse(text = paste("TAC.BOTHBIND.FLATSUR$TAC.BS.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
            eval(parse(text = paste("TAC.BOTHBIND.FLATSUR$TAC.AI.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
        } 
        
        # Reallocate the remaining TAC.
        # 
        distributefun <- function(goal,abcdt,dt) {
                while (goal > 0) {
                    # calculate the distance (from tac to abc) to tac ratio for each flatfish.
                    disttoabc_to_tac_ratio <- (abcdt - dt)/dt
                    disttoabc_to_tac_ratio[disttoabc_to_tac_ratio < 1e-9] <- 0 # close enough to zero
                    # the minimum, nonzero, ratio is the one that will determine how the goal amt is spread across the species, this round
                    if (min(disttoabc_to_tac_ratio, na.rm = T) < -1e-6) stop('x is negative!! something went horribly wrong')  # close enough to zero--a little coarser, was running in to trouble here 
                    xvec <- disttoabc_to_tac_ratio
                    xvec[xvec > 0] <- min(disttoabc_to_tac_ratio[disttoabc_to_tac_ratio > 0], na.rm = T)
                    xvec[is.na(xvec)] <- 0
                    xvec[xvec < 0] <- 0
                    if (sum(xvec*dt)==0 & goal > 1e-9) stop('infinite loop')
                    # check that another loop is needed at all.  
                    if (sum(xvec*dt) < goal) {
                        # decrease remaining goalamt to distribute
                        goal <- goal - sum(xvec*dt)
                        # increase all TAC proportionally, by the allowable distance given tac<abc constraint
                        dt <- dt + xvec*dt
                        # and go back to the beginning and do it again until goalamt = 0
                        if (goal < -1e-6) stop('goalamt is negative, but it shouldnt be.  something went horribly wrong') else if (goal < 1e-8) {goal <- 0} # if it's between 1e-9 and -1e-9 just set it to 0; it's close enough.
                    } else {
                        dtvec <- dt
                        dtvec[xvec == 0] <- 0 # so you don't go above abc!
                        # increase all TAC proportionally, by the remaining goalamt
                        dt <- dt + (dtvec/sum(dtvec))*goal
                        # calculate remaining goalamt
                        goal <- goal - sum((dtvec/sum(dtvec))*goal)
                        if (goal > 1e-9) stop('goalamt should be 0, but its not.  something went horribly wrong') else goal <- 0 #numerically it'll get super close but due to rounding you have to manually set to 0
                    }
                }
                return(dt)
         }
         
        TAC.reallocate <- function(DT_orig,split_param) {
            DT <- DT_orig %>% select(starts_with("TAC"))
            # If prediction exceeds cap, trim down from the LARGEST stocks
            NETTAC <- rowSums(DT, na.rm = TRUE)
            EXTRATAC <- as.numeric(NETTAC < 2e6)*(2e6 - NETTAC)
            DT <- DT[order(DT, decreasing = T)]
            # If there is excess, give split_param*excess to pollock and cod, up to ABC
            # Give the rest to the rest
            FORWHITEFISH <- EXTRATAC*split_param
            
            # goal, to increase pollock + cod TAC by the minimum of:  forwhitefish  or ABC-TAC
            
            goal <- pmin(FORWHITEFISH,FISH.DATA$ABC.BS.201 - DT$TAC.BS.201 + FISH.DATA$ABC.BSAI.202 - DT$TAC.BSAI.202)
            goalamt <- goal
            
            
            dt <- DT[c("TAC.BS.201","TAC.BSAI.202") ]# just to keep things clean..
            abcdt <- FISH.DATA[c("ABC.BS.201","ABC.BSAI.202")]
            
            dt <- distributefun(goalamt,abcdt,dt)
            
            DT_orig$TAC.BS.201 <- dt$TAC.BS.201
            DT_orig$TAC.BSAI.202 <- dt$TAC.BSAI.202 
            
            
            # increase first the species that empirically see increases when pollock is low
            # this is yellowfin, arrowtooth, rock sole and flathead.  (see manuscript table)
            # expert opinion is others (e.g. northern rockfish) should also go up but
            # couldn't get a statistically significant result when this was tested, so leaving
            # that out.
            # 
            FORPOLRESFISH <- EXTRATAC - goalamt
            
            if (FORPOLRESFISH > 0) {
                dt <- DT[c("TAC.BSAI.140",
                           "TAC.BSAI.104",
                           "TAC.BSAI.103",
                           "TAC.BSAI.141")] # just to keep things clean..
                abcdt <- FISH.DATA[c("ABC.BSAI.140",
                                     "ABC.BSAI.104",
                                     "ABC.BSAI.103",
                                     "ABC.BSAI.141")] # just to keep things clean..
                
                goalamt <- pmin(FORPOLRESFISH, sum(abcdt) - sum(dt))
                
                
                dt <- distributefun(goalamt,abcdt,dt)
                
                # put the results into the prediction dataframe and move on to the next row
                DT_orig[c('TAC.BSAI.140',
                          'TAC.BSAI.104',
                          'TAC.BSAI.103',
                          'TAC.BSAI.141')] <- dt[c('TAC.BSAI.140',
                                                   'TAC.BSAI.104',
                                                   'TAC.BSAI.103',
                                                   'TAC.BSAI.141')]
                #
                # Finally if there is still any remaining 2mmt use it to increase everything else.
                FORRESTFISH <- FORPOLRESFISH - goalamt
                if (FORRESTFISH > 0) {
                    dt <- DT[-c('TAC.BSAI.60',
                                'TAC.BSAI.65',
                                'TAC.BSAI.90',
                                'TAC.BSAI.400',
                                'TAC.BSAI.326',
                                'TAC.BSAI.307',
                                'TAC.BSAI.310',
                                'TAC.BSAI.303',
                                'TAC.BSAI.301',
                                'TAC.BS.203',
                                'TAC.AI.203',
                                'TAC.BSAI.204',
                                'TAC.BSAI.147',
                                'TAC.BSAI.100',
                                'TAC.BS.102',
                                'TAC.AI.102',
                                'TAC.BSAI.106')] # just to keep things clean..
                    abcdt <- FISH.DATA[c('ABC.BSAI.60',
                                         'ABC.BSAI.65',
                                         'ABC.BSAI.90',
                                         'ABC.BSAI.400',
                                         'ABC.BSAI.326',
                                         'ABC.BSAI.307',
                                         'ABC.BSAI.310',
                                         'ABC.BSAI.303',
                                         'ABC.BSAI.301',
                                         'ABC.BS.203',
                                         'ABC.AI.203',
                                         'ABC.BSAI.204',
                                         'ABC.BSAI.147',
                                         'ABC.BSAI.100',
                                         'ABC.BS.102',
                                         'ABC.AI.102',
                                         'ABC.BSAI.106')] # just to keep things clean..
                    
                    goalamt <- pmin(FORRESTFISH, sum(abcdt) - sum(dt))
                    
                    
                    dt <- distributefun(goalamt,abcdt,dt)
                    
                    # put the results into the prediction dataframe and move on to the next row
                    DT_orig[c('TAC.BSAI.60',
                              'TAC.BSAI.65',
                              'TAC.BSAI.90',
                              'TAC.BSAI.400',
                              'TAC.BSAI.326',
                              'TAC.BSAI.307',
                              'TAC.BSAI.310',
                              'TAC.BSAI.303',
                              'TAC.BSAI.301',
                              'TAC.BS.203',
                              'TAC.AI.203',
                              'TAC.BSAI.204',
                              'TAC.BSAI.147',
                              'TAC.BSAI.100',
                              'TAC.BS.102',
                              'TAC.AI.102',
                              'TAC.BSAI.106')] <- dt[c('TAC.BSAI.60',
                                                       'TAC.BSAI.65',
                                                       'TAC.BSAI.90',
                                                       'TAC.BSAI.400',
                                                       'TAC.BSAI.326',
                                                       'TAC.BSAI.307',
                                                       'TAC.BSAI.310',
                                                       'TAC.BSAI.303',
                                                       'TAC.BSAI.301',
                                                       'TAC.BS.203',
                                                       'TAC.AI.203',
                                                       'TAC.BSAI.204',
                                                       'TAC.BSAI.147',
                                                       'TAC.BSAI.100',
                                                       'TAC.BS.102',
                                                       'TAC.AI.102',
                                                       'TAC.BSAI.106')]
                }
            }
            
            
            
            
            return(DT_orig)
        }
        
        
        TAC.BOTHBIND <- TAC.reallocate(TAC.BOTHBIND,0.7)
        
        TAC.BOTHBIND.FLATSUR <- TAC.reallocate(TAC.BOTHBIND.FLATSUR,0.7)
    }
    
    # catch depending on 
    CATCH.BOTHBIND.SURSUR <- predict.catch.function(model="SUR",fit=catch_BOTHBIND_loglin_sur,TAC.BOTHBIND )
    CATCH.BOTHBIND.SUROLS <- predict.catch.function(model="NOSUR",fit = catch_BOTHBIND_loglin_nosur,TAC.BOTHBIND )
    CATCH.BOTHBIND.FLATSUR.SURSUR <- predict.catch.function(model="FLATSUR",fit=catch_BOTHBIND_FLATSUR_loglin_sur,TAC.BOTHBIND.FLATSUR )
    
    
    # create ensemble
    # 
    CATCH.PRED <- (CATCH.BOTHBIND.SURSUR + CATCH.BOTHBIND.SUROLS + CATCH.BOTHBIND.FLATSUR.SURSUR)/3
    
    
    
    
    if (scenario == 5.4) {
        # Improved catch scenario
        # Note from Amanda to Alan: There is an easier way to automate this list, I'm sure of it, but 
        # I'm really tired right now and my contractions are picking up. so while this is a bit more work for 
        # you to edit later, since it requires you edit multiple lists, its less likely I code it wrong right now 
        # because its more straightforward... and thats valuable.
        # 
        # Take any 'leftover' from catch to 2MMT and spread it out across pollock, cod, A80 targets
        # 
        NETTAC <- rowSums(CATCH.PRED, na.rm = TRUE) 
        EXTRACATCH <- as.numeric(NETTAC < 2e6)*(2e6 - NETTAC)
        CATCH.PRED <- CATCH.PRED[order(CATCH.PRED, decreasing = T)]
        
        # If there's any remaining TAC give it to pollock (201), cod (202), yfin sole (140), sablefish (203), atka (204), POP (301), Flathead (103), Rock sole (104), Greenland turbot (102)
        
        # goal: to increase above listed species catch up to ABC/2MMT
        # Note: we're only increasing BS catch.  That's a fun little assumption? If you want to do AI too, just.. add the CATCH.AI.X list.
        # 
        # Here is the list of species that will be getting extra catch.  
        DT <- CATCH.PRED[c("CATCH.BS.201", #pollock
                           "CATCH.BS.202", #cod
                           "CATCH.BS.140", # yfin
                           "CATCH.BS.203", # sablefish
                           "CATCH.BS.204", #atka
                           "CATCH.BS.301", #POP
                           "CATCH.BS.103", #flathead
                           "CATCH.BS.104", #rock sole
                           "CATCH.BS.102")] #greenland
        
        # We need to do the ABCs too.  Easiest to just copy and paste the above list and ctrl+F "in selection" CATCH.BS <- ABC.BSAI
        # Remember there are 5 species that get BS allocated in the bering sea and ai 
        # seperately: pollock, sablefish, other rock fish, POP and Greenaldn turbot.  In these it should be ABC.BS.X
        ABCDT <- FISH.DATA[c("ABC.BS.201", #pollock
                             "ABC.BSAI.202", #cod
                             "ABC.BSAI.140", # yfin
                             "ABC.BS.203", # sablefish
                             "ABC.BSAI.204", #atka
                             "ABC.BS.301", #POP
                             "ABC.BSAI.103", #flathead
                             "ABC.BSAI.104", #rock sole
                             "ABC.BS.102")] #greenland  
        
        # goal: to increase above listed species catch up to ABC/2MMT
        goalamt <- pmin(EXTRACATCH,rowSums(ABCDT) - rowSums(DT))
        
        DT <- distributefun(goalamt,ABCDT,DT) # already coded up on line 149
        
        # Replace old catch with these new ones.
        # One more place you need to add/remove species.  Sorry. I just copy lines 357-end and paste as appropriate, and that works well..
        CATCH.PRED[c("CATCH.BS.201", #pollock
                     "CATCH.BS.202", #cod
                     "CATCH.BS.140", # yfin
                     "CATCH.BS.203", # sablefish
                     "CATCH.BS.204", #atka
                     "CATCH.BS.301", #POP
                     "CATCH.BS.103", #flathead
                     "CATCH.BS.104", #rock sole
                     "CATCH.BS.102")] <- DT[c("CATCH.BS.201", #pollock
                                              "CATCH.BS.202", #cod
                                              "CATCH.BS.140", # yfin
                                              "CATCH.BS.203", # sablefish
                                              "CATCH.BS.204", #atka
                                              "CATCH.BS.301", #POP
                                              "CATCH.BS.103", #flathead
                                              "CATCH.BS.104", #rock sole
                                              "CATCH.BS.102")]#greenland 
        
        
        
    }
    
    # # This is the old version of 5.4, where we scaled from catch to TAC.  This didn't work because, well.. TAC < catch in some cases. (Due to in-season management)
    # I'll eventaully just delete this all, since its a relic and messy, its just here while I'm on Mat leave so other ppl can follow what happened, if need be.
    # 
    # if (scenario == 5.4) {
    #     # improved catch--linear combination of predicted catch and improved catch on a scale of 0 to 1
    #     TAC <- TAC.BOTHBIND[c("TAC.BSAI.141",
    #                           "TAC.BSAI.204",
    #                           "TAC.BSAI.103",
    #                           "TAC.BS.102",
    #                           "TAC.BSAI.147",
    #                           "TAC.BSAI.303",
    #                           "TAC.BSAI.60",
    #                           "TAC.BSAI.100",
    #                           "TAC.BS.310",
    #                           "TAC.BSAI.202",
    #                           "TAC.BSAI.106",
    #                           "TAC.BS.301",
    #                           "TAC.BS.201",
    #                           "TAC.AI.201",
    #                           "TAC.BSAI.104",
    #                           "TAC.BSAI.307",
    #                           "TAC.BS.203",
    #                           "TAC.BSAI.400",
    #                           "TAC.BSAI.65",
    #                           "TAC.BSAI.326",
    #                           "TAC.BSAI.90",
    #                           "TAC.BSAI.50",
    #                           "TAC.BSAI.140")]/2 + TAC.BOTHBIND.FLATSUR[c("TAC.BSAI.141",
    #                                                                       "TAC.BSAI.204",
    #                                                                       "TAC.BSAI.103",
    #                                                                       "TAC.BS.102",
    #                                                                       "TAC.BSAI.147",
    #                                                                       "TAC.BSAI.303",
    #                                                                       "TAC.BSAI.60",
    #                                                                       "TAC.BSAI.100",
    #                                                                       "TAC.BS.310",
    #                                                                       "TAC.BSAI.202",
    #                                                                       "TAC.BSAI.106",
    #                                                                       "TAC.BS.301",
    #                                                                       "TAC.BS.201",
    #                                                                       "TAC.AI.201",
    #                                                                       "TAC.BSAI.104",
    #                                                                       "TAC.BSAI.307",
    #                                                                       "TAC.BS.203",
    #                                                                       "TAC.BSAI.400",
    #                                                                       "TAC.BSAI.65",
    #                                                                       "TAC.BSAI.326",
    #                                                                       "TAC.BSAI.90",
    #                                                                       "TAC.BSAI.50",
    #                                                                       "TAC.BSAI.140")]/2
    # 
    #     catchisTAC <- TAC[c("TAC.BSAI.141")]
    #     names(catchisTAC) <- c("CATCH.BSAI.141")
    #     catchisTAC$CATCH.BSAI.141 <- TAC$TAC.BSAI.141*CATCH.AVG.BS$CODE.141
    #     catchisTAC$CATCH.BSAI.204 <- TAC$TAC.BSAI.204*CATCH.AVG.BS$CODE.204
    #     catchisTAC$CATCH.BSAI.103 <- TAC$TAC.BSAI.103*CATCH.AVG.BS$CODE.103
    #     catchisTAC$CATCH.BS.102 <- TAC$TAC.BS.102
    #     catchisTAC$CATCH.BSAI.147 <- TAC$TAC.BSAI.147*CATCH.AVG.BS$CODE.147
    #     catchisTAC$CATCH.BSAI.303 <- TAC$TAC.BSAI.303*CATCH.AVG.BS$CODE.303
    #     catchisTAC$CATCH.BSAI.60 <- TAC$TAC.BSAI.60*CATCH.AVG.BS$CODE.60
    #     catchisTAC$CATCH.BSAI.100 <- TAC$TAC.BSAI.100*CATCH.AVG.BS$CODE.100
    #     catchisTAC$CATCH.BS.310 <- TAC$TAC.BS.310
    #     catchisTAC$CATCH.BSAI.202 <- TAC$TAC.BSAI.202*CATCH.AVG.BS$CODE.202
    #     catchisTAC$CATCH.BSAI.106 <- TAC$TAC.BSAI.106*CATCH.AVG.BS$CODE.106
    #     catchisTAC$CATCH.BS.301 <- TAC$TAC.BS.301
    #     catchisTAC$CATCH.BS.201 <- (TAC$TAC.BS.201 + TAC$TAC.AI.201)*CATCH.AVG.BS$CODE.201
    #     catchisTAC$CATCH.BSAI.104 <- TAC$TAC.BSAI.104*CATCH.AVG.BS$CODE.104
    #     catchisTAC$CATCH.BSAI.307 <- TAC$TAC.BSAI.307*CATCH.AVG.BS$CODE.307
    #     catchisTAC$CATCH.BS.203 <- TAC$TAC.BS.203
    #     catchisTAC$CATCH.BSAI.400 <- TAC$TAC.BSAI.400*CATCH.AVG.BS$CODE.400
    #     catchisTAC$CATCH.BSAI.65 <- TAC$TAC.BSAI.65*CATCH.AVG.BS$CODE.65
    #     catchisTAC$CATCH.BSAI.326 <- TAC$TAC.BSAI.326*CATCH.AVG.BS$CODE.326
    #     catchisTAC$CATCH.BSAI.90 <- TAC$TAC.BSAI.90*CATCH.AVG.BS$CODE.90
    #     catchisTAC$CATCH.BSAI.50 <- TAC$TAC.BSAI.50*CATCH.AVG.BS$CODE.50
    #     catchisTAC$CATCH.BSAI.140 <- TAC$TAC.BSAI.140*CATCH.AVG.BS$CODE.140
    #     
    #     # TAC.BS DNE for many species!!! 
    # 
    #     output <- improvedcatchscale*catchisTAC + (1-improvedcatchscale)*output
    # } 
    
    
    # This just ensures the output is in a predictable order, and that we are only returning BS catch since this is a BS function.
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