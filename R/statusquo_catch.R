statusquo_catch <- function(ABC.DATA) {
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
    
  ## SURSUR and SUROLS
   TAC.PRED.SUR <- predict.tac.function(model="SUR",fit_sur=tac_fit_sur,fit_nosur=tac_fit_nosur,ABC.DATA)
        
        # Add some indicators to the data for catch portion
        
        # TAC.PRED.SUR <- TAC.PRED.SUR %>%
        #     mutate(flex = 1) %>% # introduction of flatfish flex
        #     mutate(A80 = 1) %>%  # introduction of A80
        #     mutate(pollock.UB = as.numeric(TAC.201 >= 1.25e6 & A80 == 1) + as.numeric(TAC.201 >= 1.4e6 & A80==0)) %>% # consider a single UB?
        #     mutate(SSL = 1)%>% # stellar sea lion closure
        #     mutate(solegone = 1) %>%
        #     mutate(plaicegone = 1) %>%
        #     mutate(kamsplit = 1) #%>% filter(YEAR == 2016)
        # 
   
    ABC.DATA <- ABC.DATA %>% mutate(YEAR = 1)
    TAC.PRED.SUR <- TAC.PRED.SUR %>% mutate(YEAR = 1)
    TAC.PRED.SUR <- full_join(ABC.DATA, TAC.PRED.SUR, by = "YEAR") %>% select(-YEAR)
   
    CATCH.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_fit_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    
    CATCH.SURNOSUR <- predict.catch.function(model="NOSUR",fit_sur=catch_fit_sur,fit_nosur=catch_fit_nosur,TAC.PRED.SUR)
    
    # Rock sole not in SUR
     TAC.NOROCKSOLE <- predict.tac.function(model="NOROCKSOLE",fit_sur=tac_NOROCKSOLE_sur,fit_nosur=tac_fit_nosur,ABC.DATA)
        
        # Add some indicators to the data for catch portion
        
        # TAC.NOROCKSOLE <- TAC.NOROCKSOLE %>%
        #     mutate(flex = 1) %>% # introduction of flatfish flex
        #     mutate(A80 = 1) %>%  # introduction of A80
        #     mutate(pollock.UB = as.numeric(TAC.201 >= 1.25e6 & A80 == 1) + as.numeric(TAC.201 >= 1.4e6 & A80==0)) %>% # consider a single UB?
        #     mutate(SSL = 1)%>% # stellar sea lion closure
        #     mutate(solegone = 1) %>%
        #     mutate(plaicegone = 1) %>%
        #     mutate(kamsplit = 1) #%>% filter(YEAR == 2016)
     
        TAC.NOROCKSOLE <- TAC.NOROCKSOLE %>% mutate(YEAR = 1)
        TAC.NOROCKSOLE <- full_join(ABC.DATA, TAC.NOROCKSOLE, by = "YEAR") %>% select(-YEAR)
     
        CATCH.NOROCKSOLE <- predict.catch.function(model="NOROCKSOLE",fit_sur=catch_NOROCKSOLE_sur,fit_nosur=catch_fit_nosur,TAC.NOROCKSOLE)

    # Flatfish has own SUR
    TAC.FLATSUR <- predict.tac.function(model="FLATSUR",fit_sur=tac_FLATSUR_sur,fit_nosur=tac_fit_nosur,ABC.DATA)
        # Add some indicators to the data for catch portion
        
        # TAC.FLATSUR <- TAC.FLATSUR %>%
        #     mutate(flex = 1) %>% # introduction of flatfish flex
        #     mutate(A80 = 1) %>%  # introduction of A80
        #     mutate(pollock.UB = as.numeric(TAC.201 >= 1.25e6 & A80 == 1) + as.numeric(TAC.201 >= 1.4e6 & A80==0)) %>% # consider a single UB?
        #     mutate(SSL = 1)%>% # stellar sea lion closure
        #     mutate(solegone = 1) %>%
        #     mutate(plaicegone = 1) %>%
        #     mutate(kamsplit = 1) #%>% filter(YEAR == 2016)
    
        TAC.FLATSUR <- TAC.FLATSUR %>% mutate(YEAR = 1)
        TAC.FLATSUR <- full_join(ABC.DATA, TAC.FLATSUR, by = "YEAR") %>% select(-YEAR)
    
        CATCH.FLATSUR <- predict.catch.function(model="FLATSUR",fit_sur=catch_FLATSUR_sur,fit_nosur=catch_fit_nosur,TAC.FLATSUR)

    # No First Year Data SURSUR and SUROLS
    TAC.NOFIRSTYEAR  <- predict.tac.function(model="SUR",fit_sur=tac_NOFIRSTYEAR_sur,fit_nosur=tac_fit_nosur,ABC.DATA)
        
        # Add some indicators to the data for catch portion
        
        # TAC.NOFIRSTYEAR <- TAC.NOFIRSTYEAR %>%
        #     mutate(flex = 1) %>% # introduction of flatfish flex
        #     mutate(A80 = 1) %>%  # introduction of A80
        #     mutate(pollock.UB = as.numeric(TAC.201 >= 1.25e6 & A80 == 1) + as.numeric(TAC.201 >= 1.4e6 & A80==0)) %>% # consider a single UB?
        #     mutate(SSL = 1)%>% # stellar sea lion closure
        #     mutate(solegone = 1) %>%
        #     mutate(plaicegone = 1) %>%
        #     mutate(kamsplit = 1) #%>% filter(YEAR == 2016))) 
        
        TAC.NOFIRSTYEAR <- TAC.NOFIRSTYEAR %>% mutate(YEAR = 1)
        TAC.NOFIRSTYEAR <- full_join(ABC.DATA, TAC.NOFIRSTYEAR, by = "YEAR") %>% select(-YEAR)
     
        CATCH.NOFIRSTYEAR.SURSUR <- predict.catch.function(model="SUR",fit_sur=catch_NOFIRSTYEAR_sur,fit_nosur = catch_NOFIRSTYEAR_nosur,TAC.NOFIRSTYEAR )
        CATCH.NOFIRSTYEAR.SURNOSUR <- predict.catch.function(model="NOSUR",fit_sur=catch_NOFIRSTYEAR_sur,fit_nosur = catch_NOFIRSTYEAR_nosur,TAC.NOFIRSTYEAR )
        
        
    # create ensemble
    # 
    CATCH.PRED <-  (CATCH.SURSUR + CATCH.SURNOSUR + CATCH.NOROCKSOLE + CATCH.FLATSUR + CATCH.NOFIRSTYEAR.SURNOSUR + CATCH.NOFIRSTYEAR.SURSUR)/6
    
    
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