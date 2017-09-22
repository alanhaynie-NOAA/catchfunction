#' Catch Function
#'
#' @description 
#' This function predicts the BSAI catch for each species whose ABC is given.  It is meant to work with the ACLIM bio models.
#' 
#' If you have any questions, please contact me (e-mail: amanda.faig@noaa.gov, call: X-4281).
#' 
#' Currently programmed scenarios: Scenario 1 & Scenario 3
#' 
#' @param scenario The economic scenario number (1 through 5). 1: Status Quo, 2: Eliminate 2 million ton cap, 3: No fishing, 4: MEY, 5: "Fleet Dynamics"
#' @param Arrowtooth Optional.  ABC of Arrowtooth Flounder.
#' @param Atka Optional.  ABC of Atka Mackerel.
#' @param Flathead Optional.  ABC of Flathead Sole.
#' @param Greenland Optional.  ABC of Greenland Turbot.
#' @param Kamchatka Optional.  ABC of Kamchatka Flounder.
#' @param Northern Optional.  ABC of Northern Rockfish.
#' @param Octopus Optional.  ABC of Octopus.
#' @param OtherFlat Optional.  ABC of Other Flatfish.
#' @param OtherRock Optional.  ABC of Other Rockfish.
#' @param PCod Optional.  ABC of Pacific Cod.
#' @param Plaice Optional.  ABC of Alaska Plaice.
#' @param POP Optional.  ABC of Pacific Ocean Perch.
#' @param Pollock Optional.  ABC of Pollock.
#' @param Rock Optional.  ABC of Rock Sole.
#' @param Rougheye Optional.  ABC of Rougheye Rockfish.
#' @param Sablefish Optional.  ABC of Sablefish.
#' @param Sculpin Optional.  ABC of Sculpin.
#' @param Shark Optional.  ABC of Shark.
#' @param Shortraker Optional.  ABC of Shortraker Rockfish.
#' @param Skate Optional.  ABC of Skate.
#' @param Squid Optional.  ABC of Squid.
#' @param Yellowfin Optional.  ABC of Yellowfin Sole.
#' 
#' 
#' @import dplyr
#' @import systemfit
#'
#' @export
#' 
#' @examples 
#' catchfunction(1, Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)
#' catchfunction(3, Pollock = 2e6, Yellowfin = 2e5, PCod = 1e5)

catch_function <- function(scenario, 
                          Arrowtooth, 
                          Atka, 
                          Flathead, 
                          Greenland, 
                          Kamchatka, 
                          Northern, 
                          Octopus, 
                          OtherFlat, 
                          OtherRock, 
                          PCod, 
                          Plaice, 
                          POP, 
                          Pollock, 
                          Rock, 
                          Rougheye, 
                          Sablefish, 
                          Sculpin, 
                          Shark,
                          Shortraker, 
                          Skate, 
                          Squid, 
                          Yellowfin) {
    
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

    missingspp <- c(missing(Arrowtooth), 
                  missing(Atka), 
                  missing(Flathead), 
                  missing(Greenland), 
                  missing(Kamchatka), 
                  missing(Northern), 
                  missing(Octopus), 
                  missing(OtherFlat), 
                  missing(OtherRock), 
                  missing(PCod), 
                  missing(Plaice), 
                  missing(POP), 
                  missing(Pollock), 
                  missing(Rock), 
                  missing(Rougheye), 
                  missing(Sablefish), 
                  missing(Sculpin), 
                  missing(Shark),
                  missing(Shortraker), 
                  missing(Skate), 
                  missing(Squid), 
                  missing(Yellowfin))
    
    # mean.sd <- ABC_means
    # 
    # singledraw <- function(code) { 
    #     mu <- mean.sd %>% select(ends_with(paste(code,"mean",sep="_")))
    #     mu <- as.numeric(mu[1])
    #     # sig <- mean.sd %>% select(ends_with(paste(code,"sd",sep="_")))
    #     # sig <- as.numeric(sig[1])
    #     # draw <- round(rnorm(1,mu,sig))
    #     # 
    #     draw <- mu
    #     return(draw)
    # }
    
    ## First, create a data frame of all the ABCs. Draw unspecified species from 
    ## stationary distributions
    
    # ABC.DATA <- data.frame(ABC.BS.141 = NA)
    # 
    # for (i in 1:22) {
    #     if (missingspp[i]) {
    #         ABC.DATA$ABC <- singledraw(allspp[i])
    #     } else {
    #         ABC.DATA$ABC <- eval(parse(text = sppnames[i]))
    #     }
    #     colnames(ABC.DATA)[i] <- paste("ABC",allspp[i],sep=".")
    #     if (allspp[i] == 307) {ABC.DATA$ABC.307 <- 390} # 2004 was an outlier that should be removed, but changing the built in data right now is more likely to lead to glitches.  Fix up later.
    #     }
    #     
    ABC.DATA <- mean.BS.AI.ABCs

    for (i in 1:22) {
        if (!missingspp[i]) {
            eval(parse(text = paste("ABC.DATA$ABC.BS.",allspp[i],"<-",sppnames[i],sep="")))
        }
    }
    
    ## Create BSAI where necessary
    ABC.DATA <- ABC.DATA %>%
        mutate(ABC.BSAI.100 = ABC.BS.100 + ABC.AI.100) %>%
        mutate(ABC.BSAI.103 = ABC.BS.103 + ABC.AI.103) %>%
        mutate(ABC.BSAI.104 = ABC.BS.104 + ABC.AI.104) %>%
        mutate(ABC.BSAI.106 = ABC.BS.106 + ABC.AI.106) %>%
        mutate(ABC.BSAI.140 = ABC.BS.140 + ABC.AI.140) %>%
        mutate(ABC.BSAI.141 = ABC.BS.141 + ABC.AI.141) %>%
        mutate(ABC.BSAI.147 = ABC.BS.147 + ABC.AI.147) %>%
        mutate(ABC.BSAI.204 = ABC.BS.204 + ABC.AI.204) %>%
        mutate(ABC.BSAI.303 = ABC.BS.303 + ABC.AI.303) %>%
        mutate(ABC.BSAI.307 = ABC.BS.307 + ABC.AI.307) %>%
        mutate(ABC.BSAI.326 = ABC.BS.326 + ABC.AI.326) %>%
        mutate(ABC.BSAI.400 = ABC.BS.400 + ABC.AI.400) %>%
        mutate(ABC.BSAI.50 = ABC.BS.50 + ABC.AI.50) %>%
        mutate(ABC.BSAI.60 = ABC.BS.60 + ABC.AI.60) %>%
        mutate(ABC.BSAI.65 = ABC.BS.65 + ABC.AI.65) %>%
        mutate(ABC.BSAI.90 = ABC.BS.90 + ABC.AI.90) %>%
        mutate(ABC.BSAI.202 = ABC.BS.202 + ABC.AI.202) 

    ## Second, pass ABCs to status quo function to get catch
    
    if (scenario == 1) {
        catch <- statusquo_catch(ABC.DATA)
        
    } else if (scenario == 2) {
        print("Scenario 2: No 2MT cap. This scenario not yet programmed.")
    } else if (scenario == 3) {
       catch <- statusquo_catch(ABC.DATA)*0
    } else if (scenario == 4) {
        print("Scenario 4: MEY.  Speak with Steve about this one.")
    } else if (scenario == 5) {
        print("Scenario 5: Fleet dynamics.  See notes from socio-econ workshop")
    }
    
    # Third, pick only species that were passed in to pass back out.
    output <- catch[!missingspp]
    colnames(output) <- sppnames[!missingspp]
    #return(ABC.DATA)
    return(output)
    
}

    
