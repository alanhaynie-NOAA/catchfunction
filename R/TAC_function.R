#' TAC Function
#'
#' @description 
#' This function predicts the BSAI TAC for each species whose ABC is given.  
#' 
#' NOTE: TAC IS BSAI FOR ALL SPECIES EXCEPT THE FOLLOWING: Pollock, Greenland Turbot, Sablefish, Pacific Ocean Perch, and Other Rockfish.  
#' This is because these are the only species for which TAC is set at the BS level, all other species are set at the BSAI level.*
#' *Pacific Cod is the exception--it is currently set at the BS level, however this is a new change so our model at the moment does not model PCod TAC at the BS level.
#' 
#' Scaling it to BS is in progress. 
#' 
#' If you have any questions, please contact Amanda Faig (e-mail: amanda.faig@noaa.gov, call: X-4281).
#' 
#' This version last updated Feb 2018
#' 
#' Currently programmed scenarios: \cr
#' Scenario 1: Status Quo (Log-Linear) \cr
#' Scenario 2: Whitefish (Pollock and Cod) Political (aka TAC-setting) Preference \cr
#' Scenario 3: Flatfish Political (aka TAC-setting) Preference \cr
#' Scenario 4: No Fishing (will return all zeros) \cr
#' Scenario 5.1: Fiddle with a single species--calculate the rest still taking the ABC of the removed sp. in to account. \cr
#' Scenario 5.2: Fiddle with a single species--calculate the rest assuming the ABC of the removed sp. does not influence the sp. under the cap at all. \cr
# Scenario 5.3: Fiddle with a single species--calculate the rest assuming the ABC of the removed sp. does not influence the sp. under the cap at all and then increase the TAC of all the remaining species until the sum of the tAC = 2mmt \cr
#' 
#' @param scenario The economic scenario number. Current options: 1, 2, 3, 4, 5.1, 5.2, or 5.3
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
#' @param spptomult Required if running any of the 5-series scenarios.  Will be discarded otherwise.  Choose a species catch to override with N*ABC.  Must be spelt exactly as one of the species parameters, case sensitive.  Must be in quotation marks.  If you want to replace more than one species, create a vector of strings (e.g. c("Arrowtooth","Atka"))
#' @param multiplier Required if running scenario 5-series scenarios.  Will be discarded otherwise.  The N which will be multiplied with ABC to override the species designated by spptomult. If you are replacing more than one species, the order of the numbers corresponds to the order of the names in the spptomult string. (e.g. c(1,5) would imply the first species listed in spptomult has its catch replaced with 1*ABC_spp1 and the second is replaced with 5*ABC_spp2)
#' 
#' @import systemfit
#'
#' @export
#' 
#' @examples 
#' TAC_function(1, Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)
#' TAC_function(3, Pollock = 2e6, Yellowfin = 2e5, PCod = 1e5)
#' TAC_function(5.1, spptomult = "Arrowtooth", multiplier = 2, 
#'                  Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)
#' TAC_function(5.1, spptomult = c("Arrowtooth","Yellowfin"), multiplier = c(0.5,1), 
#'                  Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)
#' TAC_function(5.2, spptomult = "Arrowtooth", multiplier = 2, 
#'                  Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)
#' TAC_function(5.2, spptomult = c("Arrowtooth","Yellowfin"), multiplier = c(0.5,1), 
#'                  Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)
# TAC_function(5.3, spptomult = c("Arrowtooth","Yellowfin"), multiplier = c(0.5,1), 
#                  Pollock = 2e6, Arrowtooth = 2e5, Yellowfin = 2e5)#'                  

# Above is what creates the help document.  It's easier to read by 
# running ?TAC_function once you've loaded the package.
# 
# Below is the function users call.
TAC_function <- function(scenario, 
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
                           Yellowfin,
                           spptomult,
                           multiplier) {
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
    # Figure out which species' ABCs WERE NOT passed through by the user.
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
    
    if (scenario %in% c(5.1,5.2,5.3)) { # if we're running scenario 5 we need some checks to make sure the inputs work well.
        if (sum(spptomult %in% sppnames) < length(spptomult)) {stop("spptomult needs to be match one of the species inputs exactly.  Check spelling and capitalization compared to help file.")}
        if (missing(spptomult) | missing(multiplier)) {stop("Scenario 5 requires that a species to override catch with N*ABC be designated, and also that the multiplier (N) is desginated.  Check that you have both.")}
        
    }
    
    
    # Load the mean ABCs from system data.  This was calculated by just finding a 
    # simple mean using all the available data.
    ABC.DATA <- mean.BS.AI.ABCs
    
    
    # For any species given (not missing) replace the ABC.DATA$ABC.BS mean with the given.
    # Also, replace ABC.DATA$ABC.AI mean with the relative increase/decrease in the BS.
    # (i.e. assume AI ABC rises and falls with BS.)
    for (i in 1:22) {
        if (!missingspp[i]) {
            eval(parse(text = paste("ABC.DATA$ABC.BS.",allspp[i],"<-",sppnames[i],sep="")))
            eval(parse(text = paste("changeinABC <- ABC.DATA$ABC.BS.",allspp[i],"/mean.BS.AI.ABCs$ABC.BS.",allspp[i],sep="")))
            eval(parse(text = paste("ABC.DATA$ABC.AI.",allspp[i],"<-",changeinABC,"*mean.BS.AI.ABCs$ABC.AI.",allspp[i],sep="")))
        }
    }
    
    
    ## Create BSAI where necessary
    
    BSAIfun <- function(DT, code) {
        if (eval(parse(text= paste("DT$ABC.BS.",code,"[1] == 0",sep="")))) {
            # If the ABC passed through was 0, assume entire BSAI ABC is wiped out (0)
            eval(parse(text= paste("DT$ABC.BSAI.",code," <- DT$ABC.BS.",code,sep="")))
        } else {
            # Otherwise, assume BSAI ABC is the sum of BS and AI.  
            eval(parse(text= paste("DT$ABC.BSAI.",code,"<- DT$ABC.BS.",code,"+ DT$ABC.AI.",code,sep="")))
        }
        return(DT)
    }
    
    # Honestly the first else in the above loop should be redundant, since we already 
    # assume AI rises and falls with BS.  At some point I probably should remove this redundancy.
    
    ABC.DATA <- BSAIfun(ABC.DATA,"100")
    ABC.DATA <- BSAIfun(ABC.DATA,"103")
    ABC.DATA <- BSAIfun(ABC.DATA,"104")
    ABC.DATA <- BSAIfun(ABC.DATA,"106")
    #ABC.DATA <- BSAIfun(ABC.DATA,"140")
    ABC.DATA$ABC.BSAI.140 <- ABC.DATA$ABC.BS.140  
    # For yellowfin (140), assume BS ABC makes up entire BSAI. In practice it appears the AI ABC is ignored when decision making/harvesting.
    ABC.DATA <- BSAIfun(ABC.DATA,"141")
    ABC.DATA <- BSAIfun(ABC.DATA,"147")
    ABC.DATA <- BSAIfun(ABC.DATA,"204")
    ABC.DATA <- BSAIfun(ABC.DATA,"303")
    ABC.DATA <- BSAIfun(ABC.DATA,"307")
    ABC.DATA <- BSAIfun(ABC.DATA,"326")
    ABC.DATA <- BSAIfun(ABC.DATA,"400")
    ABC.DATA <- BSAIfun(ABC.DATA,"50")
    ABC.DATA <- BSAIfun(ABC.DATA,"60")
    ABC.DATA <- BSAIfun(ABC.DATA,"65")
    ABC.DATA <- BSAIfun(ABC.DATA,"90")
    ABC.DATA <- BSAIfun(ABC.DATA,"202")
    
    
    ## Second, pass ABCs to status quo function to get TAC
    
    if (scenario == 1) {
        TAC <- ensemble_fun(ABC.DATA,1,"TAC")
        # log linear
        # } else if (scenario == 1.1) {
        #    TAC <- ensemble_fun(ABC.DATA,1.1,"TAC")
        # log log
    } else if (scenario == 2) {
        TAC <- ensemble_fun(ABC.DATA,2,"TAC")
    } else if (scenario == 3) {
        TAC <- ensemble_fun(ABC.DATA,3,"TAC")
    } else if (scenario == 4) {
        TAC <- ensemble_fun(ABC.DATA,1,"TAC")*0
    } else if (scenario == 5.1) {
        TAC <- ensemble_fun(ABC.DATA,1,"TAC")
    } else if (scenario == 5.2) {
        for (i in 1:length(spptomult)) {
            eval(parse(text = paste("ABC.DATA$ABC.BSAI.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
            eval(parse(text = paste("ABC.DATA$ABC.BS.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
            eval(parse(text = paste("ABC.DATA$ABC.AI.",allspp[match(spptomult[i],sppnames)],"<- 0",sep="")))
        } 
        TAC <- ensemble_fun(ABC.DATA,1,"TAC")
    } else if (scenario == 5.3) {
        TAC <- removefromcap_TAC(ABC.DATA,5.3,spptomult,0)
    } 
    
    
    # Third, pick only species that were passed in to pass back out.
    output <- TAC[!missingspp]
    colnames(output) <- sppnames[!missingspp]
    output[is.na(output)] <- 0
    
    if (scenario == 5.1 | scenario == 5.3) {  # in scenario 5 override.
        for (i in 1:length(spptomult)) {
            eval(parse(text = paste("output$",spptomult[i],"<-",spptomult[i],"*",multiplier[i],sep="")))
        }
    } 
    
    return(output)
    
    #return(TAC)
    #return(ABC.DATA)
    
    
    
}