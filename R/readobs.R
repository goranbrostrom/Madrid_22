readobs <- function(){
    ## Equal to Code Chunk 'readobs' in getdata2.Rmd
    ## Bug tracking!
    library(skum)
    ###
    bb <- obs$id[obs$starttyp == 2]
    births <- obs[obs$id %in% bb, 
                  c("id", "sex", "foddat", "startdat", "slutdat", "nofrs", "sluttyp", "socStatus", "hisclass", 
                    "ort", "ortnmn")]
    births$event <- (births$sluttyp == 2)
    ## New start in days...:
    births$enter <- as.numeric(with(births, difftime(startdat, foddat, unit = "days")))
    births$exit <- as.numeric(with(births, difftime(slutdat, foddat, unit = "days")))
    ##
    expand <- (births$startdat == births$slutdat) & births$event
    births$exit[expand] <- births$enter[expand] + 0.25 # 6 hours!
    ##
    weq <- (births$startdat == births$slutdat) & (!births$event)

    births <- births[!weq, ]
    ##
    births <- age.window(births, c(0, 365))
    births <- births[order(births$id, births$enter), ]
    ##return(births)
    ################################### 1st check OK!!!!
    ##indx <- tapply(births$id, births$id)
    ##births$mexit <- tapply(births$exit, births$id, max)[indx]
    ##births$mevent <- tapply(births$event, births$id, max)[indx]
    ##births <- births[!duplicated(births$id), ]
    ### Deaths:
    did <- unique(births$id[births$event == 1]) # NOTE: 'age.window' changes event = logical to numeric!
    deaths <- births[births$id %in% did, ]
    indx <- tapply(deaths$id, deaths$id)
    deaths$menter <- tapply(deaths$enter, deaths$id, min)[indx]
    ##return(deaths)
    ################################# 2nd check
    ## NEW: Sort before unique:
    deaths <- deaths[order(deaths$exit, decreasing = TRUE), ]
    deaths <- deaths[!duplicated(deaths$id), ]
    deaths$enter <- deaths$menter
    return(deaths)
    ################################## 3rd check
    deaths$menter <- NULL
    deaths$enter <- 0 ## satisfied....
    ### Survivors:
    survs <- births[!(births$id %in% did), ]
    indx <- tapply(survs$id, survs$id)
    survs$mexit <- tapply(survs$exit, survs$id, max)[indx]
    survs <- survs[!duplicated(survs$id), ]
    survs$exit <- survs$mexit
    survs$mexit <- NULL
    survs$enter <- 0 ## Ok ...
    ### Now, give birth to NEW births:
    births <- rbind(deaths, survs)
    births <- births[births$foddat > as.Date("1894-12-31"), ]
    
    indx <- match(births$id, pers$id)
    births$ab <- pers$ab[indx]
    ##births$df <- pers$df[indx]
    births$illeg <- factor(births$ab == 2, labels = c("no", "yes"))
    births$ab <- NULL
    births$sluttyp <- NULL
    births$parity <- pers$paritet_g[indx]
    ## Add:
    births$birthdate <- toTime(births$foddat)
    ##
    births$exit <- births$exit / 365 ## Measure time in years!!
    
}