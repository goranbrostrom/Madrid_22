normal <- function(period = c(1894, 1951)){
    ## "normal" stands for "norSJÖ + malÅ"

    
    bjur = "~/Forskning/Data/bjur_temp.csv"
    sten = "~/Forskning/Data/sten_temp.csv"
    stentemp <- read.table(sten, 
                           header = TRUE,
                           skip = 10, sep = ";")[, 1:3]
    bjurtemp <- read.table(bjur,
                           header = TRUE,
                           skip = 10, sep = ";")[, 1:3]
    cre <- function(temp){
    ## We need only the first three columns
        names(temp) <- c("date", "time", "temp")
        temp$date <- as.Date(temp$date)
        temp$year <- as.numeric(format(temp$date, "%Y"))
        temp$month <- as.numeric(format(temp$date, "%m"))
        temp <- temp[order(temp$date, temp$time), ]
        temp$quarter <- quarters(temp$date)
        temp
    
    
        temp <- temp[temp$year %in% period[1]:period[2], ]
        indx <- tapply(temp$date, temp$date)
        temp$mintemp <- tapply(temp$temp, temp$dat, min)[indx]
        temp$maxtemp <- tapply(temp$temp, temp$dat, max)[indx]
        temp$meantemp <- tapply(temp$temp, temp$dat, mean)[indx]
        temp <- temp[!duplicated(temp$date), 
                     c("date", "year", "month", "quarter", "mintemp", "maxtemp", "meantemp")]
    ## Create "week", 1:52 each year, week 52 with 8(9) days:
        get_week <- function(days){ # days = vector of 365 or 366, length of year
            n <- length(days)
            tmp <- c(rep(1:52, each = 7), 52, 52)
            tmp[1:n]
        }
        temp$week <- as.numeric(unlist(tapply(temp$year, temp$year, get_week)))
        temp
    }
    bt <- cre(bjurtemp)
    st <- cre(stentemp)
    temp <- st[st$week %in% c(1:12, 38:52), ]
    temp <- rbind(temp, bt[bt$week %in% 13:37, ])
    temp <- temp[order(temp$date), ]
    
    ## Calculate weekly extremes of min and max temp
    wmin <- aggregate(mintemp ~ week + year, data = temp, FUN = min)
    wmax <- aggregate(maxtemp ~ week + year, data = temp, FUN = max)
    wmean <- aggregate(meantemp ~ week + year, data = temp, FUN = mean)
    ##indx <- tapply(temp$week, temp$week)
    ##wmintemp <- with(temp, tapply(mintemp, list(week, year), min))
    ##wmaxtemp <- with(temp, tapply(maxtemp, list(week, year), max))[indx]
######
    temp <- wmin
    temp$maxtemp <- wmax$maxtemp
    temp$meantemp <- round(wmean$meantemp, 1)
    ##
    indx <- tapply(temp$week, temp$week)
    temp$emintemp <- tapply(temp$mintemp, temp$week, mean)[indx]
    temp$emaxtemp <- tapply(temp$maxtemp, temp$week, mean)[indx]
    temp$emeantemp <- tapply(temp$meantemp, temp$week, mean)[indx]
    temp$heat <- round(temp$maxtemp - temp$emaxtemp)
    temp$cold <- round(temp$mintemp - temp$emintemp)
    temp$extemp <- round(temp$meantemp - temp$emeantemp)
    # NEW 28 Jan 2022:
    n <- NROW(temp)
    temp$cold.1 <- c(NA, temp$cold[-n])
    temp$extemp.1 <- c(NA, temp$extemp[-n])
    temp$heat.1 <- c(NA, temp$heat[-n])
    ##
    temp
}
