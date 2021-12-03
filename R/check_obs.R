check_obs <- function(){
    library(skum)
    ## Check of age at death, in days.
    qid <- obs$id[obs$starttyp == 2]
    oj <- obs[obs$id %in% qid, 
              c("id", "sex", "startdat", "slutdat", "foddat", "sluttyp")]
    oj <- oj[oj$foddat > as.Date("1894-12-31") & 
                 oj$foddat < as.Date("1951-01-01"), ]
    oj$event <- as.integer(oj$sluttyp == 2)
    oj$sluttyp <- NULL
    oj$denter <- as.double(difftime(oj$startdat, oj$foddat, units = "days"))
    oj$dexit <- as.double(difftime(oj$slutdat, oj$foddat, units = "days")) + 1
    oj <- age.window(oj, c(-1, 28), surv = c("denter", "dexit", "event"))
    ume <- readRDS("data/umetemp.rds")
    oj$exit <- oj$dexit / 365
    oj$enter <- oj$denter / 365
    oj$birthdate <- toTime(oj$foddat)
    ##oj1 <- make.communal(oj, umetemp["week"], start = 1894.9999, period = 1 / 52)
    source("R/mkomm.R")
    oj1 <- mkomm(oj, ume2[, c("year", "week", "cold")], start = 1894, period = 1 / 52)
    ##list(oj = oj, oj1 = oj1)
    oj1
}