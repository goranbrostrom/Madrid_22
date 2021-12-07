testcomm <- function(){
    ##lagg <- 3 / 365  ## NOTE!!
    lagg <- 0
    temp_start <- 1894
    ## Ume:
    umetemp <- readRDS("../data/umetemp.rds")
    ##bu10 <- readRDS("bu10.rds")
    bumeo <- readRDS("../data/births.rds")
    bume <- make.communal(bumeo, umetemp["week"], 
                          start = temp_start, period = 1 / 52, lag = lagg)
    ##bume <- bume[bume$enter < bume$exit, ]
    bume <- make.communal(bume, umetemp["year"], 
                          start = temp_start, period = 1 / 52, lag = lagg)
    ##bume <- bume[bume$enter < bume$exit, ]
    ##
    bume <- make.communal(bume, umetemp["heat"], 
                          start = temp_start, period = 1/52, lag = lagg)
    ##bume <- bume[bume$enter < bume$exit, ]
    bume <- make.communal(bume, umetemp["cold"], 
                          start = temp_start, period = 1 / 52, lag = lagg)
    ##bume <- bume[bume$enter < bume$exit, ]
    ## mkomm:
    cat("mkomm:\n")
    source("../R/mkomm.R")
    b2 <- mkomm(bumeo, umetemp[, c("week", "year", "heat", "cold")], 
                start = temp_start, period = 1 / 52, lag = lagg)
    ## Handräkning:
    ##endt <- seq(0, 1, by = 53)
    
    ##
    cat("Diff = ", with(bume, sum(exit - enter)) - with(b2, sum(exit - enter)), "\n")
    list(bume = bume, b2 = b2)
}
