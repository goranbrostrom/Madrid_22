check_obs <- function(){
    library(skum)
    ## Check of age at death, in days.
    qid <- obs$id[obs$starttyp == 2]
    oj <- obs[obs$id %in% qid, c("id", "startdat", "slutdat", "foddat", "sluttyp")]
    oj <- oj[oj$foddat > as.Date("1894-12-31") & oj$foddat < as.Date("1951-01-01"), ]
    oj$event <- as.integer(oj$sluttyp == 2)
    oj$sluttyp <- NULL
    oj$enter <- as.double(difftime(oj$startdat, oj$foddat, units = "days"))
    oj$exit <- as.double(difftime(oj$slutdat, oj$foddat, units = "days"))
    oj <- age.window(oj, c(-1, 28))
    
    oj
}