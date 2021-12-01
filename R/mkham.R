fun <- function(beta, enter, exit, event){
    shape <- numeric(2)
    scale <- 1 / beta[3]
    shape[1] <- exp(beta[1])
    shape[2] <- exp(beta[2])
    p1 <- sum(event * hmakeham(exit, shape = shape, scale = scale, log = TRUE))
    p2 <- sum(Hmakeham(exit, shape = shape, scale = scale))
    p3 <- sum(Hmakeham(enter, shape = shape, scale = scale))
    p1 - p2 + p3
}

maxlog <- function(enter, exit, event){
    beta <- numeric(3)
    beta[3] <- 1
    res <- optim(beta, fun, enter = enter, exit = exit, event = event,
                 control = list(fnscale = -1, trace = 2))
    res
}

testmk <- function(){
    x <- readRDS("data/infdat.rds")
    fit <- maxlog(infdat$enter, x$exit, x$event)
    fit
}