## Gompertz-Makeham functions

hgm <- function(t, a, log = FALSE){
    ## Hazard function:
    ## a is a vector of length 3:
    ## hgm(t) = exp(a[1]) + exp(a[2] + a[3] * t)
    ret <- exp(a[1]) + exp(a[2] + a[3] * t)
    if (log) ret <- log(ret)
    ret
}

hgm1 <- function(t, a){
    ## Derivative wrt a[1] of hgm(log = FALSE)
    exp(a[1])
}

hgm2 <- function(t, a){
    ## Derivative wrt a[2].
    exp(a[2] + a[3] * t)
}

hgm3 <- function(t, a){
    ## Derivative wrt a[3].
    t * exp(a[2] + a[3] * t)
}
    
Hgm <- function(t, a){
    ## Cumulative hazards function,
    ## has length 3.
    t * exp(a[1]) - exp(a[2]) * expm1(a[3] * t) / a[3]
}

Hgm1 <- function(t, a){
    t * exp(a[1])
}

Hgm2 <- function(t, a){
    -exp(a[2]) * expm1(a[3] * t) / a[3]
}

Hgm3 <- function(t, a){
    p1 <- expm1(a[3] * t) / a[3]
    p2 <- t * exp(a[3] * t)
    exp(a[2]) * (p1 - p2) / a[3]
}

loggm <- function(a, enter, exit, event){
    ## log likelihood function
    ## a is vector of length 3 (parameter)
    ## enter, exit, event are equal-length vectors (data).
    p1 <- sum(event * hgm(exit, a, log = TRUE))
    p2 <- sum(Hgm(enter, a) - Hgm(exit, a))
    p1 + p2
}

dloggm <- function(a, enter, exit, event){
    ## Score function(s)

    sco <- numeric(3)
    sco[1] <- sum(event * hgm1(exit, a) / hgm(exit, a) +
                  Hgm1(enter, a) - Hgm1(exit, a))
    sco[2] <- sum(event * hgm2(exit, a) / hgm(exit, a) +
                  Hgm2(enter, a) - Hgm2(exit, a))
    sco[3] <- sum(event * hgm3(exit, a) / hgm(exit, a) +
                  Hgm3(enter, a) - Hgm3(exit, a))
    sco
}
