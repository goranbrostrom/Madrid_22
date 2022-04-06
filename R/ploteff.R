ploteff <- function(){
    library(effects)
    library(eha)
    post <- readRDS("data/tabpost.rds")
    fit <- glm(event ~ offset(log(exposure)) + age + urban + emeantemp:urban, data = post[!post$summer, ],
               family = poisson())
    fit.tp <- tpchreg(oe(event, exposure) ~ urban + emeantemp:urban, data = post[!post$summer, ], time = age)
    ##plot(predictorEffect(mod = fit, predictor = "emeantemp"))
    list(fit = fit, fit.tp = fit.tp)
}