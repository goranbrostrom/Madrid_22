intre <- function(who = "farming"){
    tabpost <- readRDS("data/tabpost_br.rds")
    library(eha)
    ume <- tabpost[tabpost$subreg == "ume", ]
    # Just to experiment:
    ##fit <- tpchreg(oe(event, exposure) ~ (socBranch * socStatus):emeantemp, 
      ##             data = ume, time = age)
    res <- vector(mode = "list", length = 3)
    i <- 0
        fit <- glm(event ~ offset(log(exposure)) + emeantemp * extemp,
                    data = ume, family = poisson()) 
        fit1 <- glm(event ~ offset(log(exposure)) + extemp + extemp %in% emeantemp,
                    data = ume, family = poisson())
        fit2 <- glm(event ~ offset(log(exposure)) + emeantemp + (extemp + extemp.1) %in% emeantemp,
                    data = ume, family = poisson())
        
        fit3 <- tpchreg(oe(event, exposure) ~ emeantemp + (extemp + extemp.1) %in% emeantemp, data = ume[ume$socBranch == who, ],
                       time = age)
    res <- anova(fit2, fit1, fit, test = "Chisq")
    fit1
}