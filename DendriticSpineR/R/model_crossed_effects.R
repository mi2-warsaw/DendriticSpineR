#' Function model crossed effects and returns two data frames with model summaries
#'
#' Function calculates models and computes post hoc tests for these models
#' It is assumed that there are two crossed effects.
#'
#' @param data data.frame with data
#' @param var variable of interest
#' @param trans transformation of the var variable before ANOVA
#' @param f1 first effect
#' @param f2 second effect
#' @param strat stratification (Animal)
#' @param mixed if TRUE a mixed model is applied, if FALSE it's just standard linear regression
#'
#' @import lsmeans
#' @import lme4
#'
#' @export

model_crossed_effects <- function(data, var, trans = I, f1="group", f2="condition", strat = "Animal", mixed=TRUE) {
  ndata <- data
  ndata[,var] <- trans(data[,var])

  if (mixed) {
    form <- as.formula(paste0(var, " ~ ", f1, ":", f2, "-1 +(1|",strat,")"))
    mod2 <- lmer(form, ndata)
  } else {
    form <- as.formula(paste0(var, " ~ ", f1, ":", f2, "/factor(",strat,")-1"))
    mod2 <- lm(form, ndata)
  }

  pp <- lsmeans(mod2, pairwise~group:condition)

  r1 <- summary(pairs(pp))
  r2 <- cld(pp, alpha = 0.1, details=FALSE)

  list(pairs = r1,
       cld = r2)
}
