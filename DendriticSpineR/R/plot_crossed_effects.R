#' Function plots crossed effects
#'
#' Function calculates averages and sd's (with the use of fixed or mixed models) and plots them.
#' It is assumed that there are two crossed effects.
#'
#' @param data data.frame with data
#' @param var variable of interest
#' @param trans transformation of the var variable before ANOVA
#' @param inv inverse transformation of the trans(var)
#' @param f1 first effect
#' @param f2 second effect
#' @param strat stratification (Animal)
#' @param mixed if TRUE a mixed model is applied, if FALSE it's just standard linear regression
#' @param addpoints if TRUE additional points for each f1/f2/strat group will be added
#'
#' @import ggplot2
#' @import lme4
#' @import dplyr
#'
#' @export

plot_crossed_effects <- function(data, var, trans = I, inv = I, f1="group", f2="condition", 
                                 strat = "Animal", mixed=TRUE, addpoints=FALSE) {
  ndata <- data
  ndata[,var] <- trans(data[,var])
  
  if (mixed) {
    form <- as.formula(paste0(var, " ~ ", f1, ":", f2, "-1 + (1|",strat,")"))
    ss <- summary(lmer(form, ndata))$coef[,1:2]
    co2 <- data.frame(n = rownames(ss), ss)
  } else {
    form <- as.formula(paste0(var, " ~ ", f1, ":", f2, "/factor(",strat,")-1"))
    model <- summary(lm(form, ndata))
    co2 <- data.frame(n = rownames(model$coef), model$coef[,1:2])
    co2 <- co2[!grepl(co2$n, pattern = "factor(", fixed = TRUE),]
  }

  co2 <- co2 %>% dplyr::mutate(l1 = inv(Estimate - Std..Error),
                  l2 = inv(Estimate),
                  l3 = inv(Estimate + Std..Error)) %>%
    extract(n, c(f1, f2), paste0(f1, "([[:alnum:]]+):", f2, "([[:alnum:]]+)"))

  pl <- ggplot(co2, aes_string(x=f1, color=f2, ymin="l1", y="l2", ymax="l3")) +
    geom_errorbar(width=0.1, position=position_dodge(width=0.2)) +
    geom_point(size=5, position=position_dodge(width=0.2)) +
    theme_bw() + ylab(var)
  
  if (addpoints) {
    fdata <- ndata
    if (length(grep(strat, pattern = ":")) > 0) 
      stop("the strat argument cannot contain ':' when ppoints are added")
    fdata$fake_var <- fdata[,var]
    fdata$fake_f1 <- fdata[,f1]
    fdata$fake_f2 <- fdata[,f2]
    fdata$fake_strat <- fdata[,strat]
    fpoints <- fdata %>%
      group_by(fake_f1,fake_f2,fake_strat) %>%
      summarise(meds = inv(mean(fake_var)))
    pl <- pl + 
      geom_point(data=fpoints, aes(x=fake_f1, color=fake_f2, y=meds, ymin=meds, ymax=meds))
  }

  pl
}
