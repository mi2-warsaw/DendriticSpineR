#' Function plots crossed effects
#'
#' Function \code{plot_crossed_effects} calculates averages and sd's
#' (with the use of fixed or mixed models) and plots them.
#'
#' @details
#' Function \code{plot_crossed_effects} calculates averages and sd's
#' (with the use of fixed or mixed models) and plots them.
#' It is assumed that there are two crossed effects. Also if you do not
#' choose both transformations which should be used during working with
#' data, the function will match them to chosen property by itself.
#'
#' @usage plot_crossed_effects(spines, property = "length", trans = I, inv = I,
#'    strat = colnames(spines)[1], mixed = TRUE, addpoints = FALSE)
#'
#' @param spines a data.frame of spines class
#' @param property a character with property variable; default: "length"
#' @param trans a transformation of the property variable before ANOVA; default: NA
#' @param inv an inverse transformation of the trans(property); default: NA
#' @param strat a stratification (Animal); default: colnames(spines)[1]
#' @param mixed if TRUE a mixed model is applied, if FALSE it's just standard
#' linear regression; default: TRUE
#' @param addpoints if TRUE additional points for each group/condition/strat group
#' will be added; default: FALSE
#'
#' @return a crossed effects plot
#'
#' @import ggplot2
#' @import lme4
#' @import dplyr
#' @import tidyr
#' @importFrom stringi stri_detect_regex
#'
#' @export

plot_crossed_effects <- function(spines, property = "length", trans = I, inv = I, strat = colnames(spines)[1],
                                 mixed = TRUE, addpoints = FALSE){
  UseMethod("plot_crossed_effects")
}

#' @export

plot_crossed_effects.spines <- function(spines, property = "length", trans = NA, inv = NA,
                                        strat = colnames(spines)[1], mixed = TRUE, addpoints = FALSE){
  stopifnot(is.data.frame(spines), is.character(property), length(property) == 1,
            (suppressWarnings(is.na(trans)) || is.function(trans)),
            suppressWarnings((is.na(inv)) || is.function(inv)), is.character(strat),
            is.logical(mixed), is.logical(addpoints))

  if(stri_detect_regex(strat, ":") && mixed == FALSE){
    stop("Standard linear regression cannot be applied for this stratification! Remove ':'.")
  } else if(stri_detect_regex(strat, ":") && addpoints == TRUE){
    stop("Ppoints cannot be added for this stratification! Remove ':'.")
  }

  col_names <- colnames(spines)
  strats <- unlist(strsplit(strat, split=":"))
  if(!all(strats %in% col_names)){
    stop(paste0("Some of words in chosen stratification '", strat, "' do not match to columns ",
                "in spines data."))
  }

  if(!(property %in% col_names)){
    stop(paste0("There is not any column in spines data called ", property, "."))
  }

  #transformations
  if(!(is.function(trans) && is.function(inv))){
    transfs <- transformations(property)
    trans <- transfs[[1]]
    inv <- transfs[[2]]
  }

  ndata <- spines
  ndata[, property] <- trans(spines[, property])
  f1 <- col_names[3]
  f2 <- col_names[4]

  #plot
  if (mixed) {
    form <- as.formula(paste0(property, " ~ ", f1, ":", f2, "-1 + (1|", strat, ")"))
    ss <- summary(lmer(form, ndata))$coef[, 1:2]
    co2 <- data.frame(n = rownames(ss), ss)
  } else {
    form <- as.formula(paste0(property, " ~ ", f1, ":", f2, "/factor(", strat, ")-1"))
    model <- summary(lm(form, ndata))
    co2 <- data.frame(n = rownames(model$coef), model$coef[, 1:2])
    co2 <- co2[!grepl(co2$n, pattern = "factor(", fixed = TRUE),]
  }

  co2 <- co2 %>% dplyr::mutate(l1 = inv(Estimate - Std..Error),
                               l2 = inv(Estimate),
                               l3 = inv(Estimate + Std..Error)) %>%
    extract(n, c(f1, f2), paste0(f1, "([[:alnum:]]+):", f2, "([[:alnum:]]+)"))

  pl <- ggplot(co2, aes_string(x=f1, color=f2, ymin="l1", y="l2", ymax="l3")) +
    geom_errorbar(width=0.1, position=position_dodge(width=0.2)) +
    geom_point(size=5, position=position_dodge(width=0.2)) +
    theme_bw() + ylab(property)

  if (addpoints) {
    fdata <- ndata
    if (length(grep(strat, pattern = ":")) > 0)
      stop("the strat argument cannot contain ':' when ppoints are added")
    fdata$fake_var <- fdata[, property]
    fdata$fake_f1 <- fdata[, f1]
    fdata$fake_f2 <- fdata[, f2]
    fdata$fake_strat <- fdata[, strat]
    fpoints <- fdata %>%
      group_by(fake_f1, fake_f2, fake_strat) %>%
      summarise(meds = inv(mean(fake_var)))
    pl <- pl +
      geom_point(data=fpoints, aes(x=fake_f1, color=fake_f2, y=meds, ymin=meds, ymax=meds))
  }

  return(pl)
}
