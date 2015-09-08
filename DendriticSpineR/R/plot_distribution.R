#' Function plots crossed effects
#'
#' Function plots densities for given varialbe divided into groups (f1) plotted on different panels (strat)
#'
#' @param data data.frame with data
#' @param var variable of interest
#' @param f1 first effect (used as fill)
#' @param strat stratification (Animal)
#' @param box how to present distribution? boxplot or kernel density?
#'
#' @import ggplot2
#'
#' @export

plot_distribution <- function(data, var, f1="Group", strat = "Animal", box = FALSE) {
  if (box) {
    data$ng <- factor(paste(data[,f1], data[,strat]))
    data$ng <- reorder(data$ng, data[,var], median, na.rm=TRUE)

    ggplot(data, aes_string(x="ng", y=var, fill=f1)) +
      geom_boxplot() + coord_flip() +
      theme(legend.position="top") +
      facet_wrap(as.formula(paste0("~", strat)))  + xlab("")
  } else {
    quant <- quantile(data[,var], c(0.001, 0.999))

    ggplot(data, aes_string(fill=f1, x=var)) +
      geom_density(adjust=1, alpha=0.5) +
      coord_cartesian(xlim=quant) +
      facet_wrap(as.formula(paste0("~", strat))) +
      theme(legend.position="top")
  }
}
