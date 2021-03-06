#' Plot of distributions of spines
#'
#' Function \code{plot_animals} plots densities for given variable
#' divided into groups plotted on different panels for different animal
#'
#' @usage plot_animals(spines, property = "length", box = FALSE)
#'
#' @param spines a data.frame of spines class
#' @param property a character with property variable; default: "length"
#' @param box a way of presenting distribution - FALSE for kernel density
#' or TRUE for boxplot; default: FALSE
#'
#' @return a panel plot of distributions
#'
#' @import ggplot2
#'
#' @export

plot_animals <- function(spines, property = "length", box = FALSE){
  UseMethod("plot_animals")
}

#' @export

plot_animals.spines <- function(spines, property = "length", box = FALSE){
  stopifnot(is.data.frame(spines), is.character(property), length(property) == 1,
            is.logical(box))

  data <- spines
  col_names <- colnames(spines)
  if(!(property %in% col_names)){
    stop(paste0("There is not any column in spines data called ", property, "."))
  }

  strat <- col_names[1]
  f1 <- col_names[2]
  if (box) {
    data$ng <- factor(paste(data[, 2], data[, strat]))
    data$ng <- reorder(data$ng, data[, property], median, na.rm=TRUE)

    pl <- ggplot(data, aes_string(x="ng", y=property, fill=f1)) +
      geom_boxplot() + coord_flip() +
      theme(legend.position="top") +
      facet_wrap(as.formula(paste0("~", strat)))  + xlab("")
  } else {
    quant <- quantile(data[, property], c(0.001, 0.999))

    pl <- ggplot(data, aes_string(fill=f1, x=property)) +
      geom_density(adjust=1, alpha=0.5) +
      coord_cartesian(xlim=quant) +
      facet_wrap(as.formula(paste0("~", strat))) +
      theme(legend.position="top")
  }

  return(pl)
}
