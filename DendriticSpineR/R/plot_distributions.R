#' Distribution of spines
#'
#' Function \code{plot_distributions} plots a distribution (both density function
#' and cumulative density function) for dendritic spines grouped by one
#' or more grouping variables.
#'
#' @usage plot_distributions(spines, property = "length", ecdf = TRUE, x_lim = c(0, 2))
#'
#' @param spines a data.frame of spines class
#' @param property a character with property variable; default: "length"
#' @param ecdf if TRUE then cumulative density function is plotted,
#' if FALSE then density function is plotted; default: TRUE
#' @param x_lim a vector with limits of x axis; default: c(0, 2)
#'
#' @return a (cumulative) density function plot
#'
#' @import scales
#' @import tidyr
#'
#' @export

plot_distributions <- function(spines, property = "length", ecdf = TRUE, x_lim = c(0, 2)){
  UseMethod("plot_distributions")
}

#' @export

plot_distributions.spines <- function(spines, property = "length", ecdf = TRUE, x_lim = c(0, 2)){
  stopifnot(is.data.frame(spines), is.character(property), length(property) == 1, is.logical(ecdf),
            is.numeric(x_lim), length(x_lim) == 2)

  col_names <- colnames(spines)
  if(!(property %in% col_names)){
    stop(paste0("There is not any column in spines data called ", property, "."))
  }

  col_nr_property <- which(col_names == property)
  group <- spines[, 2]
  spiness <- spines[, col_nr_property]
  df <- data.frame(spiness, group)
  if (ecdf) {
    pl <- ggplot(df, aes(x=spiness, fill=group, color=group)) +
      stat_ecdf(size=2) + theme_bw()  +
      scale_y_continuous(labels = percent) +
      ylab("")
  } else {
    pl <- ggplot(df, aes(x=spiness, fill=group, color=group)) +
      geom_density(alpha=0.5) + theme_bw()
  }

  pl <- pl + coord_cartesian(xlim=x_lim) + xlab(property)
  return(pl)
}
