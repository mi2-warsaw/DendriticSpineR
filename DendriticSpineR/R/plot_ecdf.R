#' Distribution of spines
#'
#' Function \code{plot_ecdf} plots a distribution (both density function
#' and cumulative density function) for dendritic spines grouped by one
#' or more grouping variables.
#'
#' @usage plot_ecdf(spines, properties = "length", ecdf = TRUE, x_lim = c(0, 2))
#'
#' @param spines a data.frame of spines class
#' @param properties a vector with properties variable/s; default: "length"
#' @param ecdf if TRUE then cumulative density function is plotted,
#' if FALSE then density function is plotted; default: TRUE
#' @param x_lim a vector with limits of x axis; default: c(0, 2)
#'
#' @return a (cumulative) density function plot
#'
#' @import scales
#'
#' @export

plot_ecdf <- function(spines, properties = "length", ecdf = TRUE, x_lim = c(0, 2)){
  UseMethod("plot_ecdf")
}

#' @export

plot_ecdf.spines <- function(spines, properties = "length", ecdf = TRUE, x_lim = c(0, 2)){
  stopifnot(is.data.frame(spines), is.character(properties), is.logical(ecdf),
            is.numeric(x_lim), length(x_lim) == 2)

  col_names <- colnames(spines)
  length_properties <- length(properties)
  if(!all(properties %in% col_names)){
    if(length_properties > 1){
      properties_col <- paste0(properties, collapse = " or ")
    }
    stop(paste0("There is not any column in spines data called ", properties_col, "."))
  }

  col_nr_properties <- numeric(length_properties)
  for(i in seq_len(length_properties)){
    col_nr_properties[i] <- which(col_names == properties[i])
  }

  #df <- as.data.frame(spines[, c(col_nr_properties,2)])
  group <- spines[, 2]
  spiness <- unlist(spines[, col_nr_properties], use.names = FALSE)
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

  x_lab <- paste0(properties, collapse = " + ")
  pl <- pl + coord_cartesian(xlim=x_lim) + xlab(x_lab)
  return(pl)
}
