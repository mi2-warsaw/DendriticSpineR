#' Distribution of spines
#'
#' This function plots a distribution (both density function and cumulative density function)
#' for dendritic spines grouped by one or more grouping variables.
#'
#' @param spines a vector with spine properties (length, area etc)
#' @param group a vector with grouping variable
#' @param ecdf if TRUE then cumulative density function is plotted, if FALSE then density function is plotted
#'
#' @import scales
#'
#' @export

plot_ecdf <- function(spines, group, ecdf=TRUE) {
  df <- data.frame(spines, group)
  if (ecdf) {
    ggplot(df, aes(x=spines, fill=group, color=group)) +
      stat_ecdf(size=2) + theme_bw()  +
      scale_y_continuous(labels = percent) +
      ylab("")
  } else {
    ggplot(df, aes(x=spines, fill=group, color=group)) +
      geom_density(alpha=0.5) + theme_bw()
  }
}

