#' A diffogram for lsmeans
#'
#' This function plots a diffogram for least square means calculated
#' with the lsmeans package.
#' The diffogram presents results from post hoc testing.
#' More informations about diffograms:
#' ,,Interpreting the Differences Among LSMEANS in Generalized Linear Models''
#' http://www.mwsug.org/proceedings/2011/dataviz/MWSUG-2011-DG08.pdf
#'
#' @param lsmodel an object from lsmeans function
#' 
#' @param logt are the reposnce after log transformation?
#'
#' @import lsmeans
#'
#' @examples \dontrun{
#' model <- lmer(log(length) ~ group + (1|Animal) , data=data)
#' lsmodel <- lsmeans(model,  pairwise~group, adjust="tukey")
#' diffogram(lsmodel)
#' }
#'
#' @export

diffogram <- function(lsmodel, logt=FALSE) {
  # effects
  tmp1 <- confint(lsmodel$lsmeans)
  effects <- data.frame(labels = tmp1[,attr(tmp1, "pri.vars")],
                   values = tmp1[,attr(tmp1, "estName")])
  rownames(effects) <- effects$labels

  # confidence intervals
  tmp2 <- confint(lsmodel$contrasts)
  ci <- data.frame(labels = tmp2[,attr(tmp2, "pri.vars")],
                    values = tmp2[,attr(tmp2, "estName")],
                    ciL = tmp2[,attr(tmp2, "clNames")[1]],
                    ciR = tmp2[,attr(tmp2, "clNames")[2]])

  # indexes
  ll <- strsplit(as.character(ci$labels), split=" - ")
  ci_names <- data.frame(
    ci_left_name  = sapply(ll, `[`, 1),
    ci_right_name = sapply(ll, `[`, 2),
    stringsAsFactors = FALSE
  )

  to_plot <- data.frame(name_x = ci_names$ci_left_name,
                        name_y = ci_names$ci_right_name,
                        wsp_x = effects[ci_names$ci_left_name,"values"],
                        wsp_y = effects[ci_names$ci_right_name,"values"],
                        wsp_x_y = ci[,"values"],
                        wsp_x_y_ci_left = ci[,"ciL"] - ci[,"values"],
                        wsp_x_y_ci_right = ci[,"ciR"] - ci[,"values"],
                        significant = ifelse(ci[,"ciL"] * ci[,"ciR"] > 0,
                                             "significant", "non-significant"))
  to_plot$seg_x <- to_plot$wsp_y + to_plot$wsp_x_y - to_plot$wsp_x_y_ci_left/2
  to_plot$seg_x_end <- to_plot$wsp_y + to_plot$wsp_x_y - to_plot$wsp_x_y_ci_right/2
  to_plot$seg_y <- to_plot$wsp_y + to_plot$wsp_x_y_ci_left/2
  to_plot$seg_y_end <- to_plot$wsp_y + to_plot$wsp_x_y_ci_right/2
  
  # ranges
  spec <- range(effects$values) + max(c(abs(to_plot$wsp_x_y_ci_left), abs(to_plot$wsp_x_y_ci_right))) * c(-0.5,0.5)
  effects$spec1 <- spec[1]
  effects$spec2 <- spec[2]
  
  if (logt) {
    effects[,2:ncol(effects)] <- exp(effects[,2:ncol(effects)]) 
    to_plot[,3:7] <- exp(to_plot[,3:7]) 
    to_plot[,9:12] <- exp(to_plot[,9:12]) 
    spec <- exp(spec)
  }

  # the plot
  pl <- ggplot(to_plot, aes(x=wsp_x, y=wsp_y)) +
    geom_hline(data=effects, aes(yintercept=values), lty=3, color="grey") +
    geom_text(data=effects, aes(x=spec1, y=values, label=labels), hjust=0, vjust=-0.3, size=4) +
    geom_vline(data=effects, aes(xintercept=values), lty=3, color="grey") +
    geom_text(data=effects, aes(y=spec2, x=values, label=labels), hjust=1, vjust=-0.3, size=4, angle=90) +
    geom_point(size=2) +
    geom_segment(data=to_plot, aes(x=seg_x, xend=seg_x_end,
                     y=seg_y, yend=seg_y_end,
                     color=significant,
                     lty=significant)) +
    geom_abline(intercept=0, slope=1) +
    xlim(spec) + ylim(spec) + xlab("") + ylab("") +
    scale_color_manual(values=c("navyblue", "red4")) +
    scale_linetype_manual(values=c(2,1)) +
    theme(panel.background	= element_rect(fill = "white"),
          legend.position="bottom")
  if (logt) {
    pl <- pl + scale_x_log10(limits=spec) + scale_y_log10(limits=spec)
  }
  
  pl
}






