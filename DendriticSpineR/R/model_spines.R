#' lsmeans calculation
#'
#' Function \code{model_spines} calculates lsmeans function
#' on mixed model of spines data.
#'
#' @usage model_spines(spines, property = "length", trans = NA,
#'    photo_col_name = colnames(spines)[5])
#'
#' @param spines a data.frame of spines class
#' @param property a character with property variable; default: "length"
#' @param trans a transformation of the property variable before ANOVA; default: NA
#' @param inv an inverse transformation of the trans(property); default: NA
#' @param strat a stratification (Animal); default: paste0(animal_col_name, ":group")
#' @param photo_col_name a name of column with photos; default: colnames(spines)[5]
#'
#' @return a list from lsmeans function
#'
#' @import lme4
#' @import lsmeans
#' @import pbkrtest
#'
#' @export

model_spines <- function(spines, property = "length", trans = NA,
                         strat = paste0(colnames(spines)[1], ":group"), photo_col_name = colnames(spines)[5]){
  UseMethod("model_spines")
}

#' @export

model_spines.spines <- function(spines, property = "length", trans = NA,
                                strat = paste0(colnames(spines)[1], ":group"), photo_col_name = colnames(spines)[5]){
  stopifnot(is.data.frame(spines), is.character(property), length(property) == 1,
            (suppressWarnings(is.na(trans)) || is.function(trans)), is.character(photo_col_name))

  col_names <- colnames(spines)
  if(!(property %in% col_names)){
    stop(paste0("There is not any column in spines data called ", property, "."))
  }

  #transformations
  if(!is.function(trans)){
    transfs <- transformations(property, TRUE)
    trans <- transfs[[1]]
  }

  if(!(photo_col_name %in% col_names)){
    stop(paste0("There is not any column in spines data called ", photo_col_name, "."))
  }

  data <- spines
  colnames(data)[2] <- "Group"
  formula_model <- as.formula(paste0(trans, "(", property, ") ~ ", col_names[2], " + (1|",
                                     strat, ") + (1|", photo_col_name,
                                     ":", strat, ")"))
  mixed_model <- lmer(formula_model, data=data)
  ms <- lsmeans(mixed_model, pairwise~Group, adjust="tukey")

  return(ms)
}
