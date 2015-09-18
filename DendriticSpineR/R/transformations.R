#' A choose of transformations for property
#'
#' Function \code{transformations} chooses
#' a tranformations for chosen property.
#'
#' @details
#' Function \code{transformations} chooses
#' a tranformations for chosen property. The first tranformation
#' is a function for applying the property to the model.
#' The second is a inversion of first function.
#'
#' @usage transformations(property, as_character = FALSE)
#'
#' @param property a character with property variable
#' @param as_character a logical value indicating whether value on return
#' must be a character vector; default: FALSE
#'
#' @return a list with two functions or character vector
#'
#' @export

transformations <- function(property, as_character = FALSE){
  stopifnot(is.character(property), length(property) == 1,
            is.logical(as_character))

  log_exp <- c("length", "head_width", "max_width",
               "width_length_ratio", "length_width_ratio",
               "foot", "circumference", "area",
               "length_area_ratio")

  if(property %in% log_exp){
    if(as_character){
      transf <- c("log", "exp")
    } else {
      transf <- c(log, exp)
    }
  } else if(property == "neck_width"){
    pow <- function(x){
      x^2
    }
    if(as_character){
      transf <- c("sqrt", "pow")
    } else {
      transf <- c(sqrt, pow)
    }
  } else{
    if(as_character){
      transf <- c("I", "I")
    } else {
      transf <- c(I, I)
    }
  }

  return(transf)
}
