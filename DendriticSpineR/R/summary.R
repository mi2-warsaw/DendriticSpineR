#' A summary of loaded data
#'
#' Function \code{summary} summarises loaded data
#' by \code{read_spines} function.
#'
#' @usage summary(spines)
#'
#' @param spines a data.frame
#'
#' @return invisible NULL
#'
#' @export

summary.spines <- function(spines){
  stopifnot(is(spines, "spines"), is.data.frame(spines))
  col_names <- colnames(spines)
  cat("Summary of loaded data:\n")

  #information about Animal
  cat(paste0("\n1. column - ", col_names[1], ":\n\n"))
  column <- spines[, 1]
  info <- c(range(column),length(unique(column)))
  names(info) <- c("Min.", "Max.", "Unique")
  print(info)

  #rest of summary
  for(i in seq_len(length(col_names))){
    column <- spines[, i]
#     if(is.integer(column)){
#       info <- c(range(column),length(unique(column)))
#       names(info) <- c("Min.", "Max.", "Unique")
#       print(info)
#     } else

    if(is.numeric(column) && !all(column%%1 == 0)){
      cat(paste0("\n", i, ". column - ", col_names[i], ":\n\n"))
      info <- summary(column)
      print(info)
    } else if(is.factor(column) || is.character(column)) {
      cat(paste0("\n", i, ". column - ", col_names[i], ":\n\n"))
      cat("Appearance of elements:\n")
      info <- summary.factor(column)
      if(any(names(info) == "x")){
        warning(paste0("Some records in '", col_names[2], "' column were without condition!\n",
                       "  These conditions were set at 'x' during reading data file."))
      }
      print(info)
    }
  }

  return(invisible(NULL))
}
