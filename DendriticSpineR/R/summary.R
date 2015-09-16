#' A summary of loaded data
#'
#' Function \code{summary} summarises loaded data
#' by \code{read_spines} function.
#'
#' @usage summary(spines)
#'
#' @param spines a data.frame of spines class
#'
#' @return invisible NULL
#'
#' @export

summary <- function(spines){
  UseMethod("summary")
}

#' @export

summary.spines <- function(spines){
  stopifnot(is(spines, "spines"), is.data.frame(spines))
  col_names <- colnames(spines)
  cat("Summary of loaded data:\n")

  #information about Animal
  cat(paste0("\n1. column - ", col_names[1], ":\n\n"))
  column <- spines[, 1]
  info <- c(length(unique(column)),table(column))
  names_info <- paste0(col_names[1], " ", names(info), ":")
  names_info[1] <- "Unique:"
  info <- matrix(info, ncol = 1, dimnames = list(names_info, "Appearance"))
  print(info)

  #information about Group
  cat(paste0("\n2. column - ", col_names[2], ":\n\n"))
  column <- spines[, 2]
  cat("Appearance of elements:\n")
  info <- summary.factor(column)
  print(info)

  #information about group and condition
  cat(paste0("\n3-4. columns - ", col_names[3], ", ", col_names[4], ":\n\n"))
  column <- spines[, 3:4]
  cat("Appearance of elements:\n")
  info <- table(column)
  if(any(colnames(info) == "x")){
    warning(paste0("Some records in '", col_names[2], "' column were without condition",
                   "(not uniform pattern)!\n",
                   "  These conditions were set at 'x' during reading data file."))
  }
  print(info)

  #information about properties
  n <- ncol(spines)
  j <- 1
  properties_nr_columns <- numeric(n)
  for(i in seq_len(n)){
    column <- spines[, i]
    if(is.numeric(column) && !all(column%%1 == 0)){
      properties_nr_columns[j] <- i
      j <- j + 1
    }
  }
  properties_columns <- properties_nr_columns[which(properties_nr_columns > 0)]
  column <- spines[, properties_columns]
  if(length(properties_columns) == 1){
    cat(paste0("\n", properties_columns, ". column - ", col_names[properties_columns],":\n\n"))
    info <- summary(column)
  } else {
    properties_col <- paste0(properties_columns[1], sep = "-", tail(properties_columns, 1))
    cat(paste0("\n", properties_col, ". columns - properties:\n\n"))
    info <- summary.data.frame(column)
  }
  print(info)

  return(invisible(NULL))
}
