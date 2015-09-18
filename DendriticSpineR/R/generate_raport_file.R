#' A generation of raport file
#'
#' Function \code{generate_raport_file} generates
#' a raport file.
#'
#' @usage generate_raport_file(folder_path, file_path, spines, x_lim = c(0, 2))
#'
#' @param folder_path a path to the folder where file will be made
#' @param file_path a path to the file with spines data
#' @param spines a data.frame of spines class
#' @param x_lim a vector with limits of x axis; default: c(0, 2)
#'
#' @return invisible NULL
#'
#' @export

generate_raport_file <- function(folder_path, file_path, spines, x_lim = c(0, 2)){
  stopifnot(is.character(folder_path), is(spines, "spines"), is.data.frame(spines))

  if(!file.exists(folder_path)){
    stop("Chosen folder does not exists. Create it before evaluation of function!")
  }

  header <- c("---\ntitle: Spines\nauthor: ", paste("date:", Sys.Date()),
              "output:\n  html_document:\n    toc: true\n---\n")

  lib <- c("```{r, message=FALSE, warning=FALSE}\nlibrary(DendriticSpineR)\n",
           paste("spines <- read_spines(\"zebrane.csv\", \"Animal\",",
                      "\"Group\", \"spine_number\", \"Photo_ID_rel\",",
                      "c(\"length\",\"foot\",\"head_width\"),",
                      "header = TRUE, sep = \";\")"), "```\n")

  name <- paste0(folder_path, "/raport-", Sys.Date(), ".Rmd")

  con <- file(name,"w")

  #adding header
  for(i in seq_len(length(header))){
    writeLines(header[i],con)
  }

  #adding library
  writeLines(lib,con)

  #adding analisys
  col_names <- colnames(spines)
  length_names <- length(col_names)
  col_analysis <- numeric(length_names)
  for(i in seq_len(length_names)){
    if(is.numeric(spines[, i]) && !all(spines[, i]%%1 == 0)){
      col_analysis[i] <- 1
    }
  }
  nr_col_analysis <- seq_len(length_names)[which(col_analysis == 1)]
  col_analysis <- col_names[which(col_analysis == 1)]
  length_analysis <- length(col_analysis)

  for(i in seq_len(length_analysis)){
    chapter <- paste("#", col_analysis[i], "\n")
    writeLines(chapter, con)
    element_1 <- paste0("plot_distributions(spines, property = \"", col_analysis[i], "\", ecdf = TRUE, ",
                        "x_lim = c(", x_lim[1], ", ", x_lim[2], "))")
    element_2 <- paste0("plot_distributions(spines, property = \"", col_analysis[i], "\", ecdf = FALSE, ",
                        "x_lim = c(", x_lim[1], ", ", x_lim[2], "))\n")
    element_3 <- paste0("plot_animals(spines, property = \"", col_analysis[i], "\", box = FALSE", ")")
    element_4 <- paste0("plot_animals(spines, property = \"", col_analysis[i], "\", box = TRUE", ")\n")
    element_5 <- paste0("plot_crossed_effects(spines, property = \"", col_analysis[i], "\", ",
                        "strat = \"Animal:group\", mixed = TRUE)\n")
    element_6 <- paste0("ms <- model_spines(spines, photo_col_name = \"",
                        col_names[nr_col_analysis[1] - 2], "\")")
    element_7 <- "diffogram(ms)"

    r_sequence <- c("```{r}", element_1, element_2, element_3, element_4, element_5,
                    element_6, element_7,"```\n")
    for(j in seq_len(length(r_sequence))){
      writeLines(r_sequence[j], con)
    }
  }

  close(con)

  return(invisible(NULL))
}
