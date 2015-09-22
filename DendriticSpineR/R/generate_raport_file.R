#' A generation of raport file
#'
#' Function \code{generate_raport_file} generates
#' a raport file.
#'
#' @usage generate_raport_file(folder_path, file_path, spines, x_lim = c(0, 2))
#'
#' @param folder_path a path to the folder where file will be made
#' @param file_path a path to the file with spines data
#' @param animal_col_name a name of column with animal
#' @param group_col_name a name of column with group
#' @param photo_col_name a name of column with photos; default: Photo_ID_rel
#' @param spines_col_name a name of column with spines' numbers
#' @param properties_col_name a name/names of column/s with properties of
#' dendritic spine which will be analysed; default "length"
#' @param x_lim a vector with limits of x axis; default: c(0, 2)
#' @param ... other arguments like sep, header, sheet, etc.
#'
#' @return invisible NULL
#'
#' @export

generate_raport_file <- function(folder_path, file_path, animal_col_name, group_col_name,
                                 photo_col_name = "Photo_ID_rel", spines_col_name,
                                 properties_col_name = "length", x_lim = c(0, 2), ...){
  stopifnot(is.character(folder_path), is.character(file_path), is.character(animal_col_name),
            length(animal_col_name) == 1, is.character(group_col_name),
            length(group_col_name) == 1, is.character(photo_col_name),
            length(photo_col_name) == 1, is.character(spines_col_name),
            length(spines_col_name) == 1, is.character(properties_col_name),
            length(properties_col_name) > 0, is.numeric(x_lim), length(x_lim) == 2)

  #checking if file and folder exists
  if(!file.exists(folder_path)){
    stop("Chosen folder does not exists. Create it before evaluation of function!")
  }

  if(!file.exists(file_path)){
    stop("Chosen file does not exists!")
  }

  #making header and library lines
  header <- c("---\ntitle: Spines\nauthor: ", paste("date:", Sys.Date()),
              "output:\n  html_document:\n    toc: true\n---\n")

  if(length(properties_col_name) > 1){
    properties_string <- paste0("\"", properties_col_name, "\"", collapse = ", ")
    properties_string <- paste0("c(", properties_string, ")")
  } else{
    properties_string <- paste0("\"", properties_col_name, "\"")
  }

  dots <- c(...)
  dots_length <- length(dots)
  if(dots_length > 0){
    for(i in seq(dots_length)){
      if(dots[i] == TRUE | dots[i] == FALSE){
        dots[i] <- dots[i]
      } else if(is.character(dots[i])){
        dots[i] <- paste0("\"", dots[i], "\"")
      }
    }
    dots <- paste0(names(dots), sep = " = ", dots, collapse = ", ")
    dots <- paste0(",\n                      ", dots)

    lib_spines <- c("```{r, message=FALSE, warning=FALSE}\nlibrary(DendriticSpineR)\n",
                    paste0("spines <- read_spines(file = \"", file_path, "\",\n",
                           "                      animal_col_name = \"", animal_col_name, "\",\n",
                           "                      group_col_name = \"", group_col_name, "\",\n",
                           "                      photo_col_name = \"", photo_col_name, "\",\n",
                           "                      spines_col_name = \"", spines_col_name, "\",\n",
                           "                      properties_col_name = ", properties_string, dots, ")"), "```\n")
  } else{
    lib_spines <- c("```{r, message=FALSE, warning=FALSE}\nlibrary(DendriticSpineR)\n",
                    paste0("spines <- read_spines(file = \"", file_path, "\",\n",
                           "                       animal_col_name = \"", animal_col_name, "\",\n",
                           "                       group_col_name = \"", group_col_name, "\",\n",
                           "                       photo_col_name = \"", photo_col_name, "\",\n",
                           "                       spines_col_name = \"", spines_col_name, "\",\n",
                           "                       properties_col_name = ", properties_string, ")"), "```\n")
  }

  name <- paste0(folder_path, "/raport-", Sys.Date(), ".Rmd")

  con <- file(name,"w")

  #adding header
  for(i in seq_len(length(header))){
    writeLines(header[i], con)
  }

  #adding library
  writeLines(lib_spines, con)

  #adding analisys
  spines <- read_spines(file_path, animal_col_name = animal_col_name,
                        group_col_name = group_col_name,
                        spines_col_name = spines_col_name,
                        photo_col_name = photo_col_name,
                        properties_col_name = properties_col_name, ...)
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
    element_6 <- paste0("(ms <- model_spines(spines, photo_col_name = \"",
                        col_names[nr_col_analysis[1] - 2], "\"))")
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
