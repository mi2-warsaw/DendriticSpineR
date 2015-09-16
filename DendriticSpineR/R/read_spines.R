#' Proper loading data
#'
#' Function \code{read_spines} loads data from the indicated file.
#'
#' @details
#' Function \code{read_spines} loads data from the indicated file.
#' The result of this function is a data frame with filtred data
#' including chosen by user columns and two new columns - 'group'
#' and 'condition'. In 'group' column is a group of the animal
#' and in 'condition' is substance which was used in the study.
#' If there was only group (pattern like xx, not like xxxx or
#' xx xx) in group_col_name column, a 'condition' is setted on 'x',
#' so remember to update data file before using this function.
#' Also remember to do not use 'group' as a name for
#' group_col_name column, because 'group' column
#' will be added by \code{read_spines} function and it
#' will be used by plotting functions.
#'
#' @usage read_spines(file, sep = ";", header = TRUE, sheet = 1,
#'   animal_col_name, group_col_name,
#'   photo_col_name = c("Photo_ID_abs", "Photo_ID_rel"),
#'   spines_col_name, properties_col_name = c("length"))
#'
#' @param file a file's path that will be loaded
#' @param sep a field separator character; default: ";"
#' @param header a logical value indicating whether the file
#' contains the names of the variables as its first line;
#' default: TRUE
#' @param sheet a number indicating which sheet from file is needed;
#' default: 1
#' @param animal_col_name a name of column with animal
#' @param group_col_name a name of column with group
#' @param photo_col_name a name/names of column/s with photos;
#' default: c("Photo_ID_abs", "Photo_ID_rel")
#' @param spines_col_name a name of column with spines' numbers
#' @param properties_col_name a name/names of column/s with properties of
#' dendritic spine which will be analysed; default c("length")
#'
#' @return a data.frame of spines class
#'
#' @importFrom stringi stri_extract_all_regex
#' @importFrom openxlsx read.xlsx
#'
#' @export

read_spines <- function(file, animal_col_name, group_col_name,
                       photo_col_name = c("Photo_ID_abs", "Photo_ID_rel"), spines_col_name,
                       properties_col_name = c("length"), ...){
  stopifnot(is.character(file), 
            is.character(animal_col_name), length(animal_col_name) == 1, is.character(group_col_name),
            length(group_col_name) == 1, is.character(photo_col_name), length(photo_col_name) > 0,
            is.character(spines_col_name), length(spines_col_name) == 1, is.character(properties_col_name),
            length(properties_col_name) > 0)

  #checking group_col_name to wrong pattern
  if(group_col_name == "group"){
    stop("group_col_name is 'group'! Please change this column's name in the file with data!
         Column 'group' will be added by read_spines function and it will be used by plotting functions!")
  }

  #checking type of file
  file_type <- unlist(stri_extract_all_regex(file,"(?<=\\.).{3,4}$"))

  #reading file
  if(file_type == "xlsx"){
    spines <- read.xlsx(file, ...)
  } else if(file_type == "csv") {
    spines <- read.table(file, ...)
  } else {
    stop("Wrong type of file (*.csv, *.xlsx)!")
  }

  #checking columns' names
  col_names <- colnames(spines)
  if(!(animal_col_name %in% col_names)){
    stop(paste0("There is not any column in spines data called ", animal_col_name, "."))
  } else if(!(group_col_name %in% col_names)){
    stop(paste0("There is not any column in spines data called ", group_col_name, "."))
  } else if(!all(photo_col_name %in% col_names)){
    if(length(photo_col_name) > 1){
      photo_col <- paste0(photo_col_name, collapse = " or ")
    }
    stop(paste0("There is not any column in spines data called ", photo_col, "."))
  } else if(!all(spines_col_name %in% col_names)){
    stop(paste0("There is not any column in spines data called ", spines_col_name, "."))
  } else if(!all(properties_col_name %in% col_names)){
    if(length(properties_col_name) > 1){
      properties_col <- paste0(properties_col_name, collapse = " or ")
    }
    stop(paste0("There is not any column in spines data called ", properties_col, "."))
  }

  #spliting Group column into two columns with group and condition information
  groups <- stri_extract_all_regex(spines[, group_col_name], "\\p{L}{2}")
  
  groups <- strsplit(as.character(spines[, group_col_name]), split="[^A-Za-z0-9]")
  length_groups <- length(groups)
  flag_warning <- FALSE
  for(i in seq_len(length_groups)){
    if(length(groups[[i]]) == 1){
      groups[[i]][2] <- "x"
      flag_warning <- TRUE
    }
  }

  #information if there were some records without condition
  if(flag_warning){
    warning(paste0("Some records in '", group_col_name, "' column were without condition",
                   " (not uniform pattern)!\n",
                   "  These conditions were set at 'x' during adding columns with group and condition."))
  }

  #adding columns with group and condition
  groups <- unlist(groups)
  group <- groups[seq(1, 2*length_groups, 2)]
  condition <- groups[seq(2, 2*length_groups, 2)]
  spines <- cbind(spines, group = group, condition = condition)

  #choosing only proper columns
  col_names <- colnames(spines)
  col_nr_animal <- which(col_names == animal_col_name)
  col_nr_Group <- which(col_names == group_col_name)
  col_nr_group <- which(col_names == "group")
  col_nr_condition <- which(col_names == "condition")
  length_photo_col <- length(photo_col_name)
  col_nr_photo <- numeric(length_photo_col)
  for(i in seq_len(length_photo_col)){
    col_nr_photo[i] <- which(col_names == photo_col_name[i])
  }
  col_nr_spines <- which(col_names == spines_col_name)
  length_properties_col <- length(properties_col_name)
  col_nr_properties <- numeric(length_properties_col)
  for(i in seq_len(length(properties_col_name))){
    col_nr_properties[i] <- which(col_names == properties_col_name[i])
  }
  spines <- spines[, c(col_nr_animal, col_nr_Group, col_nr_group, col_nr_condition,
                       col_nr_photo, col_nr_spines, col_nr_properties)]

  #setting class for S3 methods
  class(spines) <- c("spines", "data.frame")

  return(spines)
}
