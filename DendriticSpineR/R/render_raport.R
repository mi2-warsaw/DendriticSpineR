#' A render of raport
#'
#' Function \code{render_raport} renders
#' a raport.
#'
#' @details Function \code{render_raport} renders
#' a raport in indicated folder. To generate raport the *.Rmd
#' file with generation of raport is needed. The scheme
#' of this file is: "spines-raport-YYYY-MM-DD.Rmd", where:
#' \itemize{
#' \item "YYYY" is year,
#' \item "MM" is month,
#' \item "DD" is day.}
#' It is possible to change raport's title or add author to the raport.
#' To do that, edit the raport file by hand. The space with these
#' informations is at the top of the file. When the raport file is ready,
#' use \code{render_raport} function to render it to the *.html file.
#'
#' @usage render_raport(file_path, destination_path)
#'
#' @param file_path a path to the file with generation of raport
#' @param destination_path a path to the file with raport
#'
#' @return invisible NULL
#'
#' @importFrom rmarkdown render
#'
#' @export

render_raport <- function(file_path, destination_path){
  stopifnot(is.character(file_path), is.character(destination_path))

  if(!file.exists(file_path)){
    stop("Chosen file does not exists. Create it before evaluation of function!")
  }

  render(file_path, output_file = destination_path)

  return(invisible(NULL))
}
