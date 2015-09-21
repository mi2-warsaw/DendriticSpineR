#' A render of raport
#'
#' Function \code{render_raport} renders
#' a raport.
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
