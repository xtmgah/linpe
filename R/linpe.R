#' link
#' @param data a data frame object
#' @param file Character vector of length one pointing to a .rmd file location
#' @param linpe Character vector of length one giving the name of the linpe (analysis). When `linpe=NULL` (default value),
#' the name of the linpe is deduced from the file name
#'
#' @return A dataframe with a `linpe` attribute linked to it
#' @export
#'
#' @examples
#' file <- paste(find.package("linpe"), "rmd/test-linpe.rmd", sep = "/")
#' # analysis is saved with default name
#' mtcars_linpe <- link (mtcars, file = file)
#' attr(mtcars_linpe, "test-linpe")
#'
#' # analysis is saved with specific name
#' mtcars_linpe <- link (mtcars, file, linpe = "this-linpe")
#' attr(mtcars_linpe, "this-linpe")
link <- function(data , file, linpe = NULL) {

  if(is.null(linpe)) {
    linpe <- gsub( "\\\\", "/", file)
    linpe <- tail(unlist(strsplit(linpe,  split = "/"))  , 1)
    linpe <- head(unlist(strsplit(linpe,  split = ".", fixed = T)), 1)
  }

  file <- readLines(file)
  class(file) <- "linpe"
  attr(data, linpe) <- file
  data
}
####################################################################
#' unlink
#' @param data a data frame object
#' @param linpe Character vector of length one giving the name of the linpe (analysis)
#'
#' @return A dataframe with a `linpe` attribute removed from it
#' @export
#'
#' @examples
#' file <- paste(find.package("linpe"), "rmd/test-linpe.rmd", sep = "/")
#' mtcars_linpe <- link (mtcars, file = file)
#' linpe(mtcars_linpe)
#' mtcars_linpe <- unlink (mtcars_linpe, linpe = "test-linpe")
#' linpe(mtcars_linpe)
#'
unlink <- function(data , linpe) {
  attr(data, linpe) <- NULL
  data
}
#####################################################################
#' linpe
#'
#' @param data a data frame object
#'
#' @return list of linpes associated to data
#' @export
#'
#' @examples
#' data(mtcars_linpe)
#' linpe(mtcars_linpe)
#' linpe(mtcars)
linpe <- function(data){

  data_name <- deparse(substitute(data))
  att <- lapply(attributes(data), class)
  att <- names(att[att %in% "linpe"])

  if (length(att) == 0) {
    cat(paste("No limpe in", data_name, "\n"))
  }
  return(att)
}
#####################################################################
#' perform
#'
#' @param data a data framae object
#' @param linpe the name of an `linpe` attribute previously saved with `link()`
#' @param ... any parameter to be passed to function rmarkdown::render
#'
#' @return invisible(NULL) and, as side effect, renders the `.rmd` file corrensponding to the analysis and display it in the default html viewer
#' @export
#'
#' @examples
#' data(mtcars_linpe)
#' perform(mtcars_linpe, "test-linpe")
perform <- function(data, linpe, ...){
  file_rmd <- paste(linpe, "rmd", sep = ".")
  file_html <- paste(linpe, "html", sep = ".")
  rmd <- attr(data, linpe)
  writeLines(rmd , con = file_rmd)
  rmarkdown::render(file_rmd, ...)
  viewer <- getOption("viewer")
  viewer(file_html)
  invisible(NULL)
}
#####################################################################
#' display
#'
#' @param data a data frame object
#' @param linpe the name of the linpe attribute previously saved with `link()`
#'
#' @return invisible(NULL) and, as side effect, opens the `.rmd` linked to the linpe
#' @export
#'
#' @examples
#' data(mtcars_linpe)
#' display(mtcars_linpe, linpe = "test-linpe")
display <- function(data, linpe){
  file_rmd <- paste(linpe, "rmd", sep = ".")
  rmd <- attr(data, linpe)
  writeLines(rmd , con = file_rmd)
  file.edit(file_rmd)
  invisible(NULL)
}





