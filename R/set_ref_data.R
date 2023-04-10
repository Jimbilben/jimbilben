#' Set Reference Data
#'
#' Set reference data to be referred to by some other functions such as lunique()
#'
#' @param data The data to be set as the reference data
#' @param name A string - if you want the reference data to be named something other than ref_data, provide a name here

#' @export
#'
set_ref_data <- function(data, name = NULL) {

  if(is.null(name)) {
    ref_data <<- data
  }
  else {
    #name <- as.symbol(name)
    assign(name, data, envir = .GlobalEnv)
  }

}
