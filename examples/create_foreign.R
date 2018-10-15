#' Create Foreign Variable
#' 
#' @description This function takes an tibble input of the \code{mpg} data and creates 
#'     a new \code{foreign} variable.
#'     
#' @param .data A tibble or data frame
#' 
#' @return A new tibble with the added \code{foreign} variable.
#' 
create_foreign <- function(.data){
  
  # create new variable
  out <- dplyr::mutate(.data, foreign = ifelse(manufacturer == "audi" | 
                                                 manufacturer == "honda" |
                                                 manufacturer == "hyundai" |
                                                 manufacturer == "land rover" |
                                                 manufacturer == "nissan" |
                                                 manufacturer == "subaru" |
                                                 manufacturer == "toyota" |
                                                 manufacturer == "volkswagen",
                                               TRUE, FALSE))
  
  # return output
  return(out)
  
}
