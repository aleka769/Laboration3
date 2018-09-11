#' @title The euclidean function
#' @description Some math stuff
#' @param a A numeric value
#' @param b Also a numeric value
#' @return The function returns the greatest common diviser of a and b
#' @export euclidean

euclidean <- function(a, b){
  
  if (max(a,b) %% min(a,b) == 0){
    return( min(a,b) )
  } else {
    m <- matrix(1:min(a,b), ncol = 3, 
                nrow = min(a,b), byrow = FALSE,
                dimnames = list(1:min(a,b), 
                                c("i", "a_mod", "b_mod")))
    it <- 1:min(a,b)
    m[,c(2,3)] <- c(a%%it, b%%it)
    m_red <- m[rowSums(m[,c(2,3)]) == 0,]
    return(m_red[nrow(m_red),1])
  }
}