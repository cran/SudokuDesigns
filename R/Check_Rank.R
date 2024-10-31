
#' Checking Rank of a Matrix
#'
#' @param matrix Any matrix
#'
#' @return Print the rank of the given matrix
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' mat<-matrix(c(1,2,3,2,4,6,5,2,3),nrow=3,byrow=TRUE)
#' Check_Rank(mat)
Check_Rank<-function(matrix){
  N<-as.matrix(matrix)
  NtN<-t(N)%*%N
  NNt<-N%*%t(N)
  eig_val<-eigen(NtN)$values
 rank<-length(eig_val[eig_val>10^-7])
  #########
  list<-list("Rank of the Matrix"=rank)
  return(list)
}

