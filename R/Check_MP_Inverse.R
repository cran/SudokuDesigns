#' Moore Penrose Inverse
#'
#' @param matrix Any matrix
#'
#' @return Provides Moore Penrose inverse of a given matrix
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' mat<-matrix(c(1,2,3,2,5,3,2,4,6),nrow=3,byrow=TRUE)
#' Check_MP_Inverse(mat)
Check_MP_Inverse<-function(matrix){
  N<-as.matrix(matrix)
  NtN<-t(N)%*%N
  NNt<-N%*%t(N)
  eig_val<-eigen(NtN)$values
  positive<-NULL
  for(i in eig_val){
    if(i>10^-7){
      positive<-c(positive,TRUE)
    }else{
      positive<-c(positive,FALSE)
    }
  }
  sigma<-1/sqrt(eig_val[eig_val>10^-7])
  u<-eigen(NtN)$vectors[,positive,drop=FALSE]
  v<-eigen(NNt)$vectors[,positive,drop=FALSE]
  return(u%*%diag(sigma,nrow(t(v)))%*%t(v))
}
