
#' Observations Vs Rows Incidence Matrix
#'
#' @param Matrix Any matrix
#'
#' @return Generates observations vs rows matrix for a given design
#' @export
#'
#' @examples
#'library(SudokuDesigns)
#'mat1<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
#'mat1
#'Check_Obsn_vs_Rows_Matrix(mat1)
Check_Obsn_vs_Rows_Matrix<-function(Matrix){
  ysd=as.matrix(Matrix)
  #################obsn vs row
  D1_mat_prime=matrix(0,nrow=length(ysd),ncol=nrow(ysd))
  k=1
  for(j in 1:nrow(ysd)){
    D1_mat_prime[(k):(k-1+ncol(ysd)),j]=1
    k=k+ncol(ysd)
  }
  list=list("Observations Vs Rows Incidence Matrix"= D1_mat_prime)
  return(list)
}
