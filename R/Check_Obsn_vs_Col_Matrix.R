#' Observations Vs Columns Incidence Matrix
#'
#' @param Matrix Any matrix
#'
#' @return Generates observations vs columns incidence matrix of a given design
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#'mat1<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
#'mat1
#'Check_Obsn_vs_Col_Matrix(mat1)
Check_Obsn_vs_Col_Matrix<-function(Matrix){
  ysd=as.matrix(Matrix)
  ####################obsn vs col
  final=matrix(,nrow=0,ncol=ncol(ysd))
  for(j in 1:nrow(ysd)){
    entrymat=matrix(0,nrow=ncol(ysd),ncol=ncol(ysd))
    for(k in 1:ncol(ysd)){
      entrymat[k,k]=1
    }
    final=rbind(final,entrymat)
  }
  D2_mat_prime=final
  list=list("Observations Vs Columns Incidence Matrix"= D2_mat_prime)
  return(list)
}
