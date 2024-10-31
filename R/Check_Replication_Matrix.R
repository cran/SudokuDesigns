#' Replication Matrix
#'
#' @param matrix Any matrix
#'
#' @return A diagonal matrix of replications for each treatment.
#' @export
#'
#' @examples
#'library(SudokuDesigns)
#'mat11<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
#'mat11
#'Check_Replication_Matrix(mat11)
Check_Replication_Matrix<-function(matrix){
#######obsn vs trt
matrix<-as.matrix(matrix)
unq_ele<-unique(c(matrix))
final_mat<-matrix(0,length(unq_ele),length(unq_ele))
for(i in 1:length(unq_ele)){
  final_mat[i,i]<-length(which(matrix==unq_ele[i]))
}
  list=list("Replication Matrix"= final_mat)
  return(list)
}
