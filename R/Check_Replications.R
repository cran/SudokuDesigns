#' Replications for each treatments
#'
#' @param matrix Any matrix
#'
#' @return Returns a matrix of replications for each treatment.
#' @export
#'
#' @examples
#'library(SudokuDesigns)
#'mat11<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
#'mat11
#'Check_Replications(mat11)
Check_Replications<-function(matrix){
#######obsn vs trt
matrix<-as.matrix(matrix)
unq_ele<-unique(c(matrix))
final_vec<-NULL
for(i in 1:length(unq_ele)){
  final_vec<-c(final_vec,length(which(matrix==unq_ele[i])))
}
final_mat<-cbind(1:length(unq_ele),final_vec)
colnames(final_mat)<-c("Treatments","Replications")
  return(final_mat)
}
