#' Observations Vs Treatments Incidence Matrix
#'
#' @param Matrix Any matrix
#'
#' @return Generates observations Vs treatments matrix
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#'mat1<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
#'mat1
#'Check_Obsn_vs_Trt_Matrix(mat1)
Check_Obsn_vs_Trt_Matrix<-function(Matrix){
  ysd=as.matrix(Matrix)
  #######obsn vs trt
  delprime=NULL
  for(i in 1:nrow(ysd)){
    for(j in 1:ncol(ysd)){
      if(ysd[i,j]>0){
        create_vec=matrix(0,nrow=1,ncol=max(ysd))
        ele=ysd[i,j]
        create_vec[,ele]<-1
        delprime=rbind(delprime,create_vec)
      }else{
        j=j+1
      }
    }
  }
  list=list("Observations Vs Treatments Incidence Matrix"=delprime)
  return(list)
}
