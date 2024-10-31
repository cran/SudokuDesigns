
#' Observations Vs Regions Incidence Matrix
#'
#' @param Design A Sudoku design in matrix format
#' @param Region A matrix of regions according to the Sudoku design
#'
#' @return Observations vs regions incidence matrix for a given Sudoku design and region matrix
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' design<-matrix(c(1,2,3,4,3,4,1,2,2,1,4,3,4,3,2,1),nrow=4,ncol=4,byrow=TRUE)
#' region<-matrix(c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4),nrow=4,ncol=4,byrow=TRUE)
#' Check_Obsn_vs_Reg_Matrix(design, region)

Check_Obsn_vs_Reg_Matrix=function(Design,Region){
  ysd=as.matrix(Design)
  region=as.matrix(Region)
  ################ for construction of D3_mat
  D3_mat=matrix(0,nrow=length(ysd),ncol=max(region))
  k=1
  i=1
  while(i<=nrow(ysd)){
    j=1
    while(j<=ncol(ysd)){
      #element1<-ysd[i,j]
      element2<-region[i,j]
      D3_mat[k,element2]<-1
      k=k+1
      j=j+1
    }
    i=i+1
  }
  D3_mat_prime=D3_mat
  list=list("Observations Vs Regions Incidence Matrix"= D3_mat_prime)
  return(list)
}
