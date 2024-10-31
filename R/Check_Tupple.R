#' Find tupple occurances in a given matrix rows
#'
#' @param matrix Any matrix
#' @param tupple A vector of numbers
#'
#' @return Number of times a tupple occurs within the rows of a given matrix
#' @export
#'
#' @examples
#'mat1<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
#'mat1
#'Check_Tupple(mat1,c(1,2))
Check_Tupple<-function(matrix,tupple){
  mat=matrix
  k<-c()
  a<-tupple
  for(i in 1:nrow(mat)){
    c<-c(mat[i,])
    b<-setdiff(c,a)
    if((length(c)-length(b)-length(a))==0){
      k<-c(k,1)
    }
  }
  return(sum(k))
}
