#' Complete/Incomplete Sudoku Designs for Even Number, v
#'
#' @param v Please enter an number, v
#' @param type Please choose type as "complete" or "incomplete". Default is "complete".
#'@description
#'To obtain complete as well as incomplete Sudoku designs for an even number one can use this function. The generated designs are a new series of Sudoku designs.
#'
#' @return For a given v, this function will provide the Sudoku design and its parameters, Region matrix, C matrix, eigenvalues (EVs) and canonical efficiency factor (CEF).
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' Get_Sudoku_I(10)
Get_Sudoku_I<-function(v,type="complete"){
  if(v%%2!=0 || v<6){
    return(message("Please enter an even number v (>=6)."))
  }
  initial_vector<-c(1,v:(2))
  final_matrix<-matrix(NA,v,v)
  for(i in 1:length(initial_vector)){
    if(initial_vector[i]%%2==0){
      vec1<-NULL
      for(j in 1:(v)){
        vec1<-c(vec1,(initial_vector[i]-j+1))
      }
      final_matrix[,i]<-vec1
    }
    if(initial_vector[i]%%2!=0){
      vec2<-NULL
      for(k in 1:(v)){
        vec2<-c(vec2,(initial_vector[i]+k-1))
      }
      final_matrix[,i]<-vec2
    }
  }
  final_matrix<-final_matrix%%v
  final_matrix[final_matrix==0]<-v
  ##region formation
  if(type=="complete"){
    base<-matrix(rep(1:v,each=2),nrow=v/2,ncol=2*v,byrow=TRUE)
    region_mat<-rbind(base[,1:(v)],base[,(v+1):(2*v)])
  }
  if(type=="incomplete"){
    region_mat<-base[,1:(v)]
  }
  ############

  if(type=="incomplete"){
    final_matrix<-final_matrix[1:(v/2),]
    result<-Check_Sudoku_Design(final_matrix,region_mat)
    list<-list("Soduku Design"=final_matrix,"Region Matrix"=region_mat,"Rows"=nrow(final_matrix),"Columns"=ncol(final_matrix),"Regions"=v/2,"C Matrix"=result$"C matrix","EVs"=result$EVs,"Canonical Efficiency Factor"=result$"Canonical Efficiency Factor")
  }
  if(type=="complete"){
    result<-Check_Sudoku_Design(final_matrix,region_mat)
    list<-list("Soduku Design"=final_matrix,"Region Matrix"=region_mat,"Rows"=nrow(final_matrix),"Columns"=ncol(final_matrix),"Regions"=v,"C Matrix"=result$"C matrix","EVs"=result$EVs,"Canonical Efficiency Factor"=result$"Canonical Efficiency Factor")
    }
  return(list)
}

