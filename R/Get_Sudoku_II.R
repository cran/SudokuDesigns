#' Incomplete Sudoku designs for v = nC2 where n (>=5) is an odd number
#'
#' @param v Provide v = nC2 where n (>=5) is an odd number
#'@description
#' Generated designs with less number of regions with quite high canonical efficiency factors.
#'
#' @return It returns an incomplete Gerechte design along with its parameters, region matrix, C matrix, eigenvalues (EVs) and canonical efficiency factor (CEF).
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' Get_Sudoku_II(10)
Get_Sudoku_II<-function(v){
  n=(1+(sqrt(1+(8*v))))/2
  #v=(n*(n-1))/2
  if(n%%2==0 || n<5){
    return(message("Provide v = nC2 where n (>=5) is an odd number."))
  }
  tri_mat=diag(nrow=n,ncol=n)
  tri_mat[tri_mat==1]<-NA
  k=1
  for(i in 1:(nrow(tri_mat)-1)){
    for(j in (i+1):nrow(tri_mat)){
      if(tri_mat[i,j]==0){
        tri_mat[i,j]<-k
        k=k+1
      }
    }
  }
  for(i in 1:ncol(tri_mat)){
    tri_mat[,i]<-tri_mat[i,]
  }
  ###########################Next step
  final1=NULL
  for(i in 1:(nrow(tri_mat)-1)){
    #for(j in (i+1):ncol(tri_mat)){
    vec=c(tri_mat[i,((i+1):ncol(tri_mat))],tri_mat[i,(1:(i))])
    final1<-rbind(final1,vec)
    if(i==ncol(tri_mat)){
      break
    }
  }
  final1=rbind(final1,tri_mat[nrow(tri_mat),])
  final=final1[,-c(ncol(final1))]
  row.names(final)<-NULL
  ######################further step

  nn=(n-1)/2
  #################

  #############
  ###Matrix rotation

  Matrix_Rotation=function(matrix){
    matrix=as.matrix(matrix)
    ############pattern
    pattern<-function(vector){
      final=NULL
      vec=matrix(vector,1,length(vector))
      for(i in 0:(length(vector)-1)){
        a=vec[1,]+i
        final=rbind(final,a)
      }
      final=final%%max(vector)
      final[final==0]<-max(vector)
      if(any(final<min(vector))){
        final[final<min(vector)]<- final[final<min(vector)]+(min(vector)-1)
      }
      return(final)
    }

    #####
    atlast2=NULL
    for(k in 1:(ncol(matrix)/nn)){
      atlast1=NULL
      matrix1=c((nn*k-(nn-1)):(nn*k))
      the_mat=pattern(matrix1)
      for(i in 1:nrow(the_mat)){
        atlast1=rbind(atlast1,matrix[,c(the_mat[i,])])
      }
      atlast2=cbind(atlast2,atlast1)
    }

    return(atlast2)
  }
  #######region mat
  reg_mat<-NULL
  for(i in 1:((n-1)/2)){
    reg1<-matrix(rep(((2*i)-1):(2*i),each=((n-1)/2)),nrow=n,ncol=((n-1)),byrow=TRUE)
    reg_mat<-rbind(reg_mat,reg1)
  }
  ###sudoku
  design=Matrix_Rotation(final)
  result<-Check_Sudoku_Design(design,reg_mat)
  list<-list("Soduku Design"=design,"Region Matrix"=reg_mat,"Rows"=nrow(design),"Columns"=ncol(design),"Regions"=v,"C Matrix"=result$"C matrix","EVs"=result$EVs,"Canonical Efficiency Factor"=result$"Canonical Efficiency Factor")
  return(list)
}

