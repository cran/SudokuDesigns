#' Check properties of an incomplete row-column design (IRC)
#'
#' @param Design Provide an IRC in matrix format
#'
#' @return Provides C matrix (Information matrix), eigenvalues(EVs) and canonical efficiency factor (CEF) of a given IBD
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' Design<-matrix(c(1,2,3,2,5,3,2,4,6),nrow=3,byrow=TRUE)
#' Check_IRC(Design)
Check_IRC=function(Design){
  ysd=Design
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
  #######
  rep_mat= t(delprime)%*%delprime
  #################obsn vs row
  D1_mat_prime=NULL
  for(i in 1:nrow(ysd)){
    rowsize=length(ysd[i,][ysd[i,]>0])
    zeromatrix=matrix(0,nrow=rowsize,ncol=nrow(ysd))
    zeromatrix[,i]<-1
    D1_mat_prime=rbind(D1_mat_prime,zeromatrix)
  }
  ##############

  # D1_mat_prime=matrix(0,nrow=length(ysd),ncol=nrow(ysd))
  # k=1
  # for(j in 1:nrow(ysd)){
  #   D1_mat_prime[(k):(k-1+ncol(ysd)),j]=1
  #   k=k+ncol(ysd)
  # }
  #########
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
  ################
  #############
  X_matrix=cbind(delprime,1,D1_mat_prime,D2_mat_prime)
  X_matrix_prime_x=t(X_matrix)%*%X_matrix
  det(X_matrix_prime_x)
  x1_prime_x1=X_matrix_prime_x[(1:max(ysd)),(1:max(ysd))]
  x1_prime_x2=X_matrix_prime_x[(1:max(ysd)),(1+max(ysd)):ncol(X_matrix_prime_x)]
  x2_prime_x1=t(x1_prime_x2)
  x2_prime_x2=X_matrix_prime_x[(1+max(ysd)):nrow(X_matrix_prime_x),(1+max(ysd)):ncol(X_matrix_prime_x)]
  ##########
  i=1
  for(i in 1:ncol(x2_prime_x2)){
    rcols=c(1:ncol(x2_prime_x2))
    cj=setdiff(rcols,i)
    ri=setdiff(rcols,i)
    for(j in 1:nrow(x2_prime_x2)){

    }
  }

  inv_x2_prime_x2=Check_MP_Inverse(x2_prime_x2)
  c_mat=x1_prime_x1-x1_prime_x2%*%inv_x2_prime_x2%*%x2_prime_x1
  c_mat
  #####################################################
  e1=eigen(c_mat)$values
  e1=e1[e1>0.000000001]
  e2=e1/rep_mat[1,1]
  e3=1/e2
  cefficiency=length(e3)/sum(e3)
eigen_values<-e1
  l1=list("C Matrix"=round(c_mat,digits=4),"EVs"=table(round(e1,digits=3)),"Cannonical Efficiency"=cefficiency)
  return(l1)

}
