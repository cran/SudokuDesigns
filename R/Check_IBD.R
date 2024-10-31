#' Check properties of an incomplete block design (IBD)
#'
#' @param Design Provide an IBD in matrix format
#'
#' @return Provides C matrix (Information matrix), eigenvalues(EVs) and canonical efficiency factor (CEF) of a given IBD
#' @export
#'
#' @examples
#' library(SudokuDesigns)
#' Design<-matrix(c(1,2,3,2,5,3,2,4,6),nrow=3,byrow=TRUE)
#' Check_IBD(Design)

Check_IBD=function(Design){
  ysd=Design
  length_ysd=length(ysd)-length(ysd[ysd==0])
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
  length(ysd[ysd>0])
  #######
  rep_mat= t(delprime)%*%delprime
  ######################################### observation vs block matrix
  ################################################
  D1_mat_prime=NULL
  for(i in 1:nrow(ysd)){
    rowsize=length(ysd[i,][ysd[i,]>0])
    zeromatrix=matrix(0,nrow=rowsize,ncol=nrow(ysd))
    zeromatrix[,i]<-1
    D1_mat_prime=rbind(D1_mat_prime,zeromatrix)
  }
  K_matrix=t(D1_mat_prime)%*%D1_mat_prime
  N_matrix=t(delprime)%*%D1_mat_prime
  #library(MASS)
  c_mat=rep_mat-N_matrix%*%solve(K_matrix)%*%t(N_matrix)
  #c_mat<-round(c_mat,digits=4)
  e1=eigen(c_mat)$values
  e1=e1[e1>0.000000001]
  #print(table(e1))
  e2=e1/rep_mat[1,1]
  e3=1/e2
  cefficiency=length(e3)/sum(e3)
  eigen_values=e1
  l1=list("C Matrix"=round(c_mat,digits=4),table(eigen_values),"Cannonical Efficiency"=cefficiency)
  return(l1)
}
