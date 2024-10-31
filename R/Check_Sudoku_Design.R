#' Check Properties of Sudoku Designs
#'
#' @param Design Give the Sudoku design in a matrix format
#' @param Region Provide a Region matrix corresponding to Sudoku design
#'
#' @return Design along with design parameters, C matrix (Information matrix), eigenvalues(EVs) and canonical efficiency factor (CEF) of a given Sudoku design
#'
#' @examples
#' library(SudokuDesigns)
#'design<-matrix(c(1,2,3,4,3,4,1,2,2,1,4,3,4,3,2,1),nrow=4,ncol=4,byrow=TRUE)
#' region<-matrix(c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4),nrow=4,ncol=4,byrow=TRUE)
#' Check_Sudoku_Design(design,region)

#' @export
Check_Sudoku_Design=function(Design,Region){
  ysd=as.matrix(Design)
  region=as.matrix(Region)
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
  D1_mat_prime=matrix(0,nrow=length(ysd),ncol=nrow(ysd))
  k=1
  for(j in 1:nrow(ysd)){
    D1_mat_prime[(k):(k-1+ncol(ysd)),j]=1
    k=k+ncol(ysd)
  }
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
  #############Diagonal matrix of row sizes
  row_sizes=c()
  for(i in 1:nrow(ysd)){
    row_sizes=c(row_sizes,length(ysd[i,]))
  }
  K_mat_alpha=diag(row_sizes)
  M1=t(D1_mat_prime)%*%D2_mat_prime
  M2=t(D1_mat_prime)%*%D3_mat_prime
  M3=t(D2_mat_prime)%*%D3_mat_prime
  col_sizes=c()
  for(i in 1:ncol(ysd)){
    col_sizes=c(col_sizes,length(ysd[,i]))
  }
  K_mat_beta=diag(col_sizes)
  region_size=c()
  for(i in 1:max(region)){
    region_size=c(region_size,length(which(region==i)))
  }
  K_mat_gamma=diag(region_size)
  rep_vector=rep_mat[row(rep_mat)==col(rep_mat)]
  r_tau=t(delprime)%*%matrix(1,ncol=1,nrow=ncol(t(delprime)))
  N1=t(delprime)%*%D1_mat_prime
  N2=t(delprime)%*%D2_mat_prime
  N3=t(delprime)%*%D3_mat_prime
  n=t(matrix(1,ncol=1,nrow=ncol(t(delprime))))%*%matrix(1,ncol=1,nrow=ncol(t(delprime)))
  K11=solve(K_mat_beta)+solve(K_mat_beta)%*%M3%*%Check_MP_Inverse(K_mat_gamma-t(M3)%*%solve(K_mat_beta)%*%M3)%*%t(M3)%*%solve(K_mat_beta)
  K12=-solve(K_mat_beta)%*%M3%*%Check_MP_Inverse(K_mat_gamma-t(M3)%*%solve(K_mat_beta)%*%M3)
  K22=Check_MP_Inverse(K_mat_gamma-t(M3)%*%solve(K_mat_beta)%*%M3)
  A11=solve(K_mat_alpha)+solve(K_mat_alpha)%*%(M1%*%K11%*%t(M1)+M1%*%K12%*%t(M2)+M2%*%t(K12)%*%t(M1)+M2%*%K22%*%t(M2))%*%solve(K_mat_alpha)
  A12=-solve(K_mat_alpha)%*%(M1%*%K11+M2%*%t(K12))
  A13=-solve(K_mat_alpha)%*%(M1%*%K12+M2%*%(K22))
  ####################
  #########################
  x1_p_x1=t(delprime)%*%delprime
  x1_p_x2=cbind(r_tau,N1,N2,N3)
  x2_p_x2=rbind(cbind(n,t(row_sizes),t(col_sizes),t(region_size)),
                cbind(row_sizes,K_mat_alpha,M1,M2),
                cbind(col_sizes,t(M1),K_mat_beta,M3),
                cbind(region_size,t(M2),t(M3),K_mat_gamma))

  inv_x2_p_x2=Check_MP_Inverse(x2_p_x2)
  final_c_mat=x1_p_x1-x1_p_x2%*%(inv_x2_p_x2)%*%t(x1_p_x2)
  #####Corrected C matrix

  C_mat=diag(c(r_tau))-N1%*%A11%*%t(N1)-N1%*%A12%*%t(N2)-N1%*%A13%*%t(N3)-
    N2%*%t(A12)%*%t(N1)-N2%*%(K11)%*%t(N2)-N2%*%K12%*%t(N3)-
    N3%*%t(A13)%*%t(N1)-N3%*%t(K12)%*%t(N2)-N3%*%K22%*%t(N3)
  identical(C_mat,final_c_mat)
  C_mat-final_c_mat
  eigen(C_mat)$values-eigen(final_c_mat)$values
  ##############
  #####################################################
  e1=eigen(final_c_mat)$values
  #rankMatrix(final_c_mat)
  e1=e1[(e1)>0.000000001]
  e1=eigen(C_mat)$values
  e1=e1[(e1)>0.000000001]
  e1=eigen(final_c_mat)$values
  e1=e1[(e1)>0.000000001]
  e2=e1/rep_mat[1,1]
  e3=1/e2
  cefficiency=length(e3)/sum(e3)
  cefficiency
  design<-as.matrix(Design)
  colnames(design)<-NULL
  list<-list("Gerechte Design"=design,"Number of Regions"=max(region),"Number of Rows"=nrow(design),"Number of Columns"=ncol(design),"C matrix"=round(final_c_mat,3),"EVs"=table(round(e1,digits=3)),"Cannonical Efficiency Factor"=cefficiency)
  return(list)
}
