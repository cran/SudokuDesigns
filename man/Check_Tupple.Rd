% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Check_Tupple.R
\name{Check_Tupple}
\alias{Check_Tupple}
\title{Find tupple occurances in a given matrix rows}
\usage{
Check_Tupple(matrix, tupple)
}
\arguments{
\item{matrix}{Any matrix}

\item{tupple}{A vector of numbers}
}
\value{
Number of times a tupple occurs within the rows of a given matrix
}
\description{
Find tupple occurances in a given matrix rows
}
\examples{
mat1<-matrix(c(1,2,3,4,1,3,6,2,8,1,8,3),nrow=4,byrow=TRUE)
mat1
Check_Tupple(mat1,c(1,2))
}
