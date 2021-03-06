% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLMatrixArithmetic.R
\name{+}
\alias{+}
\title{Addition of in-database objects.}
\usage{
"+"(pObj1, pObj2)
}
\arguments{
\item{pObj1}{can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
a normal R object like matrix,sparseMatrix,vector}

\item{pObj2}{can be an in-database object like FLMatrix,FLSparseMatrix,FLVector or
a normal R object like matrix,sparseMatrix,vector}
}
\value{
\code{+} returns an in-database object if there is atleast one in-database object 
as input.Otherwise, the default behavior of R is preserved
}
\description{
\code{+} does the addition of in-database objects.
}
\details{
The addition of in-database objects mimics the normal addition of R data types.
All combinations of operands are possible just like in R and the result is an in-database object.
}
\examples{
connection <- flConnect(odbcSource="Gandalf")
flmatrix <- FLMatrix("FL_DEMO", 
"tblMatrixMulti", 1,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
Rvector <- 1:5
ResultFLmatrix <- flmatrix + Rvector
}

