#' Calculates tolerance test
#'
#' This function calculates the numbers of sub-divisions (n) necessary to approximate an integral to
#' correct value.
#' 
#' The function accepts seven arguments: a function, the starting (a) and ending (b) points of the integral,
#' the approximation rule, the initial n, and the correct integral.
#'
#' @param fun A that relates x with y.
#' @param a The starting point
#' @param b The ending point
#' @param tol A value for the difference between the approximation and the correct integral accepted.
#' @param Rule The approximation rule.
#' @param start Initial n
#' @param correct correct integral
#'
#' @return A list with the inputs and outputs of the tolerance test.
#' \itemize{
#' \item \code{name} Name of the candidate
#' \item \code{delegatesWon} Number of delegates won by the candidate so far
#' \item \code{party} Candidate party
#' \item \code{delegatesNeeded} Number of delegates needed to win the nomination in her party
#'  }
#' @author Miguel Pereira \email{m.pereira@@wustl.edu}
#' @examples
#' 
#' createCandidate("Sanders", 140, "Dem") 
#' 
#' @rdname tolTest
#' @aliases tolTest,ANY-method
#' @export
#' 
setGeneric(name="tolTest",
           def=function(fun,a,b,tol=1e-5,Rule="Trap",start=4, correct=integrate(fun,a,b))
           {standardGeneric("tolTest")}
)

setMethod(f="tolTest",
          definition=function(fun,a,b,tol=1e-5,Rule="Trap", start=4,
                              correct=integrate(fun,a,b)){
            n <- start
            h <- (b-a)/n
            x <- seq(a,b,by=h)
            y <- fun(x)
            
            integral.calc <- function(n,h,y,Rule){
              if (Rule=="Trap"){
                int.approx<-h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
              }
              if (Rule=="Simp"){
                if (n==3){
                  int.approx <- h/3*(y[1]+4*y[2]+y[3])
                }
                else {
                  int.approx <- h/3*(y[1] + y[n] + 
                                       4*sum(y[seq(2,n,by=2)]) + 
                                       2*sum(y[seq(3,n-1,by=2)]))
                }}
              return(int.approx)
            }

            int.approx<-integral.calc(n,h,y,Rule)
            int.original <-int.approx
            int.diff <- tol+1 #This line ensures that the while loop works at once
            
            while (int.diff>tol){
              int.approx.old <- int.approx
              n <- n*2
              h<-h/2
              #Recalculating the y's for the new n
              y[seq(1, n+1, by=2)] <- y  #reuses new y
              y[seq(2,n, by=2)] <- sapply(seq(a+h, b-h, by=2*h), fun)
              #Uses the internal function to compute the new integral
              int.approx<-integral.calc(n,h,y,Rule)
              int.diff <- abs(int.approx-int.approx.old)
            }
            
            return(list(
              a=a,
              b=b,
              initial.n=start,
              final.n = n,
              tolerance=tol,
              abs.error = abs(int.original-int.approx),
              integral = int.approx
            ))
            
          }
)