#' Calculates tolerance test
#'
#' This function calculates the integral of a function fun between points a and b
#' using either the Trapezoid or the Simpson method.
#' 
#' The function accepts three arguments: the delegate's name, the number of delegates won and her party.
#'
#' @param name A string with the name of the presidential candidate. 
#' @param delegatesWon A numeric object with the number of delegates already won by the candidate.
#' @param party A string with the candidate's party. It accepts either 'Dem' or 'Rep' for the Democratic and Republican parties, respectively.
#'
#' @return An object of class `Candidate' that contains
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