#' A Trapezoid object 
#' 
#' Objects of class \code{Candidate} are created by \code{addCandidate} functions
#'
#' 
#' An object of the class `Candidate' has the following slots:
#' \itemize{
#' \item \code{name} Name of the candidate
#' \item \code{delegatesWon} Number of delegates won by the candidate so far
#' \item \code{party} Candidate party
#' \item \code{delegatesNeeded} Number of delegates needed 
#' }
#'
#' @author Miguel Pereira: \email{m.pereira@@wustl.edu}
#' @aliases Trapezoid-class initialize,Trapezoid-method show,Trapezoid-method print,Trapezoid-method plot,Trapezoid-method
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid",
         slots = c(
           X = "numeric",
           Y = "numeric",
           a = "numeric",
           b = "numeric",
           Int = "numeric"
         ),
         prototype = prototype(
           X = numeric(),
           Y = numeric(),
           a = numeric(),
           b = numeric(),
           Int = numeric()
         )
)


#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, X, Y, a, b){
            .Object@X <- X
            .Object@Y <- Y
            .Object@a <- a
            .Object@b <- b
            
            sorted<-sort.int(X, index.return=T)$ix
            x<-X[sorted]
            y<-Y[sorted]
            n<-length(x[x>=a&x<=b])
            h<-(b-a)/n
            
            .Object@Int <- h/2*(y[1]+2*sum(y[2:n-1])+y[n])
            value=callNextMethod()
            return(value)
          }
) 

            
#' @export
setMethod(f="show",
          # Class the method is used for
          signature="Trapezoid",
          definition=function(object){
            showtrap <- list(Values=cbind(X=object@X, Y=object@Y), Integral=object@Int)
            print(showtrap)
          }   
)

#' @export
# Print method (print is a S3 function)
print.Trapezoid <- function(trapezoid){
  paste("Using the trapezoidal rule, the integral of this function from",trapezoid@a,"to",trapezoid@b,"is",round(trapezoid@Int,3), sep=" ")
}

