#' A Simpson object 
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
#' @aliases Simpson-class initialize,Simpson-method show,Simpson-method print,Simpson-method
#' @rdname Simpson
#' @export
setClass(Class="Simpson",
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
setMethod("initialize", "Simpson", 
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
            if (n==2){
                .Object@Int <-h/3*(y[1]+y[2])
            }
            if (n==3){
              .Object@Int <-h/3*(y[1]+4*y[2]+y[3])
            }
            else {
              .Object@Int <- h/3*(y[1] + y[n] + 
                             4*sum(y[seq(2,n-1,by=2)]) + 
                             2*sum(y[seq(3,n-2,by=2)]))
            }
            value=callNextMethod()
            return(value)
          }
) 



#' @export
setMethod(f="show",
          # Class the method is used for
          signature="Simpson",
          definition=function(object){
            showsimp <- list(Values=cbind(X=object@X, Y=object@Y), Integral=object@Int)
            print(showsimp)
          }   
)

#' @export
# Print method (print is a S3 function)
print.Simpson <- function(simpson){
  return(round(simpson@Int,4))
}