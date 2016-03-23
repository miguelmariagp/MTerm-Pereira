#' A Trapezoid object 
#' 
#' Objects of class \code{Trapezoid}
#'
#' 
#' An object of the class `Candidate' has the following slots:
#' \itemize{
#' \item \code{X} Vector X
#' \item \code{Y} Vector f(X)
#' \item \code{a} initial point to integrate
#' \item \code{b} final point to integrate 
#' \item \code{Int} The estimated integral using Trapezoidal's rule 
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
            
            .Object@a <- a
            .Object@b <- b
            
            #Subsetting the vectors between a and b
            #This allows for the vectors to be larger than the area for which we want to compute the integral
            subvec<-which(X>=a&X<=b)
            #And sorting them
            sorted<-sort.int(X[subvec], index.return=T)$ix
            x<-X[sorted]
            y<-Y[sorted]
            
            .Object@X <- x
            .Object@Y <- y
            
            
            n<-length(x)
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
            
            #Subsetting the vectors between a and b in order to show only those used to estimate the integral
            subvec<-which(object@X>=object@a & object@X<=object@b)
            #And sorting them
            sorted<-sort.int(object@X[subvec], index.return=T)$ix
            x<-object@X[sorted]
            y<-object@Y[sorted]
            
            object@X <- x
            object@Y <- y
            
            showtrap <- list(Values=cbind(X=object@X, Y=object@Y), Integral=object@Int)
            print(showtrap)
          }   
)

#' @export
# Print method (print is a S3 function)
print.Trapezoid <- function(trapezoid){
  return(round(trapezoid@Int,4))
}


#' @export
setMethod(f="plot",
          # Class the method is used for
          signature="Trapezoid",
          # The method itself
          definition=function(x=NULL, y=x, ...){
            
            #Subsetting the vectors between a and b in order to plot only 
            #those used to estimate the integral
            subvec<-which(x@X>=x@a & x@X<=x@b)
            #And sorting them by x
            sorted<-sort.int(x@X[subvec], index.return=T)$ix
            x.vec<-x@X[sorted]
            y.vec<-x@Y[sorted]
            
            plot(x.vec,y.vec,pch=16,main="Graphical display of trapezoidal rule",
                 ylim=c(min(y.vec),max(y.vec)),ylab="f(X)", xlab="X")
            n<-length(x.vec)
            #Creating the oblique segments
            segments(x.vec[1:n-1],y.vec[1:n-1],x.vec[2:n],y.vec[2:n],col="red")
            #Creating the vertical segments
            segments(x.vec,rep(0,n),x.vec,y.vec,col="gray")
            abline(a=0,b=0)
            
          }   
)
