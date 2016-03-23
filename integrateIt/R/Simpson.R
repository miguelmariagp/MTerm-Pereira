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
#' @aliases Simpson-class initialize,Simpson-method show,Simpson-method print,Simpson-method plot,Simpson-method
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




#' @export
setMethod(f="plot",
          # Class the method is used for
          signature="Simpson",
          # The method itself
          definition=function(x=NULL, y=x, ...){
            
            #Subsetting the vectors between a and b in order to plot only 
            #those used to estimate the integral
            subvec<-which(x@X>=x@a & x@X<=x@b)
            #And sorting them by x
            sorted<-sort.int(x@X[subvec], index.return=T)$ix
            x.vec<-x@X[sorted]
            y.vec<-x@Y[sorted]
            n<-length(x.vec)
            
            #To distinguish v from u and w, 
            #this vector gives me the indices of v, so v-1 is u and v+1 is w
            midpoint<-seq(2,n,2)
            
            plot(x.vec,y.vec,pch=16,main="Graphical display of Simpson's rule",
                 ylim=c(min(y.vec),max(y.vec)),ylab="f(X)", xlab="X")
            
            #I know, it's a for loop. But time was running.
            for (i in midpoint){
              X<-seq(x.vec[i-1],x.vec[i+1],length.out=20)
              px<-y.vec[i-1]*((X-x.vec[i])*(X-x.vec[i+1]))/((x.vec[i-1]-x.vec[i])*(x.vec[i-1]-x.vec[i+1]))+
                y.vec[i]*((X-x.vec[i-1])*(X-x.vec[i+1]))/((x.vec[i]-x.vec[i-1])*(x.vec[i]-x.vec[i+1])) +
                y.vec[i+1]*((X-x.vec[i-1])*(X-x.vec[i]))/((x.vec[i+1]-x.vec[i-1])*(x.vec[i+1]-x.vec[i]))
              #Here I add the parabolas
              lines(X,px,col="gray")
              
              #Here I add the vertical lines
              segments(x.vec[c(i-1,i+1)],rep(0,n),x.vec[c(i-1,i+1)],y.vec[c(i-1,i+1)],col="gray")
              points(x.vec[i],y.vec[i],col="red",pch=16)
            }
            abline(a=0,b=0)
            legend("top",legend="midpoints",col="red",pch=16,bty="n")
            
          }   
)

