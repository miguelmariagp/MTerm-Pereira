#' Compute integral from a to b
#'
#' This function calculates the integral of a function fun between points a and b
#' using either the Trapezoid or the Simpson method.
#' 
#' The function accepts four arguments: vector X, vector Y, initial point a, final point b, and the rule used to compute the integral.
#'
#' @param X Vector with X values. 
#' @param Y Vector with f(X) values.
#' @param a The starting point
#' @param b The ending point
#' @param Rule Integration rule
#'
#' @return The output of the function 'integrateIt' is either a Trapezoid or Simpson object with the following elements
#' \itemize{
#' \item \code{X} Vector X
#' \item \code{Y} Vector Y
#' \item \code{Int} The estimated integral
#'  }
#' @author Miguel Pereira \email{m.pereira@@wustl.edu}
#' 
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
#' 
setGeneric(name="integrateIt",
           def=function(X,Y,a,b,Rule)
           {standardGeneric("integrateIt")}
)

setMethod(f="integrateIt",
          definition=function(X,Y,a,b,Rule){
            if (Rule=="Trap"){
              return(new("Trapezoid",X=X,Y=Y,a=a,b=b))}
            if (Rule=="Simp"){
              return(new("Simpson",X=X,Y=Y,a=a,b=b))
            }
            else {
              print("Please specify the rule you want to use")} 
              }
            )