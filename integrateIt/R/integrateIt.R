#' Compute integral from a to b
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