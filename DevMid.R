#Miguel Maria Pereira
#Midterm
library(devtools)
library(roxygen2)

setwd("C:/Users/ststest/Dropbox/Spr16/Programming/MTerm-Pereira") #This will need to be changed to match your directory

create("integrateIt")

current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)

