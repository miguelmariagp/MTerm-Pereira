#Miguel Maria Pereira
#Midterm
library(devtools)
library(roxygen2)

setwd("C:/Users/ststest/Dropbox/Spr16/Programming/MTerm-Pereira") #This will need to be changed to match your directory

#create("integrateIt")

current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)

#Example data
x<-seq(0,2,by=.2)
y=x^2

#Creating a Trapezoid object from scratch
tr<-new("Trapezoid",x,y,0,2)
#Using the show method
show(tr)
#Using the print method
print(tr)


#Creating a Simspon object from scratch
sp<-new("Simpson",x,y,0,2)
#Using the show method
show(sp)
#Using the print method
print(sp)


#Using the integratIt method
tr<-integrateIt(X=x,Y=y,a=0,b=1,Rule="Trap")

#Using the plot method
plot(tr)


tolTest(sqrt,1,2)

tolTest(sqrt,1,2,Rule="Simp")

