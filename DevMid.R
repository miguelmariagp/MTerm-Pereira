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
x1<-seq(0,2,by=.2)
y1<-x^2

x2<-seq(0,5,by=.5)
y2<-cos(x)


#Creating a Trapezoid object from scratch
tr<-new("Trapezoid",x1,y1,0,2)
#Using the integratIt method
tr<-integrateIt(X=x1,Y=y1,a=0,b=1,Rule="Trap")
#Using the show method
show(tr)
#Using the print method
print(tr)

#Using the plot method for Trapezoid objects
plot(tr)
tr2<-new("Trapezoid",x2,y2,0,4)
plot(tr2)



#Creating a Simpson object from scratch
sp<-new("Simpson",x1,y1,0,2)
#Using the integratIt method
sp<-integrateIt(X=x1,Y=y1,a=0,b=1,Rule="Simp")
#Using the show method
show(sp)
#Using the print method
print(sp)

#Using the plot method for Trapezoid objects
plot(sp)
sp2<-new("Simpson",x2,y2,0,4)
plot(sp2)



#Testing method tolTest
f<-function(x) x^2
tolTest(f,0,2)
tolTest(f,0,2,Rule="Simp")

