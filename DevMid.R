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


x<-seq(0,5,by=.5)
y=cos(x)
plot(x,y)

#Creating a Trapezoid object from scratch
tr<-new("Trapezoid",x,y,0,1)
#Using the integratIt method
tr<-integrateIt(X=x,Y=y,a=0,b=2,Rule="Trap")
#Using the show method
show(tr)
#Using the print method
print(tr)

#Using the plot method for Trapezoid objects
plot(tr)



#Creating a Simspon object from scratch
sp<-new("Simpson",x,y,0,4)
#Using the integratIt method
sp<-integrateIt(X=x,Y=y,a=0,b=2,Rule="Simp")
#Using the show method
show(sp)
#Using the print method
print(sp)




#Using the plot method for Trapezoid objects
plot(sp)


#PLOT METHOD FOR SIMPSON objects
length(x)

subvec<-which(x>=0 & x<=4)
#And sorting them by x
sorted<-sort.int(x[subvec], index.return=T)$ix
x.vec<-x[sorted]
y.vec<-y[sorted]

n<-length(x[x>=0&x<=4])

midpoint<-seq(2,n,2)


plot(x,cos(x))
X<-seq(x[2-1],x[2+1],length.out=20)
plot(x=seq(0,2,length.out=10),y=seq(0,5,length.out=10),type="n")
lines(X,px[,1])



top<-function(x,y){
  n<-length(x)
  
  v<-(x[1]+x[2])/2
  X<-seq(min(x),max(x),by=.01)
  px<-y[1]*(((X-v)*(X-x[n]))/((x[1]-v)*(x[1]-x[n])))+
    y[v]*(((X-x[1])*(X-x[n]))/((v-x[1])*(v-x[n])))+
    y[n]*(((X-x[1])*(X-v))/((x[n]-x[1])*(x[n]-v)))
  plot(x,y)
  lines(px)
  segments(x.vec[c(i-1,i+1)],rep(0,n),x.vec[c(i-1,i+1)],y.vec[c(i-1,i+1)],col="gray")
}



#Testing method tolTest
f<-function(x) x^2
tolTest(f,0,2)
tolTest(f,0,2,Rule="Simp")

