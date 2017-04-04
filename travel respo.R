### TRAVEL REPOSITORY ###
ulti.travel.repo<-matrix(ncol=8)

mikesfunction<-function(parentname){
    x<-apply(vrules.west[grep(parentname,vrules.west[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
    if(is.null(dim(x))){
    x<-sapply(x,function(x) gsub(".* Contains ","",x))
    x<-sapply(x,function(x) gsub(".*Description Is Blank","",x))
    x<-sapply(x,function(x) gsub(".*Equals ","",x))
    x[8]<-gsub("-","",as.character(west.vanilla$Category.Code[which(west.vanilla$Rule.ID==170378)][1])) 
  }
  
  else{
  x<-apply(x,2,function(x) gsub(".* Contains ","",x))
  x<-apply(x,2,function(x) gsub(".*Description Is Blank","",x))
  x<-apply(x,2,function(x) gsub(".*Equals ","",x))
  for(i in 1:nrow(x)){
    x[i,8]<-gsub("-","",as.character(west.vanilla$Category.Code[which(west.vanilla$Rule.ID==x[i,1])][1]))
  }}
  xf<-matrix(x,ncol=8)
  return(xf)
}
x1<-mikesfunction("AMERICAN AIRLINES")
x2<-mikesfunction("HILTON WORLDWIDE")
x3<-mikesfunction("MARRIOTT INTERNATIONAL INC")
x4<-mikesfunction("WESTJET AIRLINES")
x5<-mikesfunction("ENTERPRISE RENT-A-CAR")
x6<-mikesfunction("EXPEDIA INC")
x7<-mikesfunction("GREYHOUND LINES")
x8<-mikesfunction("FAIRMONT HOTELS")
x9<-mikesfunction("DELTA AIRLINES")
x10<-mikesfunction("HYATT HOTELS")
x11<-mikesfunction("BESTWESTERN")
x12<-mikesfunction("CARLSON HOTELS")
ulti.travel.repo.west<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12))
