### TRAVEL REPOSITORY ###
#author: Mike Zizzi

mikesfunction<-function(parentname){
    x<-apply(vrules.west[grep(parentname,vrules.west[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
    if(is.null(dim(x))){
    x<-sapply(x,function(x) gsub(".* Contains ","",x))
    x<-sapply(x,function(x) gsub(".*Description Is Blank","",x))
    x<-sapply(x,function(x) gsub(".*Equals ","",x))
    x[8]<-gsub("-","",as.character(west.vanilla$Category.Code[which(west.vanilla$Rule.ID==x[1])][1])) 
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

ulti.travel.repo<-matrix(ncol=8)
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
x13<-mikesfunction("IMPARK")
x14<-mikesfunction("AVIS BUDGET GROUP")
ulti.travel.repo.west<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14))

###########################################################################################################


ulti.food.repo.west<-matrix(ncol=8)
x1<-mikesfunction("STARBUCKS COFFEE")
x2<-mikesfunction("DOMINOS PIZZA")
x3<-mikesfunction("PIZZA PIZZA")
x4<-mikesfunction("MCDONALDS")
x5<-mikesfunction("CHIPOTLE MEXICAN GRILL INC")
x6<-mikesfunction("BURGER KING")
x7<-mikesfunction("BOSTON PIZZA")
x8<-mikesfunction("WENDY S")
x9<-mikesfunction("PANERA BREAD CO")
x10<-mikesfunction("COCA COLA BOTTLING CO")
x11<-mikesfunction("TIM HORTONS")
x12<-mikesfunction("DUNKIN BRANDS GROUP")
ulti.food.repo.west<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12))

###########################################################################################################
ulti.tech.repo.west<-matrix(ncol=8)

x1<-mikesfunction("APPLE COMPUTER INC")
x2<-mikesfunction("DROPBOX")
x3<-mikesfunction("GOOGLE INC")
x4<-mikesfunction("BEST BUY")
x5<-mikesfunction("DELL INC")

ulti.tech.repo.west<-do.call(rbind,list(x1,x2,x3,x4,x5))

###########################################################################################################
ulti.retail.repo.west<-matrix(ncol=8)

x1<-mikesfunction("AMAZON.COM")
x2<-mikesfunction("COSTCO WHOLESALE")
x3<-mikesfunction("HOME DEPOT")
x4<-mikesfunction("SHOPPERS FOOD & PHARMACY")
x5<-mikesfunction("ANIXTER INC")
x6<-mikesfunction("LOWES COMPANIES INC")
x7<-mikesfunction("PARTY CITY")
x8<-mikesfunction("MICHAELS STORES")
x9<-mikesfunction("STAPLES")
x10<-mikesfunction("STAPLES INC")
ulti.retail.repo.west<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))

###########################################################################################################

ulti.west.repo<-do.call(rbind,list(ulti.food.repo.west,ulti.retail.repo.west,ulti.tech.repo.west,ulti.travel.repo.west))


