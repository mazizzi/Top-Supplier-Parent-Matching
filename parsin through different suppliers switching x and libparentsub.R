x<-apply(vrules.west[grep("FACEBOOK",vrules.west[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
x<-apply(x,2,function(x) gsub(".* Contains ","",x))
x<-apply(x,2,function(x) gsub(".*Description Is Blank","",x))
x<-apply(x,2,function(x) gsub(".*Equals ","",x))
x
lib.parentsub<-datagrabber("GLOBAL LASER INC",lib.vanilla)

library(foreach)
library(doParallel)
library(iterators)

ruleparser<-function(datasource,rulematrix){
  dummyvector<-c()
  parent.list<-vector('list',length=nrow(datasource))
  for(i in 1:nrow(datasource)){
    dummymatrix<-matrix(nrow=nrow(x),ncol=3)
    for(j in 1:nrow(x)){
      dummyvector<-c()
      if((length(grep(rulematrix[j,4],apply(datasource[i,],1,as.character), ignore.case = TRUE))==0)){next}
      else {dummyvector<-(grep(rulematrix[j,4],apply(datasource[i,],1,as.character), ignore.case = TRUE)[1])
      rulelength<-length(rulematrix[j,which(rulematrix[j,]!="")])
      if(rulelength>4){
        if(length(grep("Does Not Contain", rulematrix[j,5], ignore.case = TRUE))!=0){
          antiphrase<-strsplit(rulematrix[j,5],"Contain ")
          if(length(grep(antiphrase[[1]][2], apply(datasource[i,],1,as.character), ignore.case = TRUE))!=0){next}
          dummymatrix[j,1]<-j
          dummymatrix[j,2]<-dummyvector
          dummymatrix[j,3]<-"anti"
        }
        else{
          dummymatrix[j,1]<-j
          dummymatrix[j,2]<-dummyvector
          dummymatrix[j,3]<-(grep(rulematrix[j,5],apply(datasource[i,],1,as.character), ignore.case = TRUE)[1])}
      }
      
      dummymatrix[j,1]<-j
      dummymatrix[j,2]<-dummyvector
      dummymatrix[j,3]<-c("")}
      
    }
    if(all(is.na(dummymatrix))){next}
    else(parent.list[[i]]<-na.omit(dummymatrix))
  }
  return(parent.list)
}

lib.parsed<-ruleparser(lib.parentsub,x)

lib.parsed<-lib.parsed[!sapply(lib.parsed,is.null)]
lib.parsed


buildrule<-function(i,datasource,dataparsed){
  if(is.null(dataparsed[[i]])){next}
  add1<-( paste( gsub("[[:punct:]]"," ",colnames(datasource)[as.numeric(dataparsed[[i]][1,2])]), "Contains",x[as.numeric(dataparsed[[i]][1,1]),4])) ### Needs finish
  if(dataparsed[[i]][1,3]!=""){add2<-(paste( gsub("[[:punct:]]"," ",colnames(datasource)[as.numeric(dataparsed[[i]][1,3])]), "Contains",x[as.numeric(dataparsed[[i]][1,1]),5]))}
  else(add2<-c(""))
  b<-"Supplier Parent Name Contains ENTERPRISE RENT-A-CAR"
  rulesid<-x[as.numeric(dataparsed[[i]][1,1]),1]
  return(c(b,add1,add2,rulesid))
}



rulematrix<-matrix(nrow=length(lib.parsed),ncol=4)
for(i in 1:length(lib.parsed)){
  abc<-buildrule(i,lib.parentsub,lib.parsed)
  rulematrix[i,1]<-abc[1]
  rulematrix[i,2]<-abc[2]
  rulematrix[i,3]<-abc[3]
  rulematrix[i,4]<-abc[4]
}

df.rulematrix<-data.frame(rulematrix)

new.ruledf<-subset(df.rulematrix, !duplicated(X4))

new.ruledf