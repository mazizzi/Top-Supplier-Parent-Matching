#### Rule Parser & Writer by Parent (Staples) Final ####
#Author: Mike Zizzi

library(dplyr)

#Load data Subset by staples and fields not blank
lib.vanilla<-read.csv("C:/Users/mzizzi/Documents/mlearn jaggaer/Jaggaer_machine-learning/Liberty_Export.csv",stringsAsFactors = FALSE)
datagrabber<-function(parentname,datasource){
  data.fun<-datasource[which(datasource$Supplier.Parent.Name == parentname),]
  data.fun<-data.fun[which(data.fun$Description!=""),]
  data.fun<-data.fun[,-which(names(data.fun) %in% c("Category.Level.1","Category.Level.2","Category.Level.3",
                                                   "Category.Level.4","Category.Level.5","Category.Level.6"))]
  return(data.fun)
}

lib.parentsub<-datagrabber("STAPLES INC",lib.vanilla)


#Grab rules to use
vrules.west<-read.csv("westonrules.csv",stringsAsFactors = FALSE)[,c(1,2,9,10,11,12,13,14)]
x<-apply(vrules.west[grep("STAPLES",vrules.west[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
x<-apply(x,2,function(x) gsub(".* Contains ","",x))
x<-apply(x,2,function(x) gsub(".*Description Is Blank","",x))
x<-apply(x,2,function(x) gsub(".*Equals ","",x))
x
x<-x[-2,]
#### review/debug needed

#Parse for rule matches
ruleparser<-function(datasource,rulematrix){
dummyvector<-c()
parent.list<-vector('list',length=nrow(datasource))
for(i in 1:nrow(datasource)){
  dummymatrix<-matrix(nrow=nrow(x),ncol=2)
  for(j in 1:nrow(x)){
    dummyvector<-c()
    if((length(grep(rulematrix[j,4],apply(datasource[i,],1,as.character), ignore.case = TRUE))==0)){next}
    else {dummyvector<-(grep(rulematrix[j,4],apply(datasource[i,],1,as.character), ignore.case = TRUE)[1])
    dummymatrix[j,1]<-j
    dummymatrix[j,2]<-dummyvector}
    
  }
  if(all(is.na(dummymatrix))){next}
  else(parent.list[[i]]<-na.omit(dummymatrix))
}
return(parent.list)
}

lib.parsed<-ruleparser(lib.parentsub,x)

#Remove nulls
lib.parsed<-lib.parsed[!sapply(lib.parsed,is.null)]

buildrule<-function(i,datasource,dataparsed){
  if(is.null(dataparsed[[i]])){next}
  add1<-( paste( gsub("[[:punct:]]"," ",colnames(datasource[(dataparsed[[i]][1,2])])), "Contains",x[dataparsed[[i]][1,1],4])) ### Needs finish
  b<-"Supplier Parent Name Contains STAPLES"
  rulesid<-x[dataparsed[[i]][1,1],1]
  return(c(b,add1,rulesid))
}


rulematrix<-matrix(nrow=length(lib.parsed),ncol=3)
for(i in 1:length(lib.parsed)){
  abc<-buildrule(i,lib.parentsub,lib.parsed)
  rulematrix[i,1]<-abc[1]
  rulematrix[i,2]<-abc[2]
  rulematrix[i,3]<-abc[3]
  
  
}
df.rulematrix<-data.frame(rulematrix)

new.ruledf<-subset(df.rulematrix, !duplicated(X3))

index<-new.ruledf[,3]
taxmap<-read.csv("C:/Users/mzizzi/Documents/mlearn/ultimate_tax_map.csv")
matchlist<-c()
for(i in 1:length(index)){
  matchlist[i]<-gsub("-","",west.vanilla$Category.Code[which(west.vanilla$Rule.ID==index[i])[1]])
}
matchlist[1]<-"101912"
matchlist<-matchlist[-1]
matchlist<-matchlist[-1]
matchlist
#### review/debug needed

taxmatch<-read.csv("lib_west_match.csv")

libmatch<-c()
for(i in 1:length(matchlist)){
libmatch[i]<-taxmatch$V2[match(matchlist[i], taxmatch$V1)]
}


new.ruledf<-new.ruledf[-c(1,2),-3]
new.ruledf$X3<-libmatch
new.ruledf

write.csv(new.ruledflib,file="imputed_rules_lib.csv")


