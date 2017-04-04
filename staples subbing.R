library(dplyr)

parentname<-"STAPLES INC"
staplesub<-coc.vanilla[which(coc.vanilla$Supplier.Parent.Name == "STAPLES INC"),]
paste("SUPPLIER PARENT NAME CONTAINS",parentname)

vrules.west[grep("STAPLES",vrules.west[,3]),]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


a<-c()
for(i in 1:1844){
  if((length(grep("DRY ERASE",as.vector(staplesub[i,]), ignore.case = TRUE))==0)){a[i]<-0}
  else (a[i]<-grep("DRY ERASE",as.vector(staplesub[i,]), ignore.case = TRUE))
}
a
staplesub[1,12]
staplesub[1,]

newrules<-matrix(nrow=10,ncol=14)
parent<-paste("SUPPLIER PARENT NAME CONTAINS",parentname)
add1<-toupper( paste( gsub("[[:punct:]]"," ",colnames(staplesub)[12]), "CONTAINS","DRY ERASE"))
newrules[1,1]<-parent
newrules[1,2]<-add1
newrules


a<-c()
for(i in 1:1844){
  if((length(grep("PENCIL",as.vector(staplesub[i,]), ignore.case = TRUE))==0)){a[i]<-0}
  else (a[i]<-grep("PENCIL",as.vector(staplesub[i,]), ignore.case = TRUE))
}
a

staplesub[411,]

staples.rules<-rulecleaner.mat(vrules.west[grep("STAPLES",vrules.west[,3]),])

x<-apply(vrules.west[grep("STAPLES",vrules.west[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
x<-apply(x,2,function(x) gsub(".* Contains ","",x))
x<-apply(x,2,function(x) gsub(".*Description Is Blank","",x))
x<-apply(x,2,function(x) gsub(".*Equals ","",x))
x
x<-x[-2,]

staplesub<-staplesub[,-c(51,52,53,54)]

dummyvector<-c()
staples.list<-vector('list',length=nrow(staplesub))
for(i in 1:nrow(staplesub)){
  dummymatrix<-matrix(nrow=nrow(x),ncol=2)
  for(j in 1:nrow(x)){
    dummyvector<-c()
    if((length(grep(x[j,4],as.vector(staplesub[i,]), ignore.case = TRUE))==0)){next}
    else (dummyvector<-Mode(grep(x[j,4],as.vector(staplesub[i,]), ignore.case = TRUE)))
    dummymatrix[j,1]<-j
    dummymatrix[j,2]<-dummyvector
    
  }
  if(all(is.na(dummymatrix))){next}
  else(staples.list[[i]]<-na.omit(dummymatrix))
}

a<-staples.list[!sapply(staples.list,is.null)]
a[[2]][which(!is.na(a[[2]]))]

buildrule<-function(i){
  if(is.null(a[[i]])){next}
  add1<-( paste( gsub("[[:punct:]]"," ",colnames(staplesub[(a[[i]][1,2])])), "Contains",x[a[[i]][1,1],4])) ### Needs finish
  b<-"Supplier Parent Name Contains STAPLES"
  rulesid<-x[a[[i]][1,1],1]
  return(c(b,add1,rulesid))
}

buildrule(209)
rulematrix<-matrix(nrow=length(a),ncol=3)
for(i in 1:length(a)){
  abc<-buildrule(i)
  rulematrix[i,1]<-abc[1]
  rulematrix[i,2]<-abc[2]
  rulematrix[i,3]<-abc[3]
  
  
}
rule.tbl<-tbl_df(rulematrix)
rule.tbl %>% group_by(V2) %>% summarize()
univ2<-unique(rulematrix[,2])
univ3<-unique(rulematrix[,3])
df.rulematrix<-data.frame(rulematrix,stringsAsFactors = FALSE )

new.ruledf<-subset(df.rulematrix, !duplicated(X3))

new.rulematrix<-matrix(nrow=length(testest),ncol=3)
new.rulematrix[,2]<-t(univ2)
new.rulematrix[,1]<-"Supplier Parent Name Contains STAPLES"
new.rulematrix[,3]<-t(univ3)


new.ruledf<-subset(df.rulematrix, !duplicated(X3))

index<-new.ruledf[,3]
taxmap<-read.csv("C:/Users/mzizzi/Documents/mlearn/ultimate_tax_map.csv")
matchlist<-c()
for(i in 1:length(index)){
  matchlist[i]<-gsub("-","",west.vanilla$Category.Code[which(west.vanilla$Rule.ID==index[i])[1]])
}

mord<-matrix(nrow=length(index),ncol=3)
mord[,1]<-index
mord[,2]<-matchlist
for(i in 1:15){
mord[i,3]<-taxmap$checked.list.redux.idnum2[match(mord[i,2],taxmap$checked.list.redux.idnum1)]
}

mord


new.ruledf$X4<-mord[,3]
new.ruledf<-new.ruledf[,-3]
new.ruledf


