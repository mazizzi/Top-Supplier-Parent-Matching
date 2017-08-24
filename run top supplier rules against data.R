# Running top supplier rules against data #
#Author: Mike Zizzi

library(foreach)
library(doParallel)
library(iterators)

#subset data
sub.west1<-west.vanilla[which(west.vanilla$Supplier.Parent.Name %in% sapply(frules.coc,"[[",5) ),]
sub.west2<-west.vanilla[which(west.vanilla$Supplier.Parent.Name %in% sapply(frules.lib,"[[",5) ),]
sub.west<-rbind(sub.west1,sub.west2)
sub.west<-sub.west[which(sub.west$Category.Code!=""),]

sub.coc1<-coc.vanilla[which(coc.vanilla$Supplier.Parent.Name %in% sapply(frules.west,"[[",5) ),]
sub.coc2<-coc.vanilla[which(coc.vanilla$Supplier.Parent.Name %in% sapply(frules.lib,"[[",5) ),]
sub.coc <-rbind(sub.coc1,sub.coc2)
sub.coc<-sub.coc[which(sub.coc$Category.Code!=""),]

sub.lib1<-lib.vanilla[which(lib.vanilla$Supplier.Parent.Name %in% sapply(frules.west,"[[",5) ),]
sub.lib2<-lib.vanilla[which(lib.vanilla$Supplier.Parent.Name %in% sapply(frules.coc,"[[",5) ),]
sub.lib<-rbind(sub.lib1,sub.lib2)
sub.lib<-sub.lib[which(sub.lib$Category.Code!=""),]

rm(sub.lib1,sub.lib2,sub.coc1,sub.coc2,sub.west1,sub.west2)

set.seed(3272017)

west.sampleindex<-sample(1:75390,size=1000)
coc.sampleindex<-sample(1:57223,size=1000)
lib.sampleindex<-sample(1:106192,size=1000)

topsupplierrules.fun<-function(sampleset,client,i){
  if(client=="west")    {dummy.paste<-paste(sampleset[1,,]$Account.Description,sampleset[1,,]$Account.Code,
                                            sampleset[1,,]$PO.Line.Description,sampleset[1,,]$Commodity.Code.Description,
                                            sampleset[1,,]$Description,sampleset[1,,]$MCC.Description,
                                            sampleset[1,,]$Faculty.or.Unit.ID,sampleset[1,,]$Expense.Type,
                                            sampleset[1,,]$Invoice.Line.Description,sep=" ")
  
  clientnum=1
  clientsum=sum(west.vanilla$Spend.Amount,na.rm = TRUE)
  rulelist<-append(frules.lib,frules.coc)
  } #end west
  else if(client=="coc"){dummy.paste<-paste(sampleset[1,,]$Detail.Description,
                                            sampleset[1,,]$ORGN.Title,sampleset[1,,]$Account.Title,sampleset[1,,]$Fund.Title,
                                            sampleset[1,,]$Invoice.Description,sampleset[1,,]$Commodity.Description ,sep=" ")
  clientnum=2
  clientsum=sum(coc.vanilla$Spend.Amount,na.rm = TRUE)
  rulelist<-append(frules.west,frules.lib)
  } #end coc 
  
  else if(client=="lib"){dummy.paste <-paste(sampleset[1,,]$Account.Description,
                                             sampleset[1,,]$Description,sampleset[1,,]$Invoice.Line.Description,
                                             sampleset[1,,]$Transaction.Description,sampleset[1,,]$Commodity.Description,
                                             sampleset[1,,]$Account.Title)
  clientnum=3 
  clientsum=sum(lib.vanilla$Spend.Amount,na.rm = TRUE)
  rulelist<-append(frules.west,frules.coc)
  } #end lib
  
  dummymatrix<-matrix(nrow=length(rulelist),ncol=12)
    for(j in(1:length(rulelist))){
     hit.counter<-c(0)
     all.counter<-c(0)
     parent.counter<-c(0)
     gpc<-c(0)
     apc<-c(0)
        
        if((length(grep(rulelist[[j]][[5]], fixed=TRUE ,as.character(sampleset$Supplier.Parent.Name)))==1)){
          parent.counter<-(1)
          for(k in (1:length(rulelist[[j]][-c(1,2,3,4,5)]))){
            if (length(agrep("Does Not Contain",rulelist[[j]][k+5],ignore.case = TRUE ))!=0){
              antiphrase.split<-strsplit(rulelist[[j]][k+5],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
            }
            #check that keyword[j,k] is not empty
            else if(is.na(rulelist[[j]][[k+5]])){
              next}
            
            else if(length(grep(rulelist[[j]][[k+5]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
            }
            else if(length(agrep(rulelist[[j]][[k+5]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
            }  
            
            } #end k loop 
          
        } #END first run grep parent
        
        ## second run for agrep parent
        
        else if((length(agrep(rulelist[[j]][[5]], ignore.case=TRUE ,as.character(sampleset[1,,9])))==1) && (length(rulelist[[j]][-c(1,2,3,4)])!=0)){
          parent.counter<-c(1)
          for(k in (1:length(rulelist[[j]][-c(1,2,3,4)]))){
            if (length(agrep("Does Not Contain",rulelist[[j]][k+5],ignore.case = TRUE ))!=0){
              antiphrase.split<-strsplit(rulelist[[j]][k+4],"Contain ")
              antiphrase<-antiphrase.split[[1]][2]
              #disqualify if antiphrase matches
              if(length(agrep(antiphrase,ignore.case = TRUE, dummy.paste))!=0){
                break
              }
            }
            #check that keyword[j,k] is not empty
            else if(is.na(rulelist[[j]][[k+5]])){
              next}
            
            else if(length(grep(rulelist[[j]][[k+5]],dummy.paste,fixed=TRUE))!=0){
              hit.counter<-hit.counter+1
            }
            else if(length(agrep(rulelist[[j]][[k+5]],dummy.paste,ignore.case = TRUE))!=0){
              hit.counter<-hit.counter+1
            }  
            
          } #end k loop 
        } #END second run agrep parent
        
      #if every word hits, add an additonal arb metric
      if(hit.counter==length(rulelist[[j]][-c(1,2,3,4,5)])){
        all.counter<-c(1)
      } 
      
     if((parent.counter==0) && (hit.counter == 0)){next} #dont enter non matches
     dummymatrix[j,1]=client
     dummymatrix[j,2]<-i
     dummymatrix[j,3]<-parent.counter
     dummymatrix[j,4]<-apc
     dummymatrix[j,5]<-gpc
     dummymatrix[j,6]<-all.counter
     dummymatrix[j,7]<-rulelist[[j]][2]
     dummymatrix[j,8]<-gsub("-","",as.character(sampleset$Category.Code))
     dummymatrix[j,9]<-sampleset[1,,]$Spend.Amount/clientsum
     dummymatrix[j,10]<-rulelist[[j]][1]
     dummymatrix[j,11]<-length(rulelist[[j]][2])-length(gsub("-","",as.character(sampleset$Category.Code)))
     dummymatrix[j,12]<-rulelist[[j]][4]
     
    } # end j loop
    
     
    
    return(dummymatrix)
}

### WEST ### 26.7 minutes per 1000
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
west.test<-foreach(i= 1:1000) %dopar%{
  topsupplierrules.fun(sub.west[west.sampleindex[i],],"west",i)
}
proc.time()-t
stopCluster(cl)

### COC ###
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
coc.test<-foreach(i= 1:1000) %dopar%{
  topsupplierrules.fun(sub.coc[coc.sampleindex[i],],"coc",i)
}
proc.time()-t
stopCluster(cl)


### lib ###
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
lib.test<-foreach(i= 1:1000) %dopar%{
  topsupplierrules.fun(sub.lib[lib.sampleindex[i],],"lib",i)
}
proc.time()-t
stopCluster(cl)

###### CLEAN ######

x<-lapply(west.test, function(x) na.omit(x))
west.final<-do.call(rbind,x)
west.final

y<-lapply(coc.test, function(x) na.omit(x))
coc.final<-do.call(rbind,y)
coc.final

z<-lapply(lib.test, function(x) na.omit(x))
lib.final<-do.call(rbind,z)
lib.final

#### extract ####
load("C:/Users/mzizzi/Documents/mlearn jaggaer/Jaggaer_machine-learning/final_toptaxmap.RData")
final.toptaxmap<-as.data.frame(final.toptaxmap)
lib.match<-lib.final[which(lib.final[,8] %in% final.toptaxmap$libertytax),]
sum(as.numeric(lib.match[,9]))
sum(as.numeric(lib.final[,9]))



#### Test #####
runsum<-c(0)
for(i in 1:nrow(lib.match)){
  if(lib.match[i,7]==""){next}
  if(lib.match[i,12]=="west"){
    if(length(as.character(final.toptaxmap$X3[which(final.toptaxmap$X1 %in% lib.match[i,7])]))==0){next}
    else if(lib.match[i,8]==as.character(final.toptaxmap$X3[which(final.toptaxmap$X1 %in% lib.match[i,7])])){
      runsum<-runsum+as.numeric(lib.match[i,9])}
  }
  else(
    if(length(as.character(final.toptaxmap$X3[which(final.toptaxmap$X2 %in% lib.match[i,7])]))==0){next}
    else if(lib.match[i,8]==as.character(final.toptaxmap$X3[which(final.toptaxmap$X2 %in% lib.match[i,7])])[1]){
      runsum<-runsum+as.numeric(lib.match[i,9])})
} 
runsum #0.002046474  # 40% accuracy

as.list(data.frame(t(lib.match)))

test.taxlist<-vector('list',length=length(z))
test.rulelist<-vector('list',length=length(z))
for (i in 1:length(z)){
  if(dim(z[[i]])[1]==0){next}
  abc<-tbl_df(z[[i]]) 
  aaa<-abc %>% count(V7) %>% arrange(desc(n))
  as.character(aaa[1,1])
  zzz<-z[[i]][which(z[[i]][,7]==as.character(aaa[1,1])),]
 holder<-as.matrix(z[[i]][which(z[[i]][,7]==as.character(aaa[1,1])),],ncol=12)
 test.taxlist[[i]]<-t(holder[1,])
  test.rulelist[[i]]<-z[[i]][which(z[[i]][,7]==as.character(aaa[1,1])),]
}

runsum<-c(0)
for(i in 1:length(test.taxlist)){
  if(is.null(test.taxlist[[i]])){next}
  if(ncol(test.taxlist[[i]])!=12){next}
  if(test.taxlist[[i]][1,7]==""){next}
  if(test.taxlist[[i]][1,12]=="west"){
    if(length(as.character(final.toptaxmap$X3[which(final.toptaxmap$X1 %in% test.taxlist[[i]][1,7])]))==0){next}
    else if(test.taxlist[[i]][1,8]==as.character(final.toptaxmap$X3[which(final.toptaxmap$X1 %in% test.taxlist[[i]][1,7])])){
      runsum<-runsum+as.numeric(test.taxlist[[i]][1,9])}
  }
  else(
    if(length(as.character(final.toptaxmap$X3[which(final.toptaxmap$X2 %in% test.taxlist[[i]][1,7])]))==0){next}
    else if(test.taxlist[[i]][1,8]==as.character(final.toptaxmap$X3[which(final.toptaxmap$X2 %in% test.taxlist[[i]][1,7])])[1]){
      runsum<-runsum+as.numeric(test.taxlist[[i]][1,9])})
} 
runsum #0.002046474  # 40% accuracy
