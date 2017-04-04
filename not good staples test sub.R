
library(foreach)
library(doParallel)
library(iterators)
library(dplyr)

coc.vanilla<-read.csv(file="C:/Users/mzizzi/Documents/mlearn jaggaer/Jaggaer_machine-learning/Charleston_Export.csv", stringsAsFactors = FALSE)

c.staples<-frules.coc [which(sapply(frules.coc, "[[",5)=="STAPLES INC")]
w.staples<-frules.west[which(sapply(frules.west,"[[",5)=="STAPLES INC")]
l.staples<-frules.lib [which(sapply(frules.lib, "[[",5)=="STAPLES INC")]
staples<-append(c.staples,w.staples)
staples<-append(staples,l.staples)


which(coc.vanilla$Supplier.Parent.Name %in% "STAPLES INC")

grep("STAPLES INC",as.vector(coc.vanilla[34438,]), ignore.case = TRUE)

as.vector (coc.vanilla[34438,])
as.numeric(coc.vanilla[34438,])
unname(unlist(coc.vanilla[34438,]))

coc.staples<-coc.vanilla[which(coc.vanilla$Supplier.Parent.Name=="STAPLES INC"),]
coc.staples<-coc.staples[which(coc.staples$Category.Code!="" ),]
coc.staples<-coc.staples[which(coc.staples$Category.Code!="22-34" ),]
coc.staples<-coc.staples[which(coc.staples$Detail.Description!="TAXES"),]

topsupplierrules.fun<-function(sampleset,client,i){
  
  
  if(client=="coc"){dummy.paste<-paste(sampleset[1,,]$Detail.Description,
                                       sampleset[1,,]$ORGN.Title,sampleset[1,,]$Account.Title,sampleset[1,,]$Fund.Title,
                                       sampleset[1,,]$Invoice.Description,sampleset[1,,]$Commodity.Description ,sep=" ")}
  
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
    
   
    #if every word hits, add an additonal arb metric
    if(hit.counter==length(rulelist[[j]][-c(1,2,3,4,5)])){
      all.counter<-c(1)
    } 
    
    if((parent.counter==0) && (hit.counter == 0)){next} #dont enter non matches
    dummymatrix[j,1]=client
    dummymatrix[j,2]<-i
    dummymatrix[j,3]<-parent.counter
    dummymatrix[j,4]<-hit.counter
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


num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
t<-proc.time()
coc.staplestest<-foreach(i= 1:nrow(coc.staples)) %dopar%{
  topsupplierrules.fun(coc.staples[i,],"coc",i)
}
proc.time()-t
stopCluster(cl)

coc.staplestest


coc.staplessummary<-vector('list',length=length(coc.staplestest)
)
for(i in 1:length(coc.staplestest)){
  dummytable<-tbl_df(coc.staplestest[[i]])
  updatetable<- dummytable %>% select(V7,V4,V12) %>% group_by(V7,V12) %>% summarise(sum=sum(as.numeric(V4))) %>% arrange(desc(sum))
  coc.staplessummary[[i]]<-as.data.frame(updatetable)
  }

for(i in 1:length(coc.staplessummary)){
toptax.staples[i]<-coc.staplessummary[[i]][1,1]
}
toptax.staples
