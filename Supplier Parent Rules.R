# Investigate Top matching Supplier Parents between clients #
#Author: Mike Zizzi

library(dplyr)

#import data sets
load("~/mlearn jaggaer/Jaggaer_machine-learning/coc_ex_vanilla.RData")
load("~/mlearn jaggaer/Jaggaer_machine-learning/lib_vanilla.RData")
load("~/mlearn jaggaer/vanilla_westonex.RData")

coc.vanilla<-coc.ex.vanilla
west.vanilla<-vanilla.westonex
rm(coc.ex.vanilla,vanilla.westonex)

#Find Top pairwise supplier parents between clients

topsuppliers.coc<- coc.vanilla %>% group_by(Supplier.Parent.Name) %>% count() %>% arrange(desc(n))
topsuppliers.west<- west.vanilla %>% group_by(Supplier.Parent.Name) %>% count() %>% arrange(desc(n))
topsuppliers.lib<- lib.vanilla %>% group_by(Supplier.Parent.Name) %>% count() %>% arrange(desc(n))


pairwise.coc_west<-topsuppliers.coc[which(topsuppliers.coc$Supplier.Parent.Name %in% topsuppliers.west$Supplier.Parent.Name),]
pairwise.coc_lib<-topsuppliers.coc[which(topsuppliers.coc$Supplier.Parent.Name %in% topsuppliers.lib$Supplier.Parent.Name),]

pairwise.west_coc<-topsuppliers.west[which(topsuppliers.west$Supplier.Parent.Name %in% topsuppliers.coc$Supplier.Parent.Name),]
pairwise.west_lib<-topsuppliers.west[which(topsuppliers.west$Supplier.Parent.Name %in% topsuppliers.lib$Supplier.Parent.Name),]

pairwise.lib_west<-topsuppliers.lib[which(topsuppliers.lib$Supplier.Parent.Name %in% topsuppliers.west$Supplier.Parent.Name),]
pairwise.lib_coc<-topsuppliers.lib[which(topsuppliers.lib$Supplier.Parent.Name %in% topsuppliers.coc$Supplier.Parent.Name),]

#Pull top N Suppliers from each to store in a client specific repository


suppliers.coc <- unique(rbind(pairwise.coc_lib[1:100,],pairwise.coc_west[1:100,]))  %>% arrange(desc(n))

suppliers.west<- unique(rbind(pairwise.west_lib[1:100,],pairwise.west_coc[1:100,])) %>% arrange(desc(n))

suppliers.lib <- unique(rbind(pairwise.lib_coc[1:100,],pairwise.lib_west[1:100,]))  %>% arrange(desc(n))


#Parse ALL rules from a client and store supplier parent in a vector
  #match vector with top supplier list for a client
vrules.coc<-read.csv("Charleston_Rules_Export.csv",stringsAsFactors = FALSE)[,c(1,2,9,10,11,12,13,14)]
vrules.lib<-read.csv("Liberty_Rules_Export.csv",stringsAsFactors = FALSE)[,c(1,2,9,10,11,12,13,14)]
vrules.west<-read.csv("westonrules.csv",stringsAsFactors = FALSE)[,c(1,2,9,10,11,12,13,14)]

#Rule parsing function
rulegrabber<-function(clientrules,clientvanilla,client){
  rules.parsed<-vector('list',length=nrow(clientrules))
  for (i in 1:nrow(clientrules)){
    
    if(length(grep("Supplier Parent Name Contains ",apply(clientrules[i,],1, as.character)))!=0){
      rules.parsed[[i]][[1]]<-clientrules[i,1]
      
      rules.parsed[[i]][[2]]<-gsub("-","",as.character(clientvanilla$Category.Code[match(clientrules[i,1],clientvanilla$Rule.ID)]))
      
      rules.parsed[[i]][[3]]<-clientrules[i,2]
      rules.parsed[[i]][[4]]<-client
      rules.parsed[[i]][[5]]<-gsub("Supplier Parent Name Contains ","",clientrules[i,(grep("Supplier Parent Name Contains ",value=FALSE,apply(clientrules[i,],1, as.character))[1])]) #will break if more than one supplier parent rule
      for (j in 1:6){
      
        if((!is.null(clientrules[i,j+2])) && (!grepl("Supplier Parent Name Contains ", as.character(clientrules[i,j+2])))){
          rules.parsed[[i]][[j+5]]<-as.character(clientrules[i,j+2])
        }
      }
      
    }
    
  }
  rules.parsed<-rules.parsed[!sapply(rules.parsed, is.null)]
  return(rules.parsed)
}

#Run the function

mrules.coc<-rulegrabber(vrules.coc,coc.vanilla,"coc")
mrules.west<-rulegrabber(vrules.west,west.vanilla,"west")
mrules.lib<-rulegrabber(vrules.lib,lib.vanilla,"lib")

#Create parent vector then filter %in% top suppliers
prules.coc <-sapply(mrules.coc, "[[",5)
prules.west<-sapply(mrules.west,"[[",5)
prules.lib <-sapply(mrules.lib, "[[",5)

index.coc <-which(prules.coc %in% c(sapply(suppliers.west, as.character),sapply(suppliers.lib,as.character)))
index.west<-which(prules.west %in% c(sapply(suppliers.coc, as.character),sapply(suppliers.lib,as.character)))
index.lib <-which(prules.lib %in% c(sapply(suppliers.west, as.character),sapply(suppliers.coc,as.character)))

frules.coc<-mrules.coc[index.coc]
frules.west<-mrules.west[index.west]
frules.lib<-mrules.lib[index.lib]

#Clean rules

rulecleaner<-function(rulelist){
  for (i in 1:length(rulelist)){
    for (j in 1:length(rulelist[[i]])){
      if(grepl('Is Blank|Is Not Blank',rulelist[[i]][j+4])){
        rulelist[[i]]<-rulelist[[i]][-(j+4)]
      }
      else{}
    }
  }
  
  for (i in 1:length(rulelist)){
    for (j in 1:length(rulelist[[i]])){
      if(grepl('Is Blank|Is Not Blank',rulelist[[i]][j+4])){
        rulelist[[i]]<-rulelist[[i]][-(j+4)]
      }
      else{}
    }
  }
  
  for (i in 1:length(rulelist)){
    for (j in 1:length(rulelist[[i]])){
      if(grepl('Contains',rulelist[[i]][j+3])){
        dummy<-strsplit(as.character(rulelist[[i]][j+3]),"Contains ")
        rulelist[[i]][j+3]<-dummy[[1]][2]
      }
      else{}
    }
    
  }  
  
  for (i in 1:length(rulelist)){
    for (j in 1:length(rulelist[[i]])){
      if(grepl('Equals ',rulelist[[i]][j+3])){
        dummy<-strsplit(as.character(rulelist[[i]][j+3]),"Equals ")
        rulelist[[i]][j+3]<-dummy[[1]][2]
      }
      else{}
    }
    
  } 
  return(rulelist)
}

frules.coc<-rulecleaner(frules.coc)
frules.west<-rulecleaner(frules.west)
frules.lib<-rulecleaner(frules.lib)

#save(frules.coc,file="frules_coc.RData")
#save(frules.west,file="frules_west.RData")
#save(frules.lib,file="frules_lib.RData")



c.staples<-frules.coc [which(sapply(frules.coc, "[[",5)=="STAPLES INC")]
w.staples<-frules.west[which(sapply(frules.west,"[[",5)=="STAPLES INC")]
l.staples<-frules.lib [which(sapply(frules.lib, "[[",5)=="STAPLES INC")]
staples<-append(c.staples,w.staples)
staples<-append(staples,l.staples)


