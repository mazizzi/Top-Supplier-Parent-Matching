vrules.lib<-read.csv("Liberty_Rules_Export.csv",stringsAsFactors = FALSE)[,c(1,2,9,10,11,12,13,14)]
z<-apply(vrules.lib[grep("APPLE COMPUTER INC",vrules.lib[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
z<-apply(z,2,function(x) gsub(".* Contains ","",x))
z<-apply(z,2,function(x) gsub(".*Description Is Blank","",x))
z<-apply(z,2,function(x) gsub(".*Equals ","",x))
z


sum(as.numeric(west.vanilla$Spend.Amount[which(west.vanilla$Supplier.Parent.Name=="STAPLES")]))/sum(as.numeric(west.vanilla$Spend.Amount),na.rm = TRUE)
