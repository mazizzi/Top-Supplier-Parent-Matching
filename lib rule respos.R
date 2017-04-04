
mikesfunction<-function(parentname){
  x<-apply(vrules.lib[grep(parentname,vrules.lib[,3]),],2,function(x) gsub("Supplier Parent Name Contains ","",x))
  if(length(x)==0){}
  else if(is.null(dim(x))){
    x<-sapply(x,function(x) gsub(".* Contains ","",x))
    x<-sapply(x,function(x) gsub(".*Description Is Blank","",x))
    x<-sapply(x,function(x) gsub(".*Equals ","",x))
    x[8]<-gsub("-","",as.character(lib.vanilla$Category.Code[which(lib.vanilla$Rule.ID==x[1])][1])) 
    xf<-matrix(x,ncol=8)
    return(xf)
  }
  
  else{
    x<-apply(x,2,function(x) gsub(".* Contains ","",x))
    x<-apply(x,2,function(x) gsub(".*Description Is Blank","",x))
    x<-apply(x,2,function(x) gsub(".*Equals ","",x))
    for(i in 1:nrow(x)){
      x[i,8]<-gsub("-","",as.character(lib.vanilla$Category.Code[which(lib.vanilla$Rule.ID==x[i,1])][1]))
    }
    xf<-matrix(x,ncol=8)
    return(xf)}
 
}
#############################################################################################

ulti.travel.repo.lib<-matrix(ncol=8)
x1<-mikesfunction("HILTON WORLDWIDE")
x2<-mikesfunction("US AIRWAYS")
x3<-mikesfunction("INTERCONTINENTAL HOTELS GROUP")
x4<-mikesfunction("NATIONAL CAR RENTAL")
x5<-mikesfunction("AMERICAN AIRLINES")
x6<-mikesfunction("DELTA AIRLINES")
x7<-mikesfunction("BCD TRAVEL")
x8<-mikesfunction("BESTlibERN")
x9<-mikesfunction("CARLSON HOTELS")
x10<-mikesfunction("UNITED RENTALS")
x11<-mikesfunction("AMTRAK")
x12<-mikesfunction("KIRKLEY HOTEL")
x13<-mikesfunction("RED ROOF INN")
x14<-mikesfunction("NATIONAL CAR TOLLS")
x15<-mikesfunction("MARRIOTT INTERNATIONAL INC")
x16<-mikesfunction("HERTZ CORP")
x17<-mikesfunction("HOTELS.COM")
x18<-mikesfunction("JETBLUE AIRWAYS")
x19<-mikesfunction("EMIRATES AIRLINES")
x20<-mikesfunction("ALASKA AIR")

ulti.travel.repo.lib<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,
                                         x17,x18,x19,x20))
rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
#############################################################################################

ulti.food.repo.lib<-matrix(ncol=8)
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
x13<-mikesfunction("KROGER CO")
x14<-mikesfunction("OUTBACK STEAKHOUSE")
x15<-mikesfunction("WHOLE FOODS MARKET")
x16<-mikesfunction("CHICK-FIL-A")
x17<-mikesfunction("PAPA JOHNS PIZZA")
x18<-mikesfunction("BUFFALO WILD WINGS")
x19<-mikesfunction("JIMMY JOHN S")
x20<-mikesfunction("ZAXBYS FRANCHISING INC")
x21<-mikesfunction("OLIVE GARDEN")
x22<-mikesfunction("SUBWAY SANDWICHES")
x23<-mikesfunction("RED ROBIN")
x24<-mikesfunction("GOLDEN CORRAL")
x25<-mikesfunction("RED LOBSTER")
x26<-mikesfunction("APPLEBEES")
x27<-mikesfunction("RUBY TUESDAYS")
x28<-mikesfunction("BOJANGLES")
x29<-mikesfunction("YUM! BRANDS INC")
x30<-mikesfunction("JERSEY MIKES SUBS")
x31<-mikesfunction("CHEESECAKE FACTORY")
x32<-mikesfunction("LITTLE CAESARS ENTERPRISES INC")
x33<-mikesfunction("CHILIS GRILL")
x34<-mikesfunction("PUBLIX SUPERMARKETS INC")
x35<-mikesfunction("FOOD LION")
x36<-mikesfunction("MELLOW MUSHROOM")
ulti.food.repo.lib<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,
                                       x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,
                                       x29,x30,x31,x32,x33,x34,x35,x36))
rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,
   x32,x33,x34,x35,x36)


#############################################################################################
ulti.tech.repo.lib<-matrix(ncol=8)

x1<-mikesfunction("APPLE COMPUTER INC")
x2<-mikesfunction("DROPBOX")
x3<-mikesfunction("GOOGLE INC")
x4<-mikesfunction("BEST BUY")
x5<-mikesfunction("DELL INC")
x6<-mikesfunction("B & H PHOTO VIDEO INC")
x7<-mikesfunction("VERIZON WIRELESS SERVICES LLC")
x8<-mikesfunction("TOSHIBA CORP")
x9<-mikesfunction("AT&T")
x10<-mikesfunction("SHI INTERNATIONAL CORP")
x11<-mikesfunction("ADOBE SYSTEMS INC")
x12<-mikesfunction("NEWEGG INC")

ulti.tech.repo.lib<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12))
rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)


###########################################################################################################
ulti.retail.repo.lib<-matrix(ncol=8)

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
x11<-mikesfunction("HARRIS OFFICE FURNITURE")
x12<-mikesfunction("TARGET CORP")
x13<-mikesfunction("NORTHERN TOOL & EQUIPMENT")
x14<-mikesfunction("DOLLAR TREE")
x15<-mikesfunction("WALGREEN CO")
x16<-mikesfunction("CVS CAREMARK")
x17<-mikesfunction("BED BATH & BEYOND")
x18<-mikesfunction("TJ MAXX")
x19<-mikesfunction("TRUE VALUE HARDWARE")
x20<-mikesfunction("HOBBY LOBBY")
x21<-mikesfunction("DICKS SPORTING GOODS")
x22<-mikesfunction("DISCOUNT SCHOOL SUPPLY")
x23<-mikesfunction("ORIENTAL TRADING CO")
x24<-mikesfunction("SEARS HOLDINGS CORP")
ulti.retail.repo.lib<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,
                                         x16,x17,x18,x19,x20,x21,x22,x23,x24))
rm(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24)

############################################################################################################
ulti.lab.repo.lib<-matrix(ncol=8)

x1<-mikesfunction("VWR INTERNATIONAL INC")
x2<-mikesfunction("GRAINGER INC")
x2<-mikesfunction("BIO-RAD LABS")
x4<-mikesfunction("THERMO FISHER SCIENTIFIC INC")
x5<-mikesfunction("INTEGRATED DNA TECHNOLOGIES INC")
x6<-mikesfunction("EUROFINS SCIENTIFIC SA")
x7<-mikesfunction("LIFETECH")
x8<-mikesfunction("QIAGEN INC")


ulti.lab.repo.lib<-do.call(rbind,list(x1,x2,x3,x4,x5,x6,x7,x8))
rm(x1,x2,x3,x4,x5,x6,x7,x8)

############################################################################################################

ulti.lib.repo<-do.call(rbind,list(ulti.food.repo.lib,ulti.retail.repo.lib,ulti.tech.repo.lib,ulti.travel.repo.lib,ulti.lab.repo.lib))





