#############################################################??????????????????????????
############3
#############################################################
library(maptools)               #load package 
library(RColorBrewer)
library(rgdal) 
x<-read.csv("ACS_13_5YR_DP03_with_ann.csv",head=FALSE,skip=2,stringsAsFactors = FALSE)#load the data
y<-read.csv("ACS_13_5YR_DP04_with_ann.csv",head=FALSE,skip=2,stringsAsFactors = FALSE)
x
y
head(x)
n<-data.frame(x[2],x[3],x[22],y[186],y[230],y[306])     #get a dataframe including the useful data

colnames(n)<-c("geID","census","unemployment","housing tenure","no vehicles","low occupancy")

head(n)

n$unemployment<-as.numeric(n$unemployment)               #transfer data type
n$`housing tenure`<-as.numeric(n$`housing tenure`)
n$`no vehicles`<-as.numeric(n$`no vehicles`)
n$`low occupancy`<-as.numeric(n$`low occupancy`)
n$`low occupancy`<-100-n$`low occupancy`

par(mfrow=c(2,2))
hist(n$unemployment)                                     #draw histgram
hist(n$`housing tenure`)
hist(n$`no vehicles`)
hist(n$`low occupancy`)

summary(n$unemployment,na.rm=TRUE)                       #get the mean,median,max,min.
summary(n$`housing tenure`,na.rm=TRUE)
summary(n$`no vehicles`,na.rm=TRUE)
summary(n$`low occupancy`,na.rm=TRUE)

sd(n$unemployment,na.rm=TRUE)
sd(n$`housing tenure`,na.rm=TRUE)
sd(n$`no vehicles`,na.rm=TRUE)
sd(n$`low occupancy`,na.rm=TRUE)

n$unemployment[n$unemployment=="-"]<-NA                  #replace - by NA
n$`housing tenure`[n$`housing tenure`=="-"]<-NA
n$`no vehicles`[n$`no vehicles`=="-"]<-NA
n$`low occupancy`[n$`low occupancy`=="-"]<-NA
##################################################################
###########4
#####################################################################
n.rectangle<-n[complete.cases(n),]                      #get rid of the NAs
nrow(n)
nrow(n.rectangle)
percent.census.no.data<-((nrow(n)-nrow(n.rectangle))/nrow(n)) #which is the percentage os the missing data
####################################################################
###########5
##################################################################
data.q5<-data.frame(n$unemployment,n$`housing tenure`,n$`no vehicles`,n$`low occupancy`)
pairs(data.q5)             #draw the scatter matrics.
plot(n)

dev.off()
n.unemp<-log(n.rectangle$unemployment+1)                 #transform data
n.oc<-log(n.rectangle$`low occupancy`+1)
n.rent<-log(n.rectangle$`housing tenure`+1)
n.car<-(n.rectangle$`no vehicles`)^0.5

data.q5.2<-data.frame(n.unemp,n.oc,n.rent,n.car)      # draw the scatter matrics after transforming.
pairs(data.q5.2)

n.unemp1<-log(n$unemployment+1)                      #calculate the data according to Townsend defination
n.oc1<-log(n$`low occupancy`+1)
n.rent1<-log(n$`housing tenure`+1)
n.car1<-(n$`no vehicles`)^0.5

n.next<-data.frame(n.unemp,n.oc,n.rent,n.car)
plot(n.next)
n.next

#####################################################################
###########6
########################################################################
n.unemp.gm<-mean(n.unemp,na.rm=TRUE)         #compute the Townsend index
n.rent.gm<-mean(n.rent,na.rm=TRUE)
n.car.gm<-mean(n.car,na.rm=TRUE)
n.oc.gm<-mean(n.oc,na.rm=TRUE)
n.unemp.std<-sd(n.unemp,na.rm=TRUE)
n.rent.std<-sd(n.rent,na.rm=TRUE)
n.car.std<-sd(n.car,na.rm=TRUE)
n.oc.std<-sd(n.oc,na.rm=TRUE)

n.unemp.mean<-mean(n.rectangle$unemployment)
n.unemp.mean2<-mean(n$unemployment)

z.unemp<-(n.unemp1-n.unemp.gm)/n.unemp.std
z.rent<-(n.rent1-n.rent.gm)/n.rent.std
z.car<-(n.car1-n.car.gm)/n.car.std
z.oc<-(n.oc1-n.oc.gm)/n.oc.std

Townsend<-z.unemp+z.rent+z.car+z.oc
n.next.next<-data.frame(n.next,Townsend)     #with the original 4 variables and Townsend
plot(n.next.next)
townsend.data<-data.frame(n$census,n$geID,Townsend)
townsend.data[which.max(Townsend),]          #find the biggest and the smallest index 
townsend.data[which.min(Townsend),]

#####################################################################
###########7 ?????????margins of error
#####################################################################


###################################################################
###########8
#####################################################################
NY.map <- readOGR(dsn="New york map", layer="tl_2013_36_tract")   #load map data
plot(NY.map)
townsend.data<-data.frame(n$census,Townsend)
rownames(townsend.data)<-n$geID
row.names(NY.map) <- as.character(NY.map$GEOID)
NYC.map <- NY.map[is.element(row.names(NY.map), n$geID),]#extract Newyorkcounty data
plot(NYC.map)

row.names(NYC.map)<-as.character(NYC.map@data$GEOID) 
row.names(townsend.data)<-n$geID
townsend.data<-townsend.data[row.names(NYC.map),]   
NYC.map <- spCbind(NYC.map, townsend.data)     #attach townsend index to map

range(Townsend, na.rm=TRUE)

plot.variable <- townsend.data$Townsend
breaks.factor <- cut(plot.variable, breaks=seq(from=-14, to=6, length=9))
levels(breaks.factor)  
plot.variable[1:10]    
breaks.factor[1:10]     
table(breaks.factor)                           
length(levels(breaks.factor)) 
color.palette <- rev(brewer.pal(n=length(levels(breaks.factor)),"Spectral"))
color.coding <- color.palette[as.numeric(breaks.factor)]
display.brewer.pal(length(levels(breaks.factor)), "Spectral")    #prepare the color using rev to reverse

plot(NYC.map, col=color.coding)
legend("bottomleft", legend=c(levels(breaks.factor), "no data","lowinstein"), 
       fill=c(color.palette, "white","black"), cex=1.2, bty="n", y.intersp=1.2, ncol=2)
title("The deprivation situation of NYC")                         #add title here
####################################################################
##########9
######################################################################
points(coordinates(NYC.map["36061014500",]),cex=1.2,pch=19)      #mark the Lowenstein
legend("bottomleft", legend=c(levels(breaks.factor), "no data","lowinstein"), 
       fill=c(color.palette, "white","black"), cex=1.2, bty="n", y.intersp=1.2, ncol=2)
cd<-coordinates(NYC.map["36061014500"])
arrows(x0=75,y0=45,x1=-73.98426,y1=40.76981,length=0.2,lwd=2)


rank.data<-data.frame(townsend.data$n.geID,townsend.data$Townsend)     #compute the rank
rank.data<-rank.data[order(-rank.data$townsend.data.Townsend),]
which(rank.data$townsend.data.n.geID==36061014500)                              #as the rank 1 for the most deprived

########################################################################
###########
#########################################################################
#explanation is in the solution attached.


