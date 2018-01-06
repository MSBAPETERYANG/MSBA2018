library(stringr)
library(reshape2)
library(igraph)
library(zoo)
library(plotly)
library(cluster)
library(network)
library(mclust)
library(data.table)

setwd("C:/Users/szyan/Downloads")
hw3data1<-read.csv(file = "Funding_events_7.14.csv")
hw3data2<-read.csv(file = "Funding_events_7.14_page2.csv")
colnames(hw3data2)[1]<-"Portfolio.Company.Name"
hw3data<-rbind(hw3data1,hw3data2)

hw3data$Deal.Date <- as.Date(hw3data$Deal.Date,format='%m/%d/%y')
hw3data$nmonth<-(as.yearmon(hw3data$Deal.Date, format = '%m/%d/%y')-as.yearmon(strptime("06/01/1981", format = '%m/%d/%Y')))*12
investors<-as.data.frame(str_split_fixed(hw3data[,11],", ",Inf))
investors$numon<-hw3data$nmonth
investors<-subset(investors,investors$V2!="")
investors[investors==""]<-NA
investors[investors=="Inc."]<-NA
investors[investors=="Ltd."]<-NA
investors[investors=="LLC"]<-NA
investors[investors=="Inc"]<-NA


investadj<-function(x){
  orgmatrix<-matrix(data=1,nrow=length(x[!is.na(x)]),ncol = length(x[!is.na(x)]))
  rownames(orgmatrix)=x[!is.na(x)]
  colnames(orgmatrix)=x[!is.na(x)]
  diag(orgmatrix)=0
  invmatrix=graph_from_adjacency_matrix(orgmatrix)
  listedge=as.matrix(get.edgelist(invmatrix))
  return(listedge)
}
#question1------------------------------------------------------

l<-c()
for(i in 1:nrow(investors)){
  l[[i]]<-investadj(investors[i, 1:27])
}


invest_edge = do.call(rbind, l)
inv_graph<-graph.data.frame(invest_edge)
which.max(degree(inv_graph))
#according to degree centrality, New Enterprise Associates is the center of network.
which.max(closeness(inv_graph))
#according to closeness centrality, Intel Capital is the center of network.
which.max(betweenness(inv_graph))
#according to betweenness centrality, Intel Capital Associates is the center of network.
mean_distance(inv_graph)
#Average shortest path length is 3.44524
#The average shortest path length is high could be a result of grouping behavior.
#Firms forms long-term co-investment relationship, while there is few firms that is able to bridge different co-investment groups.
#As a result, it takes a few more steps to get to these few connectors and get to firms in different groups.

#question2------------------------------------------------------
library(stringr)
library(reshape2)
library(igraph)
library(zoo)

setwd("C:/Users/szyan/Downloads")
hw3data1<-read.csv(file = "Funding_events_7.14.csv")
hw3data2<-read.csv(file = "Funding_events_7.14_page2.csv")
colnames(hw3data2)[1]<-"Portfolio.Company.Name"
hw3data<-rbind(hw3data1,hw3data2)

hw3data$Deal.Date <- as.Date(hw3data$Deal.Date,format='%m/%d/%y')
hw3data$nmonth<-round((as.yearmon(hw3data$Deal.Date, format = '%m/%d/%y')-as.yearmon(strptime("06/01/1981", format = '%m/%d/%Y')))*12,digits = 0)
mon<-as.data.frame(seq(1:397))
colnames(mon)<-'V3'
investors<-as.data.frame(str_split_fixed(hw3data[,11],", ",Inf))
investors$nmonth<-hw3data$nmonth
investors<-subset(investors,investors$V2!="")
investors[investors==""]<-NA


investadj<-function(x){
  orgmatrix<-matrix(data=1,nrow=length(x[!is.na(x)]),ncol = length(x[!is.na(x)]))
  rownames(orgmatrix)=x[!is.na(x)]
  colnames(orgmatrix)=x[!is.na(x)]
  diag(orgmatrix)=0
  invmatrix=graph_from_adjacency_matrix(orgmatrix,mode="undirected")
  listedge=as.matrix(get.edgelist(invmatrix))
  return(listedge)
}

l<-c()
for(i in 1:nrow(investors)){
  l[[i]]<-cbind(investadj(investors[i, 1:27]),investors$nmonth[i])
}

invest_edge = do.call(rbind, l)
invest_edge<-as.data.frame(invest_edge)

#invest_edge<-merge(mon,invest_edge,by='V3',all=TRUE)

invest_edge<-invest_edge[order(as.numeric(as.character(invest_edge$V3))),]

invest_edge<-as.data.frame(invest_edge)


average_coreness<-c()

for(i in 2:nrow(invest_edge)){
  if(invest_edge$V3[i]!=invest_edge$V3[i-1]){
    average_coreness[[i]]<-as.numeric(mean(coreness(graph.data.frame(invest_edge[1:i,]))))}
  
}

Months<-c()

for(j in 2:nrow(invest_edge)){
if(invest_edge$V3[j]!=invest_edge$V3[j-1]){
Months[j]<-as.character(invest_edge[j,3])}
}

Months<-as.numeric(Months)
mon<-as.data.frame(seq(1:397))
colnames(mon)<-'Months'
a<-as.data.frame(cbind(Months,average_coreness))
c<-merge(mon,a,by='Months',all=TRUE)
c<-na.omit(c)
plot(c$Months,c$average_coreness,type='l')

#QUESTION3-------------------------

invest_edge$V3<-as.numeric(as.character(invest_edge$V3))

prev_month<-function(x){
  for(k in 1:nrow(invest_edge)){
  if(invest_edge$V3[k]==x-120){
    return(as.integer(k))
  }
    if(x-120<0){
      k=1
      return(as.integer(k))
    }
  }
}



average_coreness<-c()

for(i in 2:nrow(invest_edge)){
  if(invest_edge$V3[i]!=invest_edge$V3[i-1]){
    average_coreness[[i]]<-as.numeric(mean(coreness(graph.data.frame(invest_edge[prev_month(invest_edge$V3[i]):i,]))))}
}

Months<-c()

for(j in 2:nrow(invest_edge)){
  if(invest_edge$V3[j]!=invest_edge$V3[j-1]){
    Months[j]<-as.character(invest_edge[j,3])}
}



Months<-as.numeric(Months)
mon<-as.data.frame(seq(1:397))
colnames(mon)<-'Months'
a<-as.data.frame(cbind(Months,average_coreness))
c<-merge(mon,a,by='Months',all=TRUE)
c<-na.omit(c)
plot(c$Months,c$average_coreness,type='l')

#Question4----------------------------------------------------------------------
hw3data<-subset(hw3data, hw3data$Deal.Date<"1991-06-30")
investors<-as.data.frame(str_split_fixed(hw3data[,11],", ",Inf))

invest<-as.data.frame(hw3data[,1])
colnames(invest)<-"invested"

newdata<-cbind(investors,invest)
newdata<-subset(newdata,newdata$V2!="")
cleandata<-melt(newdata,id="invested")
cdata<-subset(cleandata,select = -variable)
c1data<-unique(subset(cdata,cdata$value!=""))

mdata<-xtabs(~ as.character(invested)+ as.character(value),c1data)
attr(mdata,'class')<-NULL
attr(mdata,'call')<-NULL
mdata<-as.matrix(mdata)

cmdata<-t(mdata)%*%mdata

cmdata<-as.matrix(cmdata)

concor = list()
concor[[1]] = cmdata

for(i in 2:10){
  concor[[i]] = cor(concor[[i - 1]])
}


concor_ul = do.call(rbind, concor)

concor_ul = round(concor_ul, 3)

write.csv(concor_ul, "concor.csv")

concor[[10]][concor[[10]] < 0] = 0

concor_net = graph.adjacency(as.matrix.network(network(concor[[10]])), "undirected")
V(concor_net)$color = "light blue"
plot.igraph(concor_net, vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size=5,vertex.color="light blue")

#Question5----------------------------------------------------------------------
plot.igraph(inv_graph,vertex.label=NA,vertex.label.color="black",edge.color="black",vertex.size = 5, edge.arrow.size=.3,edge.curved=FALSE)
#According to the co-investment network graph, venture capital tends to exhibit more of a core-periphery structure. 
#A large amount of firms crowd in the center and connect with each other, while other firms locate in the periperal area & disconnect with the central core group.