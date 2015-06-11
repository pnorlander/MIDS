dif.groups<-function(el,out)
{
require(data.table)
require(igraph)

bel2mel.f<-function(
	db2bel=NULL
	,type=c("utel","crel")
)
{
	require(data.table)
	if(ncol(db2bel)>2) stop('db2bel must be a bimodal edgelist as a two column data.table. Selection on pendants, etc. should be made prior to passing to bel2mel.f.')

	setnames(db2bel,c('ut','cr'))
	db2bel[,ut:=as.character(ut)]
	db2bel[,cr:=as.character(cr)]

	bel2mel<-list()

	if('utel'%in%type){
		setkey(db2bel,ut,cr)
		utd<-db2bel[,.N,by=ut]
		setkey(utd,N,ut)
		utiso<-utd[list(1),ut]
		bel2mel$utel<-db2bel[!list(utiso),list(cr=combn(cr,m=2,FUN=sort,simplify=F)),by=ut]
		bel2mel$utel[,`:=`(cr1=sapply(cr,function(x) x[1]),cr2=sapply(cr,function(x) x[2]))]
		bel2mel$utel[,cr:=NULL]
		bel2mel$utel<-bel2mel$utel[,list(ew=.N,ut=list(ut)),keyby=c('cr1','cr2')]
	}
	bel2mel
}

el3<-read.table(el,sep='\t',header=T)
el3<-data.table(el3[!grepl('[A-Z]$',el3$group),])

mel3<-bel2mel.f(el3[,2:1,with=F])

net3<-graph.edgelist(as.matrix(mel3$utel[,list(cr1,cr2)]),directed=F)
E(net3)$weight<-mel3$utel[,-ew]
l<-layout.fruchterman.reingold(net3,weights=-E(net3)$weight)
sg<-replicate(100,spinglass.community(net3,spins=3,implementation='neg'),simplify=F)

par(mai=c(0.1,0.1,0.1,0.1))
pdf(out,height=5,width=5)
plot.communities(
sg[[which.max(sapply(sg,modularity,weights=E(net3)$weight))]]
,net3
,layout=l
,edge.width=-E(net3)$weight
)
graphics.off()
ret<-sg[[which.max(sapply(sg,modularity,weights=E(net3)$weight))]]
ret
}

setwd('path/to/this/repo/folder/difgroups')

dif.groups(el='s3el.txt'
,out='groups3.pdf'
)

dif.groups(
el='s4el.txt'
,out='groups4.pdf'
)