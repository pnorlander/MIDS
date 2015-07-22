dif.groups<-function(el,out,mx.grp,type=c('same','different'))
{
	source('~/Dropbox/GitHub/knowledge-survival/dissertation_source.R')
	require(data.table)
	require(igraph)

	mel<-bel2mel.f(el)

	net<-graph.edgelist(as.matrix(mel$utel[,list(cr1,cr2)]),directed=F)
	E(net)$weight<-mel$utel[,ew]*ifelse(type[1]=='different',-1,1)
	l<-layout.fruchterman.reingold(net,weights=abs(E(net)$weight))
	sg<-replicate(100,spinglass.community(net,spins=mx.grp,implementation='neg'),simplify=F)

	par(mai=c(0.1,0.1,0.1,0.1))
	pdf(out,height=3,width=3)
	plot.communities(
		sg[[which.max(sapply(sg,modularity,weights=E(net)$weight))]]
		,net
		,layout=l
		,edge.width=-E(net)$weight
	)
	dev.off()
	if(type[1]=='same') ret<-sg[[which.max(sapply(sg,modularity,weights=E(net)$weight))]]
	if(type[1]=='different') ret<-sg[[which.min(sapply(sg,modularity,weights=E(net)$weight))]]
	ret
}

if(F){
	el<-read.table('~/Dropbox/Summer 2015/MIDS/RDADA/s3el.txt',sep='\t',header=T)
	el<-data.table(el[!grepl('[A-Z]$',el$group),])
	el<-el[,2:1,with=F]

	dif.groups(el=el
						 ,out='~/Dropbox/Summer 2015/MIDS/RDADA/6/Section 3/groups3.pdf'
						 ,mx.grp=3
						 ,type='different'
	)

	el<-read.table('~/Dropbox/Summer 2015/MIDS/RDADA/s4el.txt',sep='\t',header=T)
	el<-data.table(el[!grepl('[A-Z]$',el$group),])
	el<-el[,2:1,with=F]

	dif.groups(
		el=el
		,out='~/Dropbox/Summer 2015/MIDS/RDADA/6/Section 4/groups4.pdf'
		,mx.grp=3
		,type='different'
	)
}

library(data.table)
el<-read.table('/Users/bambrose/Dropbox/Summer 2015/SOC1Online/admin/Soc1RosterSummer2015.tab.txt',sep='\t',header=T)
el<-rbindlist(
	list(data.table(el$Major,el$E.mail,el$Section)
			 ,data.table(el$Classification,el$E.mail,el$Section))
	,use.names=F,fill=F
)
setkey(el,V3)

A<-try(dif.groups(
	el=el['A',-3,with=F]
	,out='~/Dropbox/Summer 2015/SOC1Online/3/A-dif-group.pdf'
	,mx.grp=6
	,type='different'
))
B<-try(dif.groups(
	el=el['B',-3,with=F]
	,out='~/Dropbox/Summer 2015/SOC1Online/3/B-dif-group.pdf'
	,mx.grp=6
	,type='different'
))
C<-try(dif.groups(
	el=el['C',-3,with=F]
	,out='~/Dropbox/Summer 2015/SOC1Online/3/C-dif-group.pdf'
	,mx.grp=6
	,type='different'
))
if(F) {write.table(
	rbind(
		membership(A)[order(names(membership(A)))]
		,membership(B)[order(names(membership(B)))]
		,membership(C)[order(names(membership(C)))]
	)
)}

library(igraph)
els<-lapply(decompose.graph(graph.edgelist(as.matrix(el[,-3,with=F]),directed=T)),get.edgelist)
if(F){
	ABC<-list()
	for(i in 1:length(els)){
		ABC[[i]]<-try(dif.groups(
			el=data.table(els[[i]])
			,out=paste('~/Dropbox/Summer 2015/SOC1Online/3/ABC',i,'-dif-group.pdf',sep='')
			,mx.grp=23
			,type='different'
		))}
}
res<-list()
ABC<-list(A,B,C)
for(i in 1:length(ABC)){
	res[[i]]<-cbind(names(membership(ABC[[i]])),paste(i,'-',sapply(2-nchar(membership(ABC[[i]])),function(x) paste0(rep('0',times=x),collapse='')),membership(ABC[[i]]),sep=''))
}
res<-do.call(rbind,res)
try(res<-rbind(res,cbind(unique(els[[3]][,2]),'')))
res<-res[order(res[,1]),]
apply(res,1,function(x) {cat(x,sep='\t');cat('\n')})

apply(matrix(sample(c('Rat', 'Ox', 'Tiger', 'Rabbit', 'Dragon', 'Snake', 'Horse', 'Goat', 'Monkey', 'Rooster', 'Dog', 'Pig')),ncol=2),1,function(x) {cat(x,sep='\t');cat('\n')})
