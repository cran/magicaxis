magaxis <-
function(side,n=5,tcl=0.5,ratio=0.5,labels=TRUE,unlog=FALSE,tline=0.5,mtline=2,xlab,ylab,box=FALSE,...){
if(missing(side)){side=1:4}
unloglist=unlog
if(length(unlog)==1 & length(side)>1 & (unlog[1]==T | unlog[1]==F)){unloglist=rep(unlog,length(side))}
if(unlog[1]=='x'){unloglist=rep(FALSE,length(side));unloglist[side %in% c(1,3)]=TRUE}
if(unlog[1]=='y'){unloglist=rep(FALSE,length(side));unloglist[side %in% c(2,4)]=TRUE}
if(unlog[1]=='xy' | unlog[1]=='yx'){unloglist=rep(TRUE,length(side))}

for(i in 1:length(side)){
		currentside=side[i]
		unlog=unloglist[i]
  		lims=par("usr")
  		if(currentside %in% c(1,3)){
  		lims=lims[1:2];if(par('xlog')){logged=T}else{logged=F}
  		}else{
  		lims=lims[3:4];if(par('ylog')){logged=T}else{logged=F}
  		}

  		major.ticks = pretty(lims,n=5)
	
  		if(labels & (logged | unlog)){uselabels = sapply(major.ticks,function(i){as.expression(bquote(10^ .(i)))})}
 		if(labels & (logged | unlog)==F){uselabels = major.ticks}
 		
 		if(logged){
 			axis(currentside,at=10^major.ticks,tcl=tcl,labels=FALSE,...)
 			if(labels){mtext(text=uselabels[major.ticks>=lims[1] & major.ticks<=lims[2]],side=currentside,at=10^major.ticks[major.ticks>=lims[1] & major.ticks<=lims[2]],line=tline)}
 			}else{
  			axis(currentside,at=major.ticks,tcl=tcl,labels=FALSE,...)
  			if(labels){mtext(text=uselabels[major.ticks>=lims[1] & major.ticks<=lims[2]],side=currentside,at=major.ticks[major.ticks>=lims[1] & major.ticks<=lims[2]],line=tline)}
  		}
  		if(logged | unlog){minors = log10(pretty(10^major.ticks[1:2],n+2))-major.ticks[1]}else{minors = pretty(major.ticks[1:2],n+2)-major.ticks[1]}
  		minors = minors[-c(1,length(minors))]
		minor.ticks = c(outer(minors,major.ticks,`+`))
		if(logged){axis(currentside,at=10^minor.ticks,tcl=tcl*ratio,labels=FALSE)}else{
		axis(currentside,at=minor.ticks,tcl=tcl*ratio,labels=FALSE)
		}
    if(missing(xlab)==F & currentside==1){title(xlab=xlab,line=mtline)}
    if(missing(ylab)==F & currentside==2){title(ylab=ylab,line=mtline)}
}
if(box){box()}
}
