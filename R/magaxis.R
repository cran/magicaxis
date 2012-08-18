magaxis <-
function(side=1:4,majorn=5,minorn=5,tcl=0.5,ratio=0.5,labels=TRUE,unlog='Auto',tline=0.5,mtline=2,xlab=NULL,ylab=NULL,box=FALSE,crunch=TRUE,logpretty=TRUE,...){
unloglist=unlog
labelslist=labels
crunchlist=crunch
logprettylist=logpretty
if(length(unlog)==1 & length(side)>1 & (unlog[1]==T | unlog[1]==F | unlog[1]=='Auto')){unloglist=rep(unlog,length(side))}
if(length(labels)==1 & length(side)>1){labelslist=rep(labels,length(side))}
if(length(crunch)==1 & length(side)>1){crunchlist=rep(crunch,length(side))}
if(length(logpretty)==1 & length(side)>1){logprettylist=rep(logpretty,length(side))}
if(unlog[1]=='x'){unloglist=rep(FALSE,length(side));unloglist[side %in% c(1,3)]=TRUE}
if(unlog[1]=='y'){unloglist=rep(FALSE,length(side));unloglist[side %in% c(2,4)]=TRUE}
if(unlog[1]=='xy' | unlog[1]=='yx'){unloglist=rep(TRUE,length(side))}

if(length(unloglist) != length(side)){stop('Length of unlog vector mismatches number of axes!')}
if(length(labelslist) != length(side)){stop('Length of labels vector mismatches number of axes!')}

for(i in 1:length(side)){
		currentside=side[i]
		unlog=unloglist[i]
		labels=labelslist[i]
		crunch=crunchlist[i]
		logprettylist=logpretty[i]
  		lims=par("usr")
  		if(currentside %in% c(1,3)){
  		lims=lims[1:2];if(par('xlog')){logged=T}else{logged=F}
  		}else{
  		lims=lims[3:4];if(par('ylog')){logged=T}else{logged=F}
  		}
        lims=sort(lims)

        if(unlog=='Auto'){if(logged){unlog=T}else{unlog=F}}
	
  		if(labels & unlog){
        sci.tick=maglab(10^lims,n=majorn,log=T,exptext=T,crunch=crunch,logpretty=logpretty)
        major.ticks = log10(sci.tick$at)
  		uselabels = sci.tick$exp
        minors = log10(pretty(10^major.ticks[1:2],minorn+2))-major.ticks[1]
 		}
 		if(labels & logged & unlog==F){
 		sci.tick=maglab(10^lims,n=majorn,log=T,exptext=F,crunch=crunch,logpretty=logpretty)
 		major.ticks = log10(sci.tick$at)
  		uselabels = sci.tick$exp
 		minors = log10(pretty(10^major.ticks[1:2],minorn+2))-major.ticks[1]
 		}
 		if(labels & logged==F & unlog==F){
 		sci.tick=maglab(lims,n=majorn,log=F,exptext=F)
 		major.ticks = sci.tick$at
  		uselabels = sci.tick$exp
  		minors = pretty(major.ticks[1:2],minorn+2)-major.ticks[1]
 		}
 		
  		if(labels){
  		    if(logged){axis(currentside,at=10^major.ticks,tcl=tcl,labels=uselabels,side=currentside,mgp=c(2,tline,0))}
  		    else axis(currentside,at=major.ticks,tcl=tcl,labels=uselabels,side=currentside,mgp=c(2,tline,0))
  		}else{
  		axis(currentside,at=major.ticks,tcl=tcl,labels=FALSE,side=currentside)
  		}
  		
  		minors = minors[-c(1,length(minors))]
  		minor.ticks = c(outer(minors, major.ticks, `+`))
		if(logged){axis(currentside,at=10^minor.ticks,tcl=tcl*ratio,labels=FALSE)}else{
		axis(currentside,at=minor.ticks,tcl=tcl*ratio,labels=FALSE)
		}
    if(is.null(xlab)==F & currentside==1){title(xlab=xlab,line=mtline)}
    if(is.null(ylab)==F & currentside==2){title(ylab=ylab,line=mtline)}
}
if(box){box()}
}
