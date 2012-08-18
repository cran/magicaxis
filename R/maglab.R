maglab <-
function(lims,n,log=FALSE,exptext=TRUE,crunch=TRUE,logpretty=TRUE){
if(log){lims=log10(lims)}
if(missing(n)) prettyloc=pretty(lims)
else prettyloc=pretty(lims,n)
if(log){
    if(logpretty){prettyloc=prettyloc[prettyloc %% 1==0]}
    prettyloc=10^prettyloc    
    if(exptext){prettychar=format(prettyloc)}
    else prettychar=format(log10(prettyloc))
}
else prettychar=format(prettyloc)

check=grep('e',prettychar)
if(length(check)>0){
    prettychar=format(prettyloc,scientific=T)
    check=grep('e+',prettychar,fixed=T)
	prettychar[check]=paste(sub('e+','*x*10^{',prettychar[check],fixed=T),'}',sep='')
	check=grep('e-',prettychar,fixed=T)
	prettychar[check]=paste(sub('e-','*x*10^{-',prettychar[check],fixed=T),'}',sep='')
}
if(crunch){
check=grep('1*x*',prettychar)
    if(length(check)>0){
        prettychar[check]=sub('1*x*','',prettychar[check],fixed=T)
    }
}
return=list(at=prettyloc,exp=parse(text=prettychar))
}
