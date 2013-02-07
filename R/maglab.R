maglab <-
function(lims,n,log=FALSE,exptext=TRUE,crunch=TRUE,logpretty=TRUE,usemultloc=FALSE,multloc=c(1,2,5),prettybase=10){
lims=lims/(prettybase/10)
if(log){if(usemultloc==F){lims=log10(lims)}}
if(missing(n)){labloc=pretty(lims)}else{labloc=pretty(lims,n)}
if(log){labloc=labloc+log10(prettybase/10)}else{labloc=labloc*(prettybase/10)}
if(log){
    if(log){labloc=labloc[round(labloc -log10(prettybase/10),10) %% 1==0];labloc=round(labloc,10)}
    if(usemultloc==F){labloc=10^labloc;tickloc=labloc}
    if(usemultloc){
        labloc={}
        for(i in 1:length(multloc)){labloc=c(labloc,multloc[i]*10^seq(ceiling(log10(lims[1]))-1,floor(log10(lims[2]))+1))}
        labloc=sort(labloc)
        tickloc={}
        for(i in 1:9){tickloc=c(tickloc,i*10^seq(ceiling(log10(lims[1]))-1,floor(log10(lims[2]))+1))}
        tickloc=sort(tickloc)
    }
    #annoyingly I get weird issues for some numbers (e.g 0.00035) if they are in an otherwise scientific format list, and this behaves differently to the formatting on the actual plots. Only way round this is to format each number individually.
    char={}
    if(exptext){for(i in 1:length(labloc)){char=c(char,format(labloc[i]))}}
    if(! exptext){for(i in 1:length(labloc)){char=c(char,format(log10(labloc[i])))}}
}else{
tickloc=labloc
char={}
for(i in 1:length(labloc)){char=c(char,format(labloc[i]))}
}

check=grep('e',char)
if(length(check)>0){
    char=format(labloc,scientific=T)
    check=grep("0e+00",char,fixed=T)
    char[check]="0"
    check=grep('e+',char,fixed=T)
	char[check]=paste(sub('e+','*x*10^{',char[check],fixed=T),'}',sep='')
	check=grep('e-',char,fixed=T)
	char[check]=paste(sub('e-','*x*10^{-',char[check],fixed=T),'}',sep='')
}
if(crunch){
check=grep('1*x*',char)
    if(length(check)>0){
        char[check]=sub('1*x*','',char[check],fixed=T)
    }
}
return=list(tickat=tickloc,labat=labloc,exp=parse(text=char))
}
