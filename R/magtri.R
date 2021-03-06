magtri=function(chains, samples=1000, thin=1, samptype='end', grid=FALSE, do.tick=FALSE, refvals=NULL, lab=NULL, ...){
  chains=as.data.frame(chains)
  chaincolnames=colnames(chains)
  Nsamp=dim(chains)[1]
  Npar=dim(chains)[2]
  if(!is.null(refvals)){
    if(length(refvals) != Npar){stop('Length of refvales must be equal to number of parameters!')}
  }
  if(Npar<=1){stop('Need 2+ parameters!')}
  
  if(thin>1){
    chains=chains[seq(1,Nsamp,by=thin),,drop=FALSE]
    Nsamp=dim(chains)[1]
  }
  
  if(samples>Nsamp){samples=Nsamp}
  layout(matrix(1:Npar^2,Npar,Npar)[Npar:1,])
  meanvec={}
  sdvec={}
  if(samptype=='end'){usesamps=(Nsamp-samples+1):Nsamp}
  if(samptype=='ran'){usesamps=sample(Nsamp,samples)}
  if(samptype=='thin'){usesamps=seq(1,Nsamp,length=samples)}
  for(i in 1:Npar){
    meanvec=c(meanvec,mean(chains[usesamps,i]))
    sdvec=c(sdvec,sd(chains[usesamps,i]))
  }
  par(oma=c(4.1,4.1,1.1,1.1))
  for(i in 1:Npar){
    for(j in 1:Npar){
      par(mar=c(0,0,0,0))
      xrange=range(chains[usesamps,i])
      yrange=range(chains[usesamps,j])
      if(xrange[1]==xrange[2]){
        val=xrange[1]
        xrange[1]=val-0.05
        xrange[2]=val+0.05
      }
      if(yrange[1]==yrange[2]){
        val=yrange[1]
        yrange[1]=val-0.05
        yrange[2]=val+0.05
      }
      if(i==j){
        xtemp=chains[usesamps,i]
        if(sd(xtemp)==0){xtemp=xtemp+rnorm(samples,sd=1e-3)}
        plot(density(xtemp),axes=FALSE,main='',xlim=xrange)
        magaxis(1,grid=grid, grid.col = 'lightgrey',labels=FALSE,do.tick=do.tick)
        abline(v=meanvec[i],lty=1,col='red')
        abline(v=meanvec[i]-sdvec[i],lty=3,col='red')
        abline(v=meanvec[i]+sdvec[i],lty=3,col='red')
        if(!is.null(refvals)){
          abline(v=refvals[i],lty=1, col='blue')
        }
        box()
        if(i==1){
          plot.window(xlim=xrange,ylim=yrange)
          if(is.null(lab)){
            magaxis(1,xlab=chaincolnames[i])
          }else{
            magaxis(1,xlab=lab[[i]])
          }
          if(is.null(lab)){
            magaxis(2,ylab=chaincolnames[j])
          }else{
            magaxis(2,ylab=lab[[j]])
          }
        }
      }else{
        if(i>j){
          plot.new()
          plot.window(xlim=xrange,ylim=yrange)
          xtemp=chains[usesamps,i]
          ytemp=chains[usesamps,j]
          if(sd(xtemp)==0){xtemp=xtemp+rnorm(samples,sd=1e-3)}
          if(sd(ytemp)==0){ytemp=ytemp+rnorm(samples,sd=1e-3)}
          magaxis(1:2,grid=grid, grid.col = 'lightgrey',labels=FALSE,do.tick=do.tick)
          magcon(xtemp,ytemp,dobar=FALSE,doim=FALSE,add=TRUE,lty=c(2,1,3),xlim=xrange,ylim=yrange, h=c(diff(xrange),diff(yrange))/10, ...)
          points(meanvec[i],meanvec[j],col='red',pch=4,cex=2)
          box()
          abline(v=meanvec[i],lty=1,col='red')
          abline(v=meanvec[i]-sdvec[i],lty=3,col='red')
          abline(v=meanvec[i]+sdvec[i],lty=3,col='red')
          if(!is.null(refvals)){
            abline(v=refvals[i],lty=1, col='blue')
          }
          if(j==1){
            if(is.null(lab)){
              magaxis(1,xlab=chaincolnames[i])
            }else{
              magaxis(1,xlab=lab[[i]])
            }
          }
        }else{
          plot.new()
          plot.window(xlim=xrange,ylim=yrange)
          magaxis(1:2,grid=grid, grid.col = 'lightgrey',labels=FALSE,do.tick=do.tick)
          points(chains[usesamps,c(i,j)],pch='.',col='darkgrey')
          points(meanvec[i],meanvec[j],col='red',pch=4,cex=2)
          box()
          if(i==1){
            if(is.null(lab)){
              magaxis(2,ylab=chaincolnames[j])
            }else{
              magaxis(2,ylab=lab[[j]])
            }
          }
        }
      }
    }
  }
  output=cbind(mean=meanvec, sd=sdvec)
  rownames(output)=chaincolnames
  return(invisible(output))
}
