magplot <-
function(x,y,xlab,ylab,unlog=FALSE,n=5,...){
plot(x,y,axes=F,xlab='',ylab='',main='',...)
magaxis(n=n,xlab=xlab,ylab=ylab,unlog=unlog,box=T)
}
