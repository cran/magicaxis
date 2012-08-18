\name{magplot}
\alias{magplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes scientific plots based on magaxis axes. Particularly designed for log plotting.
}
\description{
Utilises base plot for the most part, but the axis drawing is replaced by a call to the magaxis fuction.
}
\usage{
magplot(x,y,log='',xlab=NULL,ylab=NULL,unlog='Auto',majorn=5,minorn=5,main='',labels=TRUE,crunch=TRUE,logpretty=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{	
The coordinates of points in the plot. Alternatively, a single plotting structure, function or any R object with a plot method can be provided.
}
  \item{y}{
The y coordinates of points in the plot, optional if x is an appropriate structure.
}
  \item{log}{
log axis arguments to be passed to plot. e.g. use 'x', 'y', 'xy' is appropriate. Default '' assumes no logging of any axes.
}
  \item{xlab}{
If desired, label for x-axis. Default produces no label.
}
  \item{ylab}{
If desired, label for y-axis. Default produces no label.
}
  \item{unlog}{
unlog argument to be passed to magaxis. If axis has been explicitly logged (e.g. log10(x)) then this will produce logged axis marking/ labelling if set to TRUE. If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides). Can also take the text argument 'x', 'y', 'xy' or 'yx', where these refer to which axes have been logged.
}
  \item{majorn}{
The target number of major axis sub-divisions for pretty plotting.
}
  \item{minorn}{
The target number of major axis sub-divisions for pretty plotting.
}
  \item{main}{
Title for the plot. Default is no title.
}
  \item{labels}{
labels argument to be passed to magaxis. Specifies whether major-axis ticks should be labelled for each axis. If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides). Default is to label all axes.
}
  \item{crunch}{
In cases where the scientific text would be written as 1x10^8, should the 1x be removed so it reads 10^8. TRUE by default.If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides).
}
  \item{logpretty}{
Should the major-ticks only be located at powers of 10. This changes cases where ticks are placed at 1, 3.1, 10, 31, 100 etc to 1, 10, 100. If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides). TRUE by default.
}
  \item{\dots}{
Further arguments to be passed to base plot.
}
}
\details{
This is a simple function that just turns off most of the plotting output of base plot, and replaces where possible those present in magaxis.
}
\value{
No output. Run for the side effect of producing nice plotting axes.
}

\author{
Aaron Robotham
}

\examples{
x=10^{1:9}
y=1:9
magplot(log10(x),y,unlog='x')

magplot(x,y,log='x')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
\keyword{axis}% __ONLY ONE__ keyword per line
\keyword{log}
