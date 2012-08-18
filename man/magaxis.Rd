\name{magaxis}
\alias{magaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create nice axes for scientific plots. Particularly designed for log plotting.
}
\description{
Code to generate nicely arranged axes for scientific plots, including minor tick marks. It supports log settings and can unclog axes that have been logged inline by the user.
}
\usage{
magaxis(side=1:4,majorn=5,minorn=5,tcl=0.5,ratio=0.5,labels=TRUE,unlog='Auto',tline=0.5,mtline=2,xlab=NULL,ylab=NULL,box=FALSE,crunch=TRUE,logpretty=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{
The side to be used for axis labelling in the same sense as the base axis function. Multiple entries are allowed. If blank all 4 axes are drawn.
}
  \item{majorn}{
The target number of major-axis sub-divisions for pretty plotting.
}
  \item{minorn}{
The target number of minor-axis sub-divisions for pretty plotting.
}
  \item{tcl}{
The length of major ticks. By default these face into the plot (in common with scientific plotting).
}
  \item{ratio}{
Ratio of minor to major tick marks.
}
  \item{labels}{
Specifies whether major-axis ticks should be labelled for each axis. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Default is to label all axes.
}
  \item{unlog}{
Determines if axis labels should be unlogged. If axis is found to be logged in par('usr') then the minor ticks are automatically log spaced, however "unlog" still controls how the labelling is done: either logged form (FALSE) or exponent form (TRUE). If axis has been explicitly logged (e.g. log10(x)) then this will can produce exponential axis marking/ labelling if set to TRUE. This case will also produce log minor tick marks. If length of unclog is 1 and length of side is longer than 1 then the assigned unlog value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Can also take the text argument 'x', 'y', 'xy' or 'yx', where these refer to which axes have been logged. If left at the default of `Auto' then unlog is assumed to be true when the axis in question is logged, and false otherwise.
}
  \item{tline}{
Number of lines separating labels from axis.
}
  \item{mtline}{
Number of lines separating axis name from axis.
}
  \item{xlab}{
x axis name.
}
  \item{ylab}{
y axis name.
}
  \item{box}{
Should a box be added around the plotting region.
}
  \item{crunch}{
In cases where the scientific text would be written as 1x10^8, should the 1x be removed so it reads 10^8. TRUE by default. If length is 1 and length of side is longer then this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side.
}
  \item{logpretty}{
Should the major-ticks only be located at powers of 10. This changes cases where ticks are placed at 1, 3.1, 10, 31, 100 etc to 1, 10, 100. If length is 1 and length of side is longer then this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. TRUE by default.
}
  \item{\dots}{
Other arguments to be passed to base axis function.
}
}
\details{
This function tries hard to make nice plots for scientific papers.
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
plot(log10(x),y,axes=FALSE)
magaxis(unlog='x')

plot(log10(x),y,axes=FALSE)
magaxis(side=c(1,3),unlog=c(TRUE,FALSE))

plot(x,y,axes=FALSE,log='x')
magaxis()

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{axis}
\keyword{plot}
\keyword{log}
