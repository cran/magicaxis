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
magaxis(side, n = 5, tcl = 0.5, ratio = 0.5, labels = TRUE, unlog = FALSE, tline = 0.5, mtline = 2, xlab, ylab, box = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{
The side to be used for axis labelling in the same sense as the base axis function. Multiple entries are allowed. If blank all 4 axes are drawn.
}
  \item{n}{
The target number of sub-divisions for pretty plotting.
}
  \item{tcl}{
The length of major ticks. By default these face into the plot (in common with scientific plotting).
}
  \item{ratio}{
Ratio of minor to major tick marks.
}
  \item{labels}{
Should text labels corresponding to major tick location be drawn.
}
  \item{unlog}{
If axis has been explicitly logged (e.g. log10(x)) then this will produce logged axis marking/ labelling if set to TRUE. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Can also take the text argument 'x', 'y', 'xy' or 'yx', where these refer to which axes have been logged.
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
Aaron Robotham (aaron dot robotham at uwa dot edu dot au)
}
\examples{
x=10^{1:9}
y=1:9
plot(log10(x),y,axes=FALSE)
magaxis(unlog='x')

plot(x,y,axes=FALSE,log='x')
magaxis()

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{axis}
\keyword{plot}
\keyword{log}
