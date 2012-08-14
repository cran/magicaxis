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
magplot(x, y, xlab, ylab, unlog = FALSE, n = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{	
the coordinates of points in the plot. Alternatively, a single plotting structure, function or any R object with a plot method can be provided.
}
  \item{y}{
the y coordinates of points in the plot, optional if x is an appropriate structure.
}
  \item{xlab}{
If desired, label for x-axis.
}
  \item{ylab}{
If desired, label for y-axis.
}
  \item{unlog}{
unlog argument to be passed to magaxis. If axis has been explicitly logged (e.g. log10(x)) then this will produce logged axis marking/ labelling if set to TRUE. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Can also take the text argument 'x', 'y', 'xy' or 'yx', where these refer to which axes have been logged.
}
  \item{n}{
The target number of sub-divisions for pretty plotting.
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
Aaron Robotham (aaron dot robotham at uwa dot edu dot au)
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
