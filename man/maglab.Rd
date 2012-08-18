\name{maglab}
\alias{maglab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate tick locations and scientifically formatted expressions
}
\description{
Utilises pretty for the major-tick locations, but makes prettier decisions if log axes are being used. Translates the default text into nicely formatted expressions- this is particularly successful when axes are logged and exponents are used since formats like 1e5 should not be used in scientific academic journals.
}
\usage{
maglab(lims,n,log=FALSE,exptext=TRUE,crunch=TRUE,logpretty=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lims}{	
Limits over which pretty major-tick locations will be calculated.
}
  \item{n}{
The target number of major-axis sub-divisions. Will not necessarily be achieved.
}
  \item{log}{
Should the limits be evenly distributed over log space. Usually what you want if an axis has been logged.
}
  \item{exptext}{
Should log==TRUE then should the text be written in exponent form (e.g. 10^8, default when exptext==TRUE) or logged (e.g. 8 in this case).
}
  \item{crunch}{
In cases where the scientific text would be written as 1x10^8, should the 1x be removed so it reads 10^8. TRUE by default.
}
  \item{logpretty}{
Should the major-ticks only be located at powers of 10. This changes cases where ticks are placed at 1, 3.1, 10, 31, 100 etc to 1, 10, 100.
}

}
\details{
This function is a mid level routine for producing nice ticks and text, with particularly effort on improving the outcome of logged axis cases. The end user will probably not require axis to it except in unusual circumstances. I note that my method of translating the default representation of the exponents is not very elegant, so any suggestions for improvement are welcome!
}
\value{
   \item{at}{
    Location of proposed major-tick marks.
}
    \item{exp}{
    Expressions to be used at major-tick marks.
}
}

\author{
Aaron Robotham
}

\examples{
x=10^{1:9}
y=1:9
plot(log10(x),y,axes=FALSE)
ticks=maglab(range(x),log=TRUE)
print(ticks)
axis(1,at=log10(ticks$at),labels=ticks$exp)

# Same outcome a different way:

plot(x,y,axes=FALSE,log='x')
ticks=maglab(range(x),log=TRUE)
print(ticks)
axis(1,at=ticks$at,labels=ticks$exp)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
\keyword{axis}% __ONLY ONE__ keyword per line
\keyword{log}
