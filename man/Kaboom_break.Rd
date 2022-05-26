\name{Kaboom_break}
\alias{Kaboom_break}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kaboom_flow
}
\description{
Kaboom for break plot. Raw function: https://karobben.github.io/2020/06/11/Blog/ggplot_split/
}

\usage{
data(cars)
cars[1,2] =100000

p <- ggplot(cars,aes(x=speed,y=dist)) + geom_bar(stat='identity')

# For tow break
Kaboom_break(p, c(0,  400, 10000, 120000), R=c(4, 1))
# For Three break
Kaboom_break(p, c(0, 10, 30, 400, 10000, 120000), R=c(1,4, 1))
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame
}
}

\examples{

Kaboom_break(p)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ break plot }% use one of  RShowDoc("KEYWORDS")
\keyword{ ggplot }% __ONLY ONE__ keyword per line
