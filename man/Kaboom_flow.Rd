\name{Kaboom_flow}
\alias{Kaboom_flow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kaboom_flow
}
\description{
Kaboom flow is a function for plotting the riverplot or snake plot. Traditional riverplot could not handle unique categories without come and going. this flow chart is designed to achieve to illustrate something like gene mutation through generations. This is based on GGplot
}

\usage{
  Kaboom_flow(TB)

# Quick Start
A <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
B <- c("A", "B", "C", "D", "E", "F", "K", "L", "M", "N")
C <- c("A", "B", "C", "D", "E", "F", "G", "H", "N","O")

TB = data.frame(row.names =  sort(unique(c(A,B,C))))
Num = 0
for(Col in list(A,B,C)){
    Num = Num + 1
    TB[paste("Group",Num, sep="_")] = 0
    TB[paste("Group",Num, sep="_")][row.names(TB) %in% Col,] = 1
}

Kaboom_flow(TB)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame
}
}

\examples{

Kaboom_flow(TB)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ riverplot }% use one of  RShowDoc("KEYWORDS")
\keyword{ flow chart }% __ONLY ONE__ keyword per line
