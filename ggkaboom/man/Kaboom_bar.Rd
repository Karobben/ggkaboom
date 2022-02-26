\name{Kaboom_bar}
\alias{Kaboom_bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
喵喵喵？
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Kaboom_bar(Data = Data, Group = Group, Var = "SD", Pos = "dodge", BarW = 0.9, BarAl = 0.6, ErbW = 0.3, Plate = "Set1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
}
  \item{Group}{
%%     ~~Describe \code{Group} here~~
}
  \item{Var}{
%%     ~~Describe \code{Var} here~~
}
  \item{Pos}{
%%     ~~Describe \code{Pos} here~~
}
  \item{BarW}{
%%     ~~Describe \code{BarW} here~~
}
  \item{BarAl}{
%%     ~~Describe \code{BarAl} here~~
}
  \item{ErbW}{
%%     ~~Describe \code{ErbW} here~~
}
  \item{Plate}{
%%     ~~Describe \code{Plate} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Data = Data, Group = Group, Var = "SD", Pos = "dodge", 
    BarW = 0.9, BarAl = 0.6, ErbW = 0.3, Plate = "Set1") 
{
    List <- unique(Data[[Group]])
    TB = data.frame()
    for (i in List) {
        TMP = Data[Data[Group] == i, ]
        TMP = TMP[-which(colnames(TMP) == Group)]
        Mean = apply(TMP, 2, mean, na.rm = TRUE)
        SD = apply(TMP, 2, sd, na.rm = TRUE)
        SEM = SD/sqrt(nrow(TMP))
        tmp = data.frame(Species = i, Mean = Mean, Sd = SD, Sem = SEM)
        tmp$Variable = row.names(tmp)
        TB = rbind(TB, tmp)
    }
    P <- ggplot() + geom_bar(data = TB, aes(x = Species, y = Mean, 
        fill = Variable), stat = "identity", position = Pos, 
        alpha = BarAl, width = BarW) + theme_bw() + scale_fill_brewer(palette = Plate)
    if (Var == "SD") {
        P <- P + geom_errorbar(data = TB, aes(x = Species, y = Mean, 
            ymax = Mean + Sd, ymin = Mean - Sd, fill = Variable), 
            position = position_dodge(0.9), width = ErbW)
    }
    if (Var == "SEM") {
        P <- P + geom_errorbar(data = TB, aes(x = Species, y = Mean, 
            ymax = Mean + Sem, ymin = Mean - Sem, fill = Variable), 
            position = position_dodge(0.9), width = ErbW)
    }
    return(P)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
