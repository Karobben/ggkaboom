\name{Kaboom_bar}
\alias{Kaboom_bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kaboom_bar
}
\description{
Kaboom Bar is a function from raw data to make quick statistics and customized bar plots in minimal arguments for those who are frustrated about time spent making statistics.
}
\usage{
  Kaboom_bar(data, x,
	 Col= FALSE, Var="SD", fill = FALSE,
	 Pos = "dodge", BarW = .9, BarAl = .6, ErbW = .3,
   Plate = "Set1",
   Facet = "wrap", Facet_row = FALSE, scales = "fixed",
   space="fixed")

# Quick Start
Kaboom_bar(iris, "Species")
Kaboom_bar(ChickWeight[-3], 'Time', 'Diet',Facet = F, fill = "Diet", Var = "SEM")
Kaboom_bar(midwest[c(1:10,200:210, 300:310), c(3:8,ncol(midwest))], "state", "category", Var = "SEM", Facet_row = 'Variable', space = 'free', scales = 'free')

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame
}
  \item{x}{
  Variable for X axis. The mean and sd/sem would calculated.
}
  \item{Col}{
  The second variable. The mean and sd/sem would be calculated based on the `x` and `Col`
}
  \item{fill}{
  Colors for the bar. Default is the Variable
}
  \item{BarW}{
  float. Width of the bar
}
  \item{BarAl}{
  0:1: Alpha of the bar
}
  \item{ErbW}{
  float: Width of the Error bar
}
  \item{Plate}{
  "Set1", "Paired", ... Color Plate. Check all plate by `brewer.pal.info`
}
  \item{Facet}{
  "wrap", "grid": `facet_wrap` if there has `Col`.
  }
  \item{Facet_row}{
  facet by a variable in row. No statistics.
  }
  \item{scales}{
  as facet_grid(scales=...)
  }
  \item{space}{
  as facet_grid(space=...)
  }

}
\details{
Quic Bar plot
}
\author{
  Karobben
}

\examples{

Kaboom_bar(iris, "Species")

Kaboom_bar(ChickWeight[-3], 'Time', 'Diet',Facet = F, fill = "Diet", Var = "SEM")

Kaboom_bar(midwest[c(1:10,200:210, 300:310), c(3:8,ncol(midwest))], "state", "category", Var = "SEM", Facet_row = 'Variable', space = 'free', scales = 'free')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
