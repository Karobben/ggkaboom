\name{Kaboom_tb_g}
\alias{Kaboom_tb_g}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kaboom_tb_g
}
\description{
This function is for plotting a table in a second with ggolot. The value would return to a ggplot object so you can add more themes at the end of the function. It accepts a dataframe and turns it into a matrix. The columns and row names would be used as the title of the table.
}
\usage{
Kaboom_tb_g(head(mtcars),
    Var1= "X", Var2 ="Y",
    T_font=14, T_fill= "#0f0f0f",
    T_color= "white" ,T_grid = 'grey',
    fill=FALSE)

# Quick Start
Kaboom_tb_g(head(mtcars))
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame
}
  \item{Var1}{
  Name of row names, default is "X".
}
  \item{Var2}{
  Name of row names, default is "Y".
}
  \item{T_font}{
  Font of the row and col names
}
\item{T_color}{
  Color of the row and col names
}
\item{T_fill}{
  background color of the row and col names

}
  \item{T_grid}{
  line color of the table (grid)
}
  \item{fill}{
  fill each table with different colors based on the value
}

\author{
  Karobben
}

\examples{

Kaboom_tb_g(head(mtcars))
Kaboom_tb_g(head(mtcars), T_fill="steelble")

#Kaboom_bar(midwest[c(1:10,200:210, 300:310), c(3:8,ncol(midwest))], "state", "category", Var = "SEM", Facet_row = 'Variable', space = 'free', scales = 'free')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ggplot }% use one of  RShowDoc("KEYWORDS")
\keyword{ table }% __ONLY ONE__ keyword per line
