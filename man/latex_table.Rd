% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/latex.r
\name{latex_table}
\alias{latex_table}
\title{Latex table}
\usage{
latex_table(table.df, ..., id.as.is = FALSE)
}
\arguments{
\item{table.df}{hierarchical data.frame}

\item{...}{parameters sent to tbl_preamble, tbl_notes, tbl_content}

\item{id.as.is}{keep id as it is, or convert (default: FALSE)}
}
\description{
Create a latex table from a hierarchical data.frame
}
\seealso{
Other latex: \code{\link{my.latexTranslate}};
  \code{\link{tbl_notes}}; \code{\link{tbl_postamble}};
  \code{\link{tbl_preamble}}
}

