% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/run.tex.R
\name{run.tex}
\alias{run.tex}
\title{Run tex on a file with a template}
\usage{
run.tex(file, tex.template, tex.engine = "lualatex",
  tex.options = "-halt-on-error", tex.runs = 2)
}
\arguments{
\item{file}{tex file including only the document part}

\item{tex.template}{tex template, the tag \code{$body$} is replaced with \code{file}}

\item{tex.engine}{which tex engine should be used? Default is lualtatex}

\item{tex.runs}{number of tex runs, for some configurations tex must run several times for correct output}
}
\description{
Run tex on a file with a template
}

