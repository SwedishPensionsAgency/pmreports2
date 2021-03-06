% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/create.pdf.R
\name{create.pdf}
\alias{create.pdf}
\title{Create pdf from markdown or tex files}
\usage{
create.pdf(files, tex.template, include.dir = "figure", single.pdfs = TRUE,
  merged.pdf = FALSE, save.merged.to = getwd(), replace.backticks = TRUE,
  keep.tex = FALSE, embed.fonts = FALSE, target.language = NULL,
  translation.table = "translation/translation-table.csv", ...)
}
\arguments{
\item{files}{vector of markdwon or tex files}

\item{tex.template}{tex template to use for pdf generation, preamble must include \code{\\usepackage{import}}}

\item{include.dir}{include directory when runnig tex, used if there are relative pathes to figures}

\item{merged.pdf}{merge all files to one and run tex on that}

\item{save.merged.to}{directory where to store the merged pdf}

\item{replace.backticks}{for swedish quotation marks}

\item{keep.tex}{keep the tex files?}

\item{embed.fonts}{embed fonts in pdfs? If you used \code{\link{save_plot}} fonts are already embedded. Default is FALSE}

\item{target.language}{a language other than swedish}

\item{translation.table}{used for string translation in objects, se \code{\link{translater}}}

\item{...}{arguments passed to \code{\link{run.tex}}}

\item{single.pdf}{run tex for each file in \code{files}}
}
\description{
Create pdf from markdown or tex files
}

