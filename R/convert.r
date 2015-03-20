#' Convert Rmd to Markdown
#' 
#' Runs knit to convert from .Rmd to .md, as well as embed fonts.
#' 
#' @param input input file
#' @param dir.path file path
#' @param embed.fonts embed fonts (default: TRUE)
#' @param output.tex output tex
#' @param replace.backticks replace backticks (default: TRUE)
#' 
#' @family convert
#' @export
Rmd2md <- function(input, dir.path, embed.fonts = TRUE, output.tex = "", replace.backticks = TRUE, ...){
  if (dev.cur() != 1) capture.output(dev.off())
  output <- knit(input, ...)
  file.lines <- readLines(output, n=-1)
  my.con = file(output, "w", encoding="UTF-8")
  writeLines(file.lines, con = my.con)
  close(my.con)
  
  # Embeding fonts into pdfs
  if (embed.fonts){
    for (file in list.files("figure", ".pdf")){
      embed_fonts(file=paste("figure/", file, sep= ""))
      cat("Fonts embeded in: \n")
      cat(file)
      cat("\n")
    }
  }

  
  if (output.tex == "") output.tex <- output
  
  cat(pandoc(output, dir.path = dir.path, output.tex = output.tex, ...))
  
  if (replace.backticks){
    cat("\nReplacing `` with '' \n")
    
    file.lines <- readLines(output.tex, n=-1)
    file.lines <- gsub("``", "''", file.lines)
    my.con = file(output.tex, "w+")
    writeLines(file.lines, con = my.con)
    close(my.con)
  }
  return(output)
}

#' Convert with Pandoc
#' 
#' Run pandoc and convert between different file formats.
#' Requires pandoc to be installed, and that its environment variable is added to the PATH.
#' 
#' @param input input file
#' @param dir.path file path
#' @param output.tex output tex
#' @param from input file format (default: "markdown")
#' @param to output file format (default: "latex")
#' @param options pandoc options (default: "--chapters")
#' 
#' @family convert
#' @export
# pandoc <- function(input, dir.path, output.tex = "chapter.tex", from = "markdown", to = "latex", options = "--chapters"){
#   cmd <- sprintf("pandoc.exe -f %1s -t %2s %3s -o %4s %5s", 
#                  from, 
#                  to, 
#                  options, 
#                  paste(dir.path, output.tex, sep=""), 
#                  paste(dir.path, input, sep=""))
#   system(cmd)
#   paste("Executed command:", cmd, "\n")
# }
