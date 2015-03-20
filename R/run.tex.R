
#' Run tex on a file with a template
#' 
#' @param file tex file including only the document part
#' @param tex.template tex template, the tag \code{$body$} is replaced with \code{file}
#' @param tex.engine which tex engine should be used? Default is lualtatex
#' @param tex.runs number of tex runs, for some configurations tex must run several times for correct output
#' @export
run.tex <- function(
  file, 
  tex.template, 
  tex.engine = "lualatex",
  tex.options = "-halt-on-error",
  tex.runs = 2){
  
  # put together the tex file and the tex template, as in pandoc the tag $body$ in the tex template is used to place the tex file.
  file.lines <- readLines(file, warn = FALSE)
  tex.template.lines <- readLines(tex.template, warn = FALSE)
  
  body.index <- which(str_detect(tex.template.lines, "[:blank:]*\\$body\\$[:blank:]*"))
  
  full.lines <- c(tex.template.lines[1:(body.index-1)], 
                  file.lines, 
                  tex.template.lines[(body.index+1):length(tex.template.lines)])
  
  tmp.file.tex <- normalizePath(file.path(dirname(file), "tmp.tex"), mustWork=FALSE)
  tmp.file.pdf <- normalizePath(file.path(dirname(file), "tmp.pdf"), mustWork=FALSE)
  
  template.dir <- normalizePath(dirname(tex.template))
  dirs.to.include <- list.dirs(template.dir)
  
  for (dir in dirs.to.include[dirs.to.include != template.dir]){
    target.dir <- normalizePath(file.path(dirname(file), basename(dir)), mustWork = FALSE)
    if(!file_test("-d", target.dir))
      dir.create(target.dir)
    file.copy(dir, 
              normalizePath(file.path(dirname(file))), 
              overwrite = TRUE, 
              recursive = TRUE)
  }
  
  writeLines(full.lines, tmp.file.tex)
  wd <- getwd()
  setwd(normalizePath(dirname(tmp.file.tex)))
  for (i in 1:tex.runs){
    tex.command <- sprintf('%s %s -output-directory="%s" "%s" ',
                           tex.engine,
                           tex.options,
                           normalizePath(dirname(tmp.file.tex)),
                           basename(tmp.file.tex))
    message("TeX: ", tex.command)
    command_output <- system(tex.command, intern = TRUE)
    if(!is.null(attr(command_output, "status"))){
      setwd(wd)
      message(paste(command_output, collapse="\n"))
      stop(paste0(tex.engine, " exited with an error code: ", attr(command_output, "status"), ". Se the log above for more information."))
    }
  }
  output.pdf <- normalizePath(paste0(file_path_sans_ext(file), ".pdf"), mustWork=FALSE)
  if (file.exists(tmp.file.pdf))
    unlink(output.pdf)
  file.rename(tmp.file.pdf, output.pdf)
  setwd(wd)
}
