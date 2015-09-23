#' Create pdf from markdown or tex files
#' 
#' @param files vector of markdwon or tex files
#' @param tex.template tex template to use for pdf generation, preamble must include \code{\\usepackage{import}}
#' @param include.dir include directory when runnig tex, used if there are relative pathes to figures
#' @param single.pdf run tex for each file in \code{files}
#' @param merged.pdf merge all files to one and run tex on that
#' @param save.merged.to directory where to store the merged pdf
#' @param replace.backticks for swedish quotation marks
#' @param keep.tex keep the tex files? 
#' @param embed.fonts embed fonts in pdfs? If you used \code{\link{save_plot}} fonts are already embedded. Default is FALSE
#' @param target.language a language other than swedish
#' @param translation.table used for string translation in objects, se \code{\link{translater}}
#' @param ... arguments passed to \code{\link{run.tex}}
#' 
#' @export
create.pdf <- function(
  files, 
  tex.template, 
  include.dir = "figure", 
  single.pdfs = TRUE, 
  merged.pdf = FALSE, 
  save.merged.to = getwd(), 
  replace.backticks = TRUE, 
  keep.tex = FALSE, 
  embed.fonts = FALSE, 
  target.language = NULL, 
  translation.table = "translation/translation-table.csv", 
  ...){
  
  if (!is.null(target.language)) {
    read.opts <- ft.opts.get("read")
    set_lang(target.language)
    ft.opts.set(read = read.opts)
  }
  
  
  
  timestamp <- format(Sys.time(), "_%Y-%m-%d+%H.%M.%S")
  
  if (!require("pmbundle")) {
    stop("create.pdf() requires package pmbundle to run!")
  }
  fontpath <- system.file("fonts", package = "pmbundle")
  
  files <- normalizePath(files)
  tempdir <- tempdir()
  merged.tex <- normalizePath(file.path(tempdir, "merged.tex"), mustWork = FALSE)
  subimport.lines <- c()
  
  for (i in 1:length(files)){
    message("Processing: ", files[i])
    
    
    file.name <- file_path_sans_ext(basename(files[i]))
    file.ext <- file_ext(files[i])
    file.parent.dir <- basename(dirname(files[i]))
    file.include.dir <- normalizePath(file.path(dirname(files[i]), include.dir), mustWork = FALSE)
    
    dir.create(normalizePath(file.path(tempdir, file.parent.dir), mustWork = FALSE), recursive = TRUE, showWarnings=FALSE)
    
    #load translation table if needed
    translation.table.path <- file.path(dirname(files[i]), translation.table)
    if (!is.null(target.language) && !is.null(translation.table) && translation.table != "" && file_test("-f", translation.table.path)){
      my.translation.table <- read.csv2(translation.table.path, colClasses = "character", header = TRUE, stringsAsFactors = FALSE)
      set_translation_table(translation.table=my.translation.table)
    }
    
    output.tex <- normalizePath(file.path(tempdir, 
                                          file.parent.dir, 
                                          paste0(file.name, ".tex")), 
                                mustWork = FALSE)
    
    # don't brew/pandoc tex files because they can become corrupted
    if (file_ext(files[i]) != "tex") {
      #brew
      output.md <- normalizePath(file.path(tempdir, file.parent.dir, paste0(file.name, ".md")), mustWork = FALSE)
      brew(file = files[i], output = output.md, chdir = TRUE)
      
      # convert encoding
      file.lines <- readLines(output.md, n=-1)
      con = file(output.md, "w+", encoding="UTF-8")
      writeLines(file.lines, con = con)
      close(con)
      
      # embed fonts
      if (embed.fonts && !is.null(file.include.dir) && file_test("-d", file.include.dir)) {
        message("Fonts embeded in: ")
        for (figur in list.files(file.include.dir, ".pdf")){
          #embed_fonts(file=normalizePath(file.path(file.include.dir, figur))) # from package extrafont
          embedFonts(file = normalizePath(file.path(file.include.dir, figur)), 
                     format = "pdfwrite", 
                     outfile = normalizePath(file.path(file.include.dir, figur)), 
                     fontpaths = fontpath)
          message(figur)
        }
      }
      
      # md -> tex med hjälp av pandoc
      run.pandoc(file = output.md, 
             output = output.tex
      )
      
    } else {
      file.copy(files[i], output.tex, overwrite = TRUE)
    }
    
    # copy include dir
    if (!is.null(file.include.dir) && file_test("-d", file.include.dir))
      file.copy(file.include.dir, 
                normalizePath(file.path(tempdir, file.parent.dir)), 
                overwrite = TRUE, 
                recursive = TRUE)
    
    # for swedish quotation marks are the same on both sides
    if (replace.backticks) {
      message("Replacing `` with '' in ", output.tex)
      file.lines <- readLines(output.tex, n=-1)
      file.lines <- gsub("``", "''", file.lines)
      con = file(output.tex, "w+")
      writeLines(file.lines, con = con)
      close(con)
    }
    
    # copy the resulting tex file if needed, don't copy if the input file is a tex file
    if (keep.tex && file_ext(files[i]) != "tex") 
      file.copy(output.tex, 
                normalizePath(file.path(dirname(files[i]), basename(output.tex)), mustWork = FALSE), 
                overwrite = TRUE)
    
    # running tex and copy the pdf
    if (single.pdfs) {
      run.tex(file = output.tex, tex.template = tex.template, ...)
      file.copy(normalizePath(file.path(tempdir, file.parent.dir, paste0(file.name, ".pdf"))), 
                normalizePath(file.path(dirname(files[i]), paste0(file.name, timestamp, ".pdf")), mustWork = FALSE), 
                overwrite = TRUE
      )
    }
    
    # for merged pdf
    subimport.lines[i] <- sprintf("\\subimport{%s/}{%s}", file.parent.dir, basename(output.tex))
    
    # save missing translations
    if (!is.null(target.language)) {
      missing.translations <- get_missing_translations(delete=TRUE)
      if (!is.null(missing.translations)) {
        write.csv2(missing.translations, file = file.path(dirname(files[i]), dirname(translation.table), "missing-translations.csv"), row.names = FALSE)
      }
    }
    
  }
  
  # run tex on merged file, copy merged tex file  and copy resulting pdf
  if (merged.pdf) {
    writeLines(text=subimport.lines, con = merged.tex)
    run.tex(file = merged.tex, tex.template = tex.template, ...)
    if (keep.tex) 
      file.copy(merged.tex, 
                normalizePath(file.path(save.merged.to, "merged.tex"), mustWork = FALSE), 
                overwrite = TRUE)
    file.copy(normalizePath(file.path(tempdir, "merged.pdf")), 
              normalizePath(file.path(save.merged.to, paste0("merged", timestamp, ".pdf")), mustWork = FALSE), 
              overwrite = TRUE
    )
  }
  
  
  
  
}
