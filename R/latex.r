#' Latex table
#' 
#' Create a latex table from a hierarchical data.frame
#' 
#' @param table.df hierarchical data.frame
#' @param ... parameters sent to tbl_preamble, tbl_notes, tbl_content
#' @param id.as.is keep id as it is, or convert (default: FALSE)
#' 
#' @family latex
#' @export
latex_table <- function(table.df, ..., id.as.is = FALSE){

  if (!id.as.is){
  # if there is only one Id == 1 then assuming it is the sum row
    if (nrow(table.df) > 1 && nrow(table.df[table.df$Id == 1,]) == 1){
      table.df$Id <- table.df$Id-1
      table.df <- rbind(table.df[rownames(table.df) != 1, ], table.df[rownames(table.df) == 1, ])
      rownames(table.df) <- c(1:nrow(table.df))
    }
  }
  
  if (is.null(table.df$Id)){
    table.df$Id <- rep(1,nrow(table.df))
  }

  if (length(unique(table.df$Id[table.df$Id > 0])) >= 3){
    my.row = NULL
    i <- 1
    while (i <= nrow(table.df)){
      if (!is.na(table.df$Id[i]) && table.df$Id[i] == 1 && (is.na(table.df$Id[i+1]) || table.df$Id[i+1] > 1)){
        my.row = i
      }else if (!is.na(table.df$Id[i]) && table.df$Id[i] > 1 && table.df$Id[i+1] %in% c(0,1, NA_real_)){
        if (!is.null(my.row)){
          if (is.na(table.df$Id[i+1])){
            table.df <- table.df[c(1:i, my.row), ]
          }else{
            table.df <- table.df[c(1:i, my.row, (i+1):nrow(table.df)), ]
          }
          
          table.df[my.row, !(colnames(table.df) %in% c("Id", "label"))] <- NA
          rownames(table.df) <- c(1:nrow(table.df))
          table.df$Id[i+1] <- (table.df$Id[i+1] + 1) * 1000
          table.df$label[i+1] <- paste("Summa", table.df$label[i+1])
          my.row <- NULL
          i <- i + 1
        }
      }
      i <- i + 1
    }
    
  }

  style.indexes <- table.df$Id

  if (!is.null(table.df$notes) && length(table.df[!is.na(table.df$notes) & table.df$notes != "", ]$notes) > 0){
    table.df$notes[is.na(table.df$notes)] <- ""
    tbl.notes <- table.df$notes
  }else{
    tbl.notes <- NULL
  }

  data <- table.df
  data$Id <- NULL
  data$notes <- NULL
  
  data.col.count <- ncol(data)

  out <- tbl_preamble(style.indexes, data.col.count, ...)
  out <- paste(out, tbl_notes(tbl.notes, ...))
  out <- paste(out, tbl_content(data, style.indexes, tbl.notes, ...))
  out <- paste(out, tbl_postamble())
  cat(out)
  
}

#' Latex escape function
#' 
#' @param my.string latex string
#' @family latex
my.latexTranslate <- function(my.string) latexTranslate(ifelse(is.na(my.string), "", my.string), inn = c("{", "}"), out = c("\\{", "\\}"))

#' Latex table preamble
#' 
#' @family latex
tbl_preamble <- function(
  style.indexes, 
  data.col.count, 
  caption = "", 
  subcaption = "", 
  tot.caption = "", 
  first.col.width = "", 
  col.type = "^r", 
  cap.label.format = "empty", 
  width = NULL, 
  tbl.notes = FALSE, 
  col.types.override = "", 
  tbl.note.all = "", 
  pos = "H", ...){
    
  setlength.firstcol <- ""
  first.col.type <- "X"
  first.col.indent.boolean <- ""
    
  if (length(unique(style.indexes[style.indexes > 1])) > 0){
    first.col.type <- "b{\\firstcolwidth}"
  }else{
    first.col.indent.boolean <- "\\toggletrue{firstcolNoIntend}%\n"
  }
    
  if (first.col.width != ""){
    setlength.firstcol <- sprintf("\\setlength\\firstcolwidth{%1s}%%\n", first.col.width)
    first.col.type <- "b{\\firstcolwidth}"
  }
  
  captionsetup <- sprintf("\\captionsetup{labelformat=%1s}%%\n", cap.label.format)
  
  ### ctable setup
  table.setup <- c()
  
  subcap.newline <- ""
  if (caption != "" && subcaption != ""){
    subcap.newline <- "\\nl{}"
  }
  if (subcaption != ""){
    subcaption <- sprintf("{\\mdseries\\color{black} %1s}", my.latexTranslate(subcaption))
  }
  
  caption.nmark <- ""
  
  if (tbl.note.all != ""){
    caption.nmark <- "\\tcapmark"
  }
  
  if (caption != "" | subcaption != ""){
    table.setup <- c(table.setup, sprintf("caption={%s%s%s%s}%%", my.latexTranslate(caption), caption.nmark, subcap.newline, subcaption))
  }
  
  # if caption for table of tables (tot.caption) is given then use it, else use caption if present
  if (tot.caption != ""){
    table.setup <- c(table.setup, sprintf("cap = {%1s}%%", my.latexTranslate(tot.caption)))
  }else if (caption != ""){
    table.setup <- c(table.setup, sprintf("cap = {%1s}%%", my.latexTranslate(caption)))
  }
  
  if (!is.null(width)){
    table.setup <- c(table.setup, sprintf("width = %1s%%", width))
  }
  
  table.setup <- c(table.setup, "captionskip=-0.5em%", sprintf("pos = %s%%", pos), "center%", "topcap%")
  
  table.setup.out <- paste(table.setup, collapse="\n,")
  
  
  ctable <- sprintf("\\ctable[%%\n%1s\n]", paste(table.setup, collapse="\n, "))
  
  col.def <- sprintf("{@{}$%s", first.col.type)
  
  if (col.types.override != ""){
    col.def <- paste(col.def, sprintf("%s@{}}%%\n", col.types.override), sep="")
  }else{
    col.def <- paste(col.def, sprintf("*{%s}{%s}@{}}%%\n", as.character(data.col.count-1), col.type), sep="")
  }
    
  begin.latexcode <- "\\begin{latexcode}%\n"
  
  out <- paste(begin.latexcode, first.col.indent.boolean, setlength.firstcol, captionsetup, ctable, col.def, sep="")
  out
}

#' Latex table postamble
#' 
#' @family latex
tbl_postamble <- function(){
  out <- "\n\\end{latexcode}"
  out
}

#' Latex table content
#' 
tbl_content <- function(data, style.indexes, tbl.notes, round.digits = 0, print.tbl.notes = TRUE, print.first.col.header = FALSE, print.header = TRUE, styles.override = NULL, ...){

  # table header
  col.names <- colnames(data)
  col.names[is.na(col.names)] <- ""
  
  first.col.header <- my.latexTranslate(ifelse(print.first.col.header, col.names[1], ""))
  if (print.header){
    tbl.header <- sprintf("\\headerRow{0em}{%1s}{%2s}%%\n", 
                          first.col.header, 
                          paste("{", my.latexTranslate(col.names[2:length(col.names)]), "}", sep="", collapse=" & "))
  }else{
    tbl.header <- ""
  }

  # row styles
  style.count <- length(unique(style.indexes[style.indexes > 0 & style.indexes < 1000]))
  styles <- c("sumRow")
  if (style.count == 1){
    styles <- c(styles, "normalRow")
  }else if (style.count == 2){
    styles <- c(styles, "normalRow", "lightSmallRow")
  }else{
    styles <- c(styles, "boldNormalRow", "normalRow", "lightSmallRow")
  }
  
  if (!is.null(styles.override)) styles <- styles.override
  
  # table body
  tbl.body <- c()
  tbl.notemark.count <- 0 
  for(i in 1:nrow(data)) {
  
    this.notemark <- ""
    if (!is.null(tbl.notes)){
      if (tbl.notes[i] != "" && !is.na(tbl.notes[i])){
        tbl.notemark.count <- tbl.notemark.count + 1
        this.notemark <- paste("\\tmark[", tbl.notemark.count, "]", sep="")
      }
    }
    
    this.style.calculated <- ifelse(style.indexes[i]+1 >= 4, 4, style.indexes[i]+1)
    next.style.calculated <- ifelse(style.indexes[i+1]+1 >= 4, 4, style.indexes[i+1]+1)
    
    # first column cell
    tbl.first.col.cell <- sprintf(
      "\\%s{%.0fem}{%s%s}{", 
      ifelse(
        style.indexes[i] < 1000 && styles[this.style.calculated] == "lightSmallRow" && (is.na(next.style.calculated) || this.style.calculated > next.style.calculated), 
             "lightSmallRowLast", 
             ifelse(style.indexes[i] >= 1000, 
                    ifelse(is.na(next.style.calculated), 
                           sprintf("subtotalRowLast{%s}", as.character(ncol(data))), 
                           sprintf("subtotalRow{%s}", as.character(ncol(data)))), 
                    styles[this.style.calculated])), 
      ifelse(style.indexes[i] == 0, 0, ifelse(style.indexes[i] >= 1000, style.indexes[i]/1000-1, style.indexes[i]-1)), 
      my.latexTranslate(data[i,1]), 
      this.notemark)
    
    # other column cells
    tbl.cells <- c()
    for (j in 2:ncol(data)){
      cell <- data[i,j]
      
      if(is.numeric(cell)){
        if(is.na(cell)){
          tbl.cells <- c(tbl.cells, "")
        }else{
          tbl.cells <- c(tbl.cells, sprintf(paste("\\numprint{%.", round.digits ,"f}", sep=""), cell))
        }
        
      }else{
        tbl.cells <- c(tbl.cells, sprintf("{%1s}", my.latexTranslate(cell)))
      }
    }
    
    tbl.row <- paste(tbl.first.col.cell, paste(tbl.cells, collapse=" & "), "}%\n")
    tbl.body <- paste(tbl.body, tbl.row, sep="")
  }
  
  out <- paste("{%\n\\toprulepm%\n", tbl.header, tbl.body, "\\bottomrulepm%\n}%", sep="")
  out
}

#' Latex table notes
#' 
#' @family latex
tbl_notes <- function(tbl.notes = NULL, tbl.note.all = "", ...){
  
  tbl.note.all.out <- ""
  if (tbl.note.all != ""){
    tbl.note.all.out <- paste(sprintf("\\tablenote{%s}%%\n", tbl.note.all), sep="")
  }
  
  tbl.notes.out <- ""
  if (!is.null(tbl.notes)){
    tbl.notes.out <- paste("\\tnote[", c(1:length(tbl.notes[tbl.notes != ""])), "]{", my.latexTranslate(tbl.notes[tbl.notes != ""]), "}", sep="", collapse="%\n")
  }
  
  out <- paste("{%\n", tbl.note.all.out, tbl.notes.out, "%\n}%\n", sep="")
  out
}
