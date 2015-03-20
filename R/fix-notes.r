#' Fix notes
#' 
#' A temporary fix function for notes in tables
#' 
#' @param x hierarchical data.frame with notes
#' @export
fix_notes <- function(x){
  if (is.null(x$notes)){return(x)} 
  
  unique.notes <- unique(x$notes[x$notes != ""])
  
  ids.notes <- x[x$notes %in% unique.notes, c("Id", "notes")]
  x$sort <- row.names(x)
  if (nrow(ids.notes) != length(unique(ids.notes$Id))){
    for (ids in unique(ids.notes$Id)){
      if (length(unique(ids.notes[ids.notes$Id == ids, ]$notes)) > 1){
        ids.notes[ids.notes$Id == ids, ]$notes <- rep(paste(ids.notes[ids.notes$Id == ids, ]$notes, collapse=" "), nrow(ids.notes[ids.notes$Id == ids, ]))
      }
    }
    ids.notes <- unique(ids.notes)
  }
  
  x$notes <- NULL
  x <- merge(x, ids.notes, by = c("Id"), sort = FALSE, all = FALSE, all.x = TRUE)
  x <- x[order(as.integer(x$sort)),]
  x$sort <- NULL
  x[is.na(x$notes), ]$notes <- ""
  x
}
