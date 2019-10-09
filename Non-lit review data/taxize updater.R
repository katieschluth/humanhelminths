
# Alright, welcome to R

# install.packages('taxize')

library(taxize)




fixer <- function(name) { 
  name2 <- tryCatch(gnr_resolve(name, best_match_only = TRUE)$matched_name,error=function(e) {NULL})
  if(is.null(name2)) {
    return(c(name,NA))
  } else {
    ids <- tryCatch(suppressMessages(get_tsn_(name2,accepted=FALSE)[[1]]$tsn),error=function(e) {NA})
    if (length(ids)==0) { return (c(name,NA)) } else {
      ids <- ids[[1]]
      ids <- as.numeric(as.character(ids))
      x <- classification(ids, db = 'itis')[[1]]
      if(nrow(x)==1) {
        ids <- tryCatch(synonyms(ids,db='itis')[[1]]$acc_tsn[1], error=function(e) {NA})
        if(is.na(ids)) {
          return(c(name2,NA))
        } else {
          x <- classification(ids, db = 'itis')[[1]]
        }
      }
      if(nrow(x[x$rank=='class',])>0) {
        species <- x[x$rank=='species',]$name
        class <- x[x$rank=='class',]$name
        return(c(species,class))
      } else {
        return(c(name2,NA))
      }
    }
  }
}