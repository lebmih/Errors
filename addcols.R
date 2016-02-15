ehebdf2 <- read.table("C:/Users/User/projects-private/error-rate/datasets/eheb_S4.txt", header = TRUE, sep = "\t")

seqs <- as.character(ehebdf2$cdr3nt)
for(i in seq_along(seqs)){
  ehebdf2$mutations.CDR3[i] <- dataframe3$mutations.CDR3[dataframe3$cdr3nt == seqs[i]][1]
  ehebdf2$cdr.insert.qual[i] <- dataframe$cdr.insert.qual[dataframe$cdr3nt == seqs[i]][1]
  ehebdf2$mutations.qual[i] <- dataframe$mutations.qual[dataframe$cdr3nt == seqs[i]][1]
  ehebdf2$has.cdr3[i] <- dataframe$has.cdr3[dataframe$cdr3nt == seqs[i]][1]
  
}

# Columns to add
cols <- c("mutations.CDR3", "cdr.insert.qual", 'v.end.in.cdr3', 'd.start.in.cdr3', 'd.end.in.cdr3', 'j.start.in.cdr3', 
         'v.del', 'd.del.5', 'd.del.3', 'j.del',
         'pol.v', 'pol.d.5',  'pol.d.3', 'pol.j',
         'mutations.FR1',  'mutations.CDR1', 
         'mutations.FR2', 'mutations.CDR2', 
         'mutations.FR3', 'mutations.FR4',
         'in.frame', 'has.cdr3', 'no.stop', 'complete', 'canonical')


## This function doesn't work and I don't know why
addcols <- function(todf, cols, fromdf){
  seqs <- as.character(todf$cdr3nt)
  for(i in seq_along(seqs)){
    for(col in cols){
      todf[i, col] <- fromdf[fromdf[,"cdr3nt"] == seqs[i], col][1]
    }
  }
}

# Rearranging the dataframe
ehebdf2 <- ehebdf2[,c("freq", "count", "v", "d", "j", "cdr3nt", "cdr3aa",
                      "mutations.CDR3", "cdr.insert.qual", "mutations.qual",
                      "has.cdr3", "in.frame", "no.stop", "canonical", "sample")]

# Saving the table
write.table(ehebdf2, file = "C:/Users/User/projects-private/error-rate/datasets/eheb_S2_new.txt", sep = "\t", row.names = FALSE)

