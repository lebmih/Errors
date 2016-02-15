## Here will be the unique part of the Signal-to-noise ratios script
# In the previous part we concatenated all files and dropped all the waste
# Result of the script: table with signal-to-noise ratios for all included samples

# Spike-in variants sequences
EHEBvars <- data.frame("V" = c("EHEB", "EHEB.V1", "EHEB.V2"),
                       "Seq" = (c("TGTGCGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG",
                                 "TGTGGGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG",
                                 "TGTGCGACACATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG")))

# Erroneous variants sequences (first two)
# This information is gathered from err-rate script output
Errors <- data.frame("Err" = c("Err1", "Err2"),
                     "Seq" = c("TGTGCGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACCTTGG",
                               "TGTGCGAGAGATGATGGCGGGGGGAAAGGGTGACTACGGAAGACTTTGG"))

# Sample names fetching
sample.names <- levels(as.factor(dataframe$Sample))

# Creating dataframe
stnratios <- data.frame("Clonotype to error" = row.names,
                        "S1" = vector("numeric", length(row.names)))

# Signal-to-noise ratio computing function
stnratio <- function(clonotype, error, sample = "ALL"){
  cdr3.clonotype.seq <- as.character(EHEBvars$Seq[EHEBvars$V == clonotype])
  error.seq <- as.character(Errors$Seq[Errors$Err == error])
  if (sample == "ALL"){
    clonotype.count <- sum(dataframe$count[dataframe$cdr3nt == cdr3.clonotype.seq])
    error.count <- sum(dataframe$count[dataframe$cdr3nt == error.seq])
  }
  else{
    clonotype.count <- sum(dataframe$count[dataframe$cdr3nt == cdr3.clonotype.seq &
                                             dataframe$Sample == sample])
    error.count <- sum(dataframe$count[dataframe$cdr3nt == error.seq &
                                         dataframe$Sample == sample])
  }
  stnratio <- clonotype.count/error.count
  stnratio
}

# Defining row names for the table
row.names <- c()

# Filling the table
for (i in seq_along(EHEBvars$V)){
  for (j in seq_along(Errors$Err)){
    row.names <- c(row.names, paste(EHEBvars$V[i], " to ", Errors$Err[j]))
    for (k in seq_along(sample.names)){
      stnratios[2*i+j-2, k] <- stnratio(EHEBvars$V[i], Errors$Err[j], sample.names[k])
    }
  }
}

stnratios <- as.data.frame(stnratios, row.names = row.names)
colnames(stnratios) <- sample.names

# Dropping the waste
rm(row.names, sample.names, i, j, k)
