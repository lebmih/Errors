# Saving current working directory and moving to error-rate
homedir <- getwd()
setwd("C:/Users/User/projects-private/error-rate")

## Here comes actual version of the script

# Specifying file names
data.dir <- "C:/Users/User/projects-private/error-rate/datasets"
files <- c("S1_CONS.txt.gz", "S2_CONS.txt.gz",
           "S3_CONS.txt.gz", "S4_CONS.txt.gz")

# Reading tables
load.sample <- function(file){
  
  file.name <- paste(data.dir, file, sep = "/")
  df <- read.table(gzfile(file.name), header = TRUE, sep = "\t")
  df$Sample <- strsplit(file, "_")[[1]][1]
  df
  
}

# Concatenating data frames
concatenate <- function(files){
  
  dataframe <- data.frame()
  for(file in files){
    df <- load.sample(file)
    dataframe <- rbind(dataframe, df)
  }
  dataframe
  
}

dataframe <- concatenate(files)

# Dropping waste
drops <- c("v.end.in.cdr3", "d.start.in.cdr3",
           "d.end.in.cdr3", "j.start.in.cdr3",
           "v.del", "d.del.5", "d.del.3", "j.del",
           "pol.v", "pol.d.5", "pol.d.3", "pol.j")
dataframe <- dataframe[,!(names(dataframe) %in% drops)]

# Selecting complete CDR3 only
dataframe <- dataframe[dataframe$complete == "true",]

# Printing part of the dataframe
print(paste("Loaded", nrow(dataframe), "clonotypes from",
            length(files), "samples."))
head(dataframe)


# Dataframe containing EHEB variants
ehebs <- data.frame(var = c("EHEB", "EHEB-V1", "EHEB-V2"),
                    seq = c("TGTGCGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG",
                            "TGTGGGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG",
                            "TGTGCGACACATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG"))

# Named vector of EHEB variants (just to compare which form is more convenient)
ehebvec <- c(EHEB = "TGTGCGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG",
             EHEB_V1 = "TGTGGGAGAGATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG",
             EHEB_V2 = "TGTGCGACACATGATGGCGGGGGAAAGGGTGACTACGGAAGACTTTGG")



# Aggregate by CDR3 and discard all differences in V/J germline outside CDR3
df <- dataframe[, c("cdr3nt", "freq", "count")]
print(paste("There are ", length(unique(df$cdr3nt)),
            " unique cdr3s in this dataset"))
dffreq <- as.data.frame(tapply(df$freq, df$cdr3nt, sum))
dfcount <- as.data.frame(tapply(df$count, df$cdr3nt, sum))
df <- cbind(dffreq, dfcount)
df$cdr3nt <- rownames(dffreq)
rownames(df) <- c()
colnames(df) <- c("freq", "count", "cdr3nt")
df <- df[order(df$count, decreasing = TRUE),]


# Dropping some variables
drops2 <- c('mutations.FR1',  'mutations.CDR1', 
            'mutations.FR2', 'mutations.CDR2', 
            'mutations.FR3', 'mutations.FR4',
            'in.frame', 'has.cdr3', 'no.stop', 'complete', 'canonical',
            'cdr.insert.qual', 'mutations.qual', 'v.end.in.cdr3',
            'd.start.in.cdr3', 'd.end.in.cdr3', 'j.start.in.cdr3', 'v.del',
            'd.del.5', 'd.del.3', 'j.del', 'pol.v', 'pol.d.5', 'pol.d.3',
            'pol.j', 'in.frame')
dataframe2 <- dataframe[,!(names(dataframe) %in% drops2)]

