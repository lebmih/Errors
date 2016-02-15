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



# Aggregate by CDR3 and discard all differences in V/J germline outside CDR3

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

dataframe2 <- dataframe2[order(dataframe2$cdr3nt, dataframe2$cdr3aa, dataframe2$v, dataframe2$d, dataframe2$j),]