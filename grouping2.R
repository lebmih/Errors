## Grouping script
# This script groups the dataframe by a specified column and returns the grouped and ordered dataframe

df1 <- data.frame(freq = sample(1:100/100, size = 10),
                  seq = sample(c("cctg", "atgt", "ggtg", "acca"), size = 10, replace = TRUE),
                  count = sample(1:100, size = 10),
                  mutations.CDR3 = sample(c("S327:T>C", "S289:C>G", "S300:A>T"), size = 10, replace = TRUE),
                  mutations.FR1 = sample(c("S20:T>C", "S29:C>G", "S30:A>T"), size = 10, replace = TRUE),
                  stringsAsFactors = FALSE)

groupby <- function(dataframe, by, aggregate.cols){
  t <- table(dataframe[,by])
  t <- t[order(t, decreasing = TRUE)]
  
  df <- dataframe[1:length(t),]
  
  for (i in seq_along(t)){
    seq <- names(t[i])
    l <- list()
    
    freq <- sum(dataframe$freq[dataframe$seq == seq])
    count <- sum(dataframe$count[dataframe$seq == seq])
   
    
    
    df[i,] <- list(freq, seq, count)
  }
  
  t
  
}
  
df2 <- groupby(df1, "seq")

df3 <- df2[order(df2$count, decreasing = TRUE),]