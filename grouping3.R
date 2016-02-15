groupby <- function(dataframe, by, aggregatecols){
  t <- table(dataframe[,by])
  t <- t[order(t, decreasing = TRUE)]
  df <- dataframe[1:length(t),]
  
  for(i in seq_along(t)){
    sequence <- names(t[i])
    
    # Aggregating columns specified by aggregatecols argument (sum)
    for(col in aggregatecols){
      df[i, col] <- sum(dataframe[dataframe[,by] == sequence, col])
    }
    
    # Assigning values to the rest of the columns
    for(n in names(dataframe)[!(names(dataframe) %in% aggregatecols)]){
      df[i, n] <- dataframe[dataframe[,by] == sequence, n][1]
    }
  }
  
  df
}

