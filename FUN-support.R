round.df = function(df){
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])) df[,i] = round(df[,i],3)
  }
  return(df)
}

# find the date in df that's closest to target date (the earlier one)
find.closest.date = function(date, dfdate){
  return(max(dfdate[dfdate - date <= 0]))
}

# gather stas of a stock df
calculate.stats = function(df){
  result = data.frame('positive.net.gain.days' = 0, 'positive.net.gain.days.pct' = 0)
  result$positive.net.gain.days = sum(df$net.gain > 0)
  result$positive.net.gain.days.pct = sum(df$net.gain > 0) / nrow(df)
  return(result)
}

transform.df.stock = function(df.stock, symbol){
  
  df.stock = data.frame(date = index(df.stock), coredata(df.stock))
  df.stock = df.stock[complete.cases(df.stock),]
  
  colnames(df.stock) = c('date', 'open', 'high', 'low', 'close', 'volume', 'adj.close')
  
  df.stock$symbol = symbol
  
  return(df.stock)
}
