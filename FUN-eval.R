
eval.eratio = function(df.stock, FUN, n = 50){
  
  df.eval = FUN(df.stock)
  df.eval = merge(df.stock, df.eval, all = T)
  df.eval$ATR = (as.data.frame(ATR(df.eval[,c('high','low','close')])))$atr
  df.eval = df.eval[complete.cases(df.eval$ATR),]
  
  df.eval$MAE = 0
  df.eval$MFE = 0
  boolvec.buy = (df.eval$sig.action == 'in') %in% TRUE
  
  if(sum(boolvec.buy) == 0)
    stop(paste("insufficient buy actions:", sum(boolvec.buy)))
  
  if(df.eval[which(boolvec.buy)[3], 'date'] + n > max(df.eval$date)) 
    stop(paste("n is too large, not enough buy actions. Date boundary:",
               max(df.eval$date),
               "Date needs to extend to:",
               df.eval[which(boolvec.buy)[3], 'date'] + n))
  
  # loop through rows where buy action happens
  for(r in which(boolvec.buy)){
    
    start = df.eval[r, 'date']
    end = start + n
    atr = df.eval[r, 'ATR']
    buy.price = df.eval[r, 'close']
    high.price = max(df.eval$close[df.eval$date >= start & df.eval$date <= end])
    low.price = min(df.eval$close[df.eval$date >= start & df.eval$date <= end])
    
    # max adverse excursion
    MAE = ifelse(low.price < buy.price, abs(low.price - buy.price), 0)
    df.eval[r, 'MAE'] = MAE/atr
    
    # max favorable excursion
    MFE = ifelse(high.price > buy.price, high.price - buy.price, 0)
    df.eval[r, 'MFE'] = MFE/atr
    
    # debug print
    # print(paste("Buy @", buy.price, ". From", start, "to", end, "highest price", high.price, "lowest price, ", low.price))
  }
  
  avgMFE = sum(df.eval$MFE[is.finite(df.eval$MFE)]) / sum(boolvec.buy)
  avgMAE = sum(df.eval$MAE[is.finite(df.eval$MAE)]) / sum(boolvec.buy)
  e.ratio = avgMFE / avgMAE
  return(e.ratio)
}
