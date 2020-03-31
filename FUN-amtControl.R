
genPos.atr = function(df){
  df$ATR = as.data.frame(ATR(df[, c('open', 'high', 'low', 'close')]))$atr
  return(df)
}


genPos.stoploss = function(df, buy.at = 'open'){
  df$div.by = 8 * df[[buy.at]]
  return(df)
}