
trade.record = data.frame(date = as.Date(character()),
                          symbol = character(),
                          type = character(),
                          nshare = integer(),
                          price = double(),
                          trade.note = character(),
                          strategy.note = character(),
                          stringsAsFactors = F)


# determine if a trade can happen, returns T/F
can.trade = function(df, type, n, price){
  
  r = nrow(df)
  
  if(type == 'b'){
    if(abs(n) * price < df[r, 'balance'])
      return(T)
    return(F)
  }
  
  if(type == 's'){
    if(abs(n) <= df[r, 'holds'])
      return(T)
    return(F)
  }
  
  if(type == 'c'){
    if(df[r, 'holds'] > 0)
      return(T)
    return(F)
  }
}


trade = function(date, history, symbol, n, price, type = c('b', 's', 'c')){
  
  if(!can.trade(history, type, n, price))
    return()
  
  if(type == 'c' || type == 's')
    n = -n
  
  history[nrow(history)+1, 'date'] = date
  history[nrow(history)+1, 'symbol'] = symbol
  history[nrow(history)+1, 'type'] = type
  history[nrow(history)+1, 'nshare'] = n
  history[nrow(history)+1, 'price'] = price
}

