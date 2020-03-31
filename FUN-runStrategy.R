
# friction: slippage or trasaction fee. Input in pct format
runStrategy = function(df.stock, inFUN, outFUN, args = list(), friction = 0){
  
  # if args is passed, run with args, else run with defaults
  # can't use ifelse here. The output seems to be dependant on 'test' part
  # see https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/ifelse
  if(length(args) > 0)
    df.sig = rbind(inFUN(df.stock, args), outFUN(df.stock, args))
  else
    df.sig = rbind(inFUN(df.stock), outFUN(df.stock))
  
  # in/out signals happens at the same day
  # use out over in signal
  if(length(unique(df.sig$date)) != length(df.sig$date))
    df.sig = subset(df.sig, !(date %in% df.sig$date[duplicated(df.sig$date)] & sig.action == 'in'))
  
  df.sig = df.sig[order(df.sig$date),]
  df.trade = merge(df.stock, df.sig, all = T)
  
  # position control strategy
  # genPos functions return the number that divides balance, then yields amount of shares to buy
  # this is only a static method. If want to make it dynamic, will need to define/call within the for loop
  df.pos = genPos.stoploss(df.trade)
  
  df.trade$action = NA
  df.trade$action.type = NA
  df.trade$balance = 20000
  df.trade$trade = 0
  df.trade$trade.value = 0
  df.trade$holds = 0
  df.trade$market.value = 0
  df.trade$net.gain = 0
  df.trade$total.cash.flow = 0
  df.trade$avg.CPS = 0
  df.trade$base.ROI = df.trade$adj.close / df.trade$adj.close[1] - 1
  
  # since trade won't happen anywhere else, it's probably better just stay in runStrategy function
  # here df.trade only trades when there is a signal. But df.sig is generated based on OHLC data only. How does trade history factor in??
  for(r in 2:nrow(df.trade)){
    
    buy.price = df.trade[r, 'open'] * (1 + friction)
    sell.price = df.trade[r, 'open'] * (1 - friction)
    
    # sell amount should be NEGATIVE
    # *** in order to dynamically control how many to buy/sell, I might as well still pass df.trade into buy/sell.amount
    buy.amount = floor((df.trade[r-1, 'balance']/df.pos[df.pos$date == df.trade[r, 'date'], 'div.by']))
    sell.amount = -floor(df.trade[r-1, 'holds'])
    
    # MIND THE ORDER TO UPDATE FIELDS IN BUY/SELL ACTIONS
    if(ifelse(is.na(df.trade[r, 'sig.action']),F,df.trade[r, 'sig.action'] == 'in') &&
       df.trade[r-1, 'balance'] >= buy.price * buy.amount &&
       buy.amount > 0){
      df.trade[r, 'action'] = 'b'
      df.trade[r, 'action.type'] = ifelse(df.trade[r-1, 'holds'] == 0, 'open', 'add')
      # # debugging
      # print(paste("@ row",r,"buy amount is",buy.amount))
      df.trade[r, 'trade'] = buy.amount
      df.trade[r, 'trade.value'] = -buy.price * buy.amount
      df.trade[r, 'holds'] = df.trade[r-1, 'holds'] + buy.amount
      df.trade[r, 'avg.CPS'] = ifelse(df.trade[r, 'action.type'] == 'open',
                                      buy.price,
                                      (abs(df.trade[r, 'trade.value']) + df.trade[r-1, 'avg.CPS'] * df.trade[r-1, 'holds']) / df.trade[r, 'holds'])
      df.trade[r, 'balance'] = df.trade[r-1, 'balance'] + df.trade[r, 'trade.value']
    }
    
    else if(ifelse(is.na(df.trade[r, 'sig.action']),F,df.trade[r, 'sig.action'] == 'out') &&
            df.trade[r-1, 'holds'] >= max(abs(sell.amount), 1) &&
            sell.amount < 0){
      df.trade[r, 'action'] = 's'
      df.trade[r, 'action.type'] = ifelse(df.trade[r-1, 'holds'] == abs(sell.amount), 'close', 'reduce')
      df.trade[r, 'trade'] = sell.amount
      df.trade[r, 'trade.value'] = -sell.price * sell.amount
      df.trade[r, 'holds'] = df.trade[r-1, 'holds'] + sell.amount
      df.trade[r, 'avg.CPS'] = ifelse(df.trade[r, 'action.type'] == 'close', 0, df.trade[r-1, 'avg.CPS'])
      df.trade[r, 'balance'] = df.trade[r-1, 'balance'] + df.trade[r, 'trade.value']
    } 
    else{
      df.trade[r, 'holds'] = df.trade[r-1, 'holds']
      df.trade[r, 'balance'] = df.trade[r-1, 'balance']
      df.trade[r, 'avg.CPS'] = df.trade[r-1, 'avg.CPS']
    }
    
    # Technically these can be calculated outside for loop. Buw will they be useful?
    df.trade[r, 'market.value'] = df.trade[r, 'holds'] * df.trade[r, 'close']
    df.trade[r, 'total.cash.flow'] = ifelse(r > 1, df.trade[r-1, 'total.cash.flow'], 0) + df.trade[r, 'trade.value']
    df.trade[r, 'net.gain'] = df.trade[r, 'market.value'] + df.trade[r, 'total.cash.flow']
    
  }
  
  df.trade$ROI = (df.trade$balance + df.trade$market.value) / df.trade$balance[1] - 1
  
  df.trade = round.df(df.trade)
  return(df.trade)
}
