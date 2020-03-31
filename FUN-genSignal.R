env.test = new.env()

computeLines = function(df.lines, inds, args = list(s.sma = 20, l.sma = 50)){
  if('macd' %in% inds){
    df.lines = cbind(df.lines, data.frame(MACD(df.lines$close)))
    df.lines$macd.left = shift(df.lines$macd, n = 2, type = 'lag')
    df.lines$signal.left = shift(df.lines$signal, n = 2, type = 'lag')
    df.lines$macd.right = shift(df.lines$macd, n = 1, type = 'lag')
    df.lines$signal.right = shift(df.lines$signal, n = 1, type = 'lag')
  }
  
  if('sma' %in% inds){
    df.lines$s.sma = SMA(df.lines$close, n = args[['s.sma']])
    df.lines$l.sma = SMA(df.lines$close, n = args[['l.sma']])
    df.lines$s.sma.left = shift(df.lines$s.sma, n = 2, type = 'lag')
    df.lines$l.sma.left = shift(df.lines$l.sma, n = 2, type = 'lag')
    df.lines$s.sma.right = shift(df.lines$s.sma, n = 1, type = 'lag')
    df.lines$l.sma.right = shift(df.lines$l.sma, n = 1, type = 'lag')
  }
  
  return(df.lines)
}


# input: df.stock
# output: dates when the buy signal is triggered, along with in/out signal type
genSignal.macd.crx.in = function(df.stock){
  
  df.sig = computeLines(df.stock, inds = 'macd')
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$macd.left < df.sig$signal.left &
           df.sig$macd.right > df.sig$signal.right &
           df.sig$macd.right > 0, 'sig.action'] = 'in'
  
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}

genSignal.macd.crx.out = function(df.stock){

  df.sig = computeLines(df.stock, inds = 'macd')
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$macd.left > df.sig$signal.left & 
           df.sig$macd.right < df.sig$signal.right, 'sig.action'] = 'out'
  
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}


# n: pull out n days after in
genSignal.macd.crx.fixtime.out = function(df.stock, args = list(n = 200)){
  n = args[['n']]
  df.sig = genSignal.macd.crx.in(df.stock)
  out.dates = as.Date(sapply(df.sig$date + n, find.closest.date, dfdate = df.stock$date))
  df.sig = data.frame(date = out.dates, sig.action = 'out')
}


genSignal.sma.crx.in = function(df.stock, args = list(s.sma = 20, l.sma = 50)){

  df.sig = computeLines(df.stock, inds = 'sma', args = args)
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$s.sma.left < df.sig$l.sma.left &
           df.sig$s.sma.right > df.sig$l.sma.right, 'sig.action'] = 'in'
  
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}

genSignal.sma.crx.out = function(df.stock, args = list(s.sma = 20, l.sma = 50)){

  df.sig = computeLines(df.stock, inds = 'sma')
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$s.sma.left > df.sig$l.sma.left &
           df.sig$s.sma.right < df.sig$l.sma.right, 'sig.action'] = 'out'
  
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}

genSignal.sma.sidecrx.in = function(df.stock){
  df.sig = computeLines(df.stock, inds = 'sma')
  df.sig$sma.200 = SMA(df.sig$close, 200)
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$s.sma.left < df.sig$l.sma.left &
           df.sig$s.sma.right > df.sig$l.sma.right &
           df.sig$s.sma.left > df.sig$sma.200, 'sig.action'] = 'in'
  
  # plot(x = df.sig$date, y = df.sig$s.sma, type = 'l')
  # par(new = T)
  # plot(x = df.sig$date, y = df.sig$l.sma, type = 'l')
  # par(new = T)
  # plot(x = df.sig$date, y = df.sig$sma.200, type = 'l')
  
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}

genSignal.sma.sidecrx.out = function(df.stock){
  df.sig = computeLines(df.stock, inds = 'sma')
  df.sig$sma.200 = SMA(df.sig$close, 200)
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$s.sma.left < df.sig$l.sma.left &
           df.sig$s.sma.right > df.sig$l.sma.right &
           df.sig$l.sma.left < df.sig$sma.200, 'sig.action'] = 'out'
  
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}

genSignal.turtle.in = function(df.stock, args = list(maxn = 20, minn = 20)){
  df.sig = df.stock
  df.sig$high.ndays = find.max.in.ndays(df.sig$high, maxn)
  df.sig$low.ndays = find.min.in.ndays(df.sig$low, minn)
  df.sig$yst.high = shift(df.sig$high, type = 'lag')
  df.sig$yst.max = shift(df.sig$high.ndays, type = 'lag')
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$yst.high > df.sig$yst.max, 'sig.action'] = 'in'
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}

genSignal.turtle.out = function(df.stock, args = list(maxn = 20, minn = 20)){
  df.sig = df.stock
  df.sig$high.ndays = runMax(df.sig$high, maxn)
  df.sig$low.ndays = runMin(df.sig$low, minn)
  df.sig$yst.low = shift(df.sig$low, type = 'lag')
  df.sig$yst.min = shift(df.sig$low.ndays, type = 'lag')
  df.sig = df.sig[complete.cases(df.sig),]
  df.sig$sig.action = NA
  
  df.sig[df.sig$yst.low < df.sig$yst.min, 'sig.action'] = 'out'
  return(df.sig[!is.na(df.sig$sig.action),c('date','sig.action')])
}
