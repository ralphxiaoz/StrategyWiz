
trade.prep.turtle = function(df.trade){
  df.trade$high.ndays = find.max.in.ndays(df.trade$high, maxn)
  df.trade$low.ndays = find.min.in.ndays(df.trade$low, minn)
  df.trade$yst.high = shift(df.trade$high, type = 'lag')
  df.trade$yst.max = shift(df.trade$high.ndays, type = 'lag')
  df.trade = df.trade[complete.cases(df.trade),]
  return(df.trade)
}
