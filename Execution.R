
chartSeries(eval(parse(text = ticker)), name = ticker, type = "candlesticks", up.col = "black", dn.col = "red",
            theme = "white", subset = "last 12 months")

stock_sma_50 <- SMA(Cl(eval(parse(text = ticker))), n = 50)
stock_sma_20 <- SMA(Cl(eval(parse(text = ticker))), n = 20)

addTA(stock_sma_50, on = 1, col = "blue")
addTA(stock_sma_20, on = 1, col = "red")


# # plotly cuts the graph when zoom, buggy
# plot_ly(data = df.stock, type = "candlestick",
#         open = df.stock$open, close = df.stock$close, high = df.stock$high, low = df.stock$low,
#         increasing = list(line = list(color = 'black')),
#         decreasing = list(line = list(color = 'red')))

infunction = genSignal.sma.crx.in
outfunction = genSignal.sma.crx.out

run.result = runStrategy(df.stock, inFUN = infunction, outFUN = outfunction)

# plot buy/sell
df.sig = rbind(infunction(df.stock), outfunction(df.stock))

# # try plotly
# ay <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "right",
#   title = "second y axis"
# )
# 
# p = plot_ly() %>%
#   add_lines(x = run.result$date, y = run.result$close, name = 'c') %>%
#   add_lines(x = run.result$date, y = run.result$ROI, name = 'R', yaxis = 'y2') %>%
#   add_lines(x = run.result$date, y = run.result$base.ROI, name = 'bR', yaxis = 'y2') %>%
#   layout(yaxis2 = ay)

plot(x = run.result$date, y = run.result$close, type='l', xlab = 'Time', ylab = 'Close')
par(new = T)
plot(x = run.result$date, y = run.result$ROI, type = 'l', axes = F, xlab = '', ylab = '', 
     ylim = c(min(run.result$ROI, run.result$base.ROI), max(run.result$ROI, run.result$base.ROI)), col = 'blue')

axis(side=4, at = pretty(range(min(run.result$ROI, run.result$base.ROI), max(run.result$ROI, run.result$base.ROI))), col = 'blue')

abline(v = df.sig[df.sig$sig.action=='in', 'date'])
abline(v = df.sig[df.sig$sig.action=='out', 'date'], col = 'red', lty = 2)



# plot e-ratio
eval.result = list()
eval.range = 1:100
for(n in eval.range){
  i = n - min(eval.range) + 1
  eval.result[[i]] = eval.eratio(df.stock, genSignal.sma.crx, n = n)
}
plot(x = eval.range, y = eval.result, type = 'l')

