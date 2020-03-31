library(quantmod)
library(magrittr)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)
library(TTR)
library(data.table)
library(dplyr)
library(plotrix)

source('FUN-genSignal.R')
source('FUN-support.R')
source('FUN-runStrategy.R')
source('FUN-eval.R')
source('FUN-amtControl.R')


# ************************************ Initiation ************************************

start <- as.Date("2008-01-01")
# end <- as.Date("2018-06-30")
end <- Sys.Date()
time.unit = 'daily'

# define multiple start/end dates for evaluation
start.year = 2015
end.year = 2018
list.start.date = list()
list.end.date = list()

for(i in 1:12){
  list.start.date[[i]] = as.Date(paste(start.year, '-', i, '-',1, sep = ''))
  list.end.date[[i]] = as.Date(paste(end.year, '-', i, '-',1, sep = ''))
}

list.end.date = list.end.date[list.end.date < Sys.Date()]

# *************************** Single ticker ***************************

ticker = "XOP"
getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = T, periodicity = time.unit)
df.stock = transform.df.stock(eval(parse(text = ticker)), symbol = ticker)

list.df.stock.diff.dates = list()
list.df.stock.init = transform.df.stock(getSymbols(ticker, src = "yahoo", from = list.start.date[[1]],
                                                   to = list.end.date[[length(list.end.date)]], auto.assign = F, periodicity = time.unit),
                                        symbol = ticker)

for(s in list.start.date){
  for(e in list.end.date){
    list.df.stock.diff.dates[[paste(s, e)]] = subset(list.df.stock.init, date >= s, date <= e)
  }
}

# *************************** Multi ticker ***************************

env.stocks.origin = new.env()

# some stocks need to be cleaned up before use. eg. HMNY
list.ticker = c('BABA', 'JD', 'MSFT', 'AAPL', 'FB', 'W', 'TWTR', 'BIDU',
                'CSCO', 'AMC', 'WMT', 'DIS', 'XOP',
                'MU', 'AMD', 'NVDA', 'INTC', 'ORCL', 'IBM',
                'AMRS',
                'TSLA')

sapply(list.ticker, function(x){
  try(
    getSymbols(
      x,
      src ="yahoo",
      from =start,
      to = end,
      periodicity = time.unit,
      env = env.stocks.origin),
    silent=TRUE)
})
