# see www.forexfactory.com/showthread.php?p=9380892#post9380892
# % of equity simple example
# take a series of wl trades where wl is an even number
# with an equal number of wins and losses
bank = 1000; lev = 1 # bank and leverage
 
wl = c(1,-1,1,-1) # 1 is a win, -1 is a loss
tpsl = 0.002; wl = wl * tpsl # make each win/loss 20 pips
 
eq = rep(0,  length(wl)) # calculate equity over time
size = rep(0, length(wl)) # trade size over time
for (i in 1:length(wl)) {
  size[i] = bank * lev
  bank = bank + size[i] * wl[i]
  eq[i] = bank
}
cbind(wl, size, eq)