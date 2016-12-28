# see: http://www.forexfactory.com/showthread.php?p=9381755#post9381755
# % of equity simple example
# take a series of wl trades where wl is an even number
# with an equal number of wins and losses
bank = 1000; lev = 100; startbank = bank# bank and leverage
trials = 1000
tpsl = 0.002; wl = 1 # make each win/loss 20 pips
while (sum(wl) !=0) wl = sample(c(-tpsl, tpsl), trials, replace=T, prob=c(0.5, 0.5))
eq = rep(0,  length(wl)) # calculate equity over time
size = rep(0, length(wl)) # trade size over time
for (i in 1:length(wl)) {
  size[i] = bank * lev
  bank = bank + size[i] * wl[i] #take a position*wl
  eq[i] = bank
  if (eq[i] < 1) break # allow for early termination when equity declines below 1
}
eq = eq[1:i]; size = size[1:i]
plot(eq, t='l', ylab='Equity'); abline(h=startbank, col='red')