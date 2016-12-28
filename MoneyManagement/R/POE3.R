# see: http://www.forexfactory.com/showthread.php?p=9381838#post9381838
# % of equity comparison with two curves example
# take a series of wl trades where wl is an even number
# with an equal number of wins and losses
bank = 1000; lev = 100; startbank = bank# bank and leverage
bank2 = bank # compute opposite signals for this account
trials = 1000
tpsl = 0.002; wl = 1 # make each win/loss 20 pips
while (sum(wl) !=0) wl = sample(c(-tpsl, tpsl), trials, replace=T, prob=c(0.5, 0.5))
eq = rep(0,  length(wl)) # calculate equity over time
size = rep(0, length(wl)) # trade size over time
eq2 = eq; size2 = size # compute equity, size variables for opposite
for (i in 1:length(wl)) {
  size[i] = bank * lev;   size2[i] = bank2 * lev
  bank = bank + size[i] * wl[i] #take a position*wl
  bank2 = bank2 + size2[i] *-wl[i] # do opposite trades
  eq[i] = bank; eq2[i] = bank2
  if (eq[i] + eq2[i] < 1) break # allow for early termination when equity declines
}
eq = eq[1:i]; size = size[1:i]
eq2 = eq2[1:i]; size2 = size2[1:i]
blackplot(3,1)
plot(eq, t='l', ylab='Equity'); abline(h=startbank, col='red')
plot(eq2, t='l', ylab='Equity #2'); abline(h=startbank, col='red')
plot((eq+eq2)/2, t='l', ylab='Combined Equity'); abline(h=startbank, col='red')
  
