# see: http://www.forexfactory.com/showthread.php?p=9380834#post9380834
end = 100000; eq = rep(0, end)
trials = 1
for (i in 1:end) {
  #eq[i] = sign(sum(sample(c(-1,1), trials, replace=T)))
  #eq[i] = sign(sum(runif(trials)-0.5))
  eq[i] = sign(sum(sign(rnorm(trials))))
}
blackplot(2,1); plot(cumsum(eq), t='l')
abline(h=0, col='red')
plot(density(cumsum(eq)), col='gold', lwd = 3)
abline(v=0, col='red')