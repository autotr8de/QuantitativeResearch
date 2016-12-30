
expectancy <- function(p, reward=1, risk=1, numTrades=100) {
  outcomes <- runif(numTrades, min=0, max=1)
  winners <- outcomes <= p
  losers <- outcomes > p
  structure(
    (sum(winners*reward) - sum(losers*risk))/numTrades,
    data=data.frame(
      pr.win= p,
      outcomes.pr=outcomes,
      outcome=outcomes <= p,
      pay.off = reward,
      risk = risk
    ),    
    class='expectancyAnalysis'
  ) 
}

expectancyMC <- function(
      p, payoff.range=c(0.1, 10), payoff.mean=2, payoff.probs=NA, risk=1, 
      numTrades=100, numSimulations=10000
) {
  
  payoffs <- seq(payoff.range[1], payoff.range[2], 0.1)  
  if (is.na(payoff.probs)) {
    payoff.probs <- c(
      dnorm(payoffs[payoffs < payoff.mean],payoff.mean,payoff.mean/4.0),
      dnorm(payoffs[payoffs >= payoff.mean],payoff.mean,payoff.mean/2.0)
    )
  }
  result <- sapply(
      1:numSimulations, 
      function(x) expectancy(
          p, sample(payoffs, numTrades, T, payoff.probs), risk, numTrades), 
      simplify=FALSE
  )
  attr(result, "payoffs") <- payoffs
  attr(result, "payoff.probs") <- payoff.probs

  result
}

expectationAnalysis <- function(
  experimentName, prWin, numTrades, 
  payoff.range,  payoff.mean,
  risk=1, openingBalance=100, simulations=100000, outputPath="~/tmp",
  saveEquityCurves=F, doEquityCurveAnalysis=T
) {
  result <- expectancyMC(p=prWin, payoff.range=payoff.range,  payoff.mean=payoff.mean, risk=risk, numTrades=numTrades, numSimulations=simulations)
  
  # Mean expectation from the simulations
  theExps <- as.numeric(result)
  meanExp <- mean(theExps)
  cat("Mean expectancy is: ", meanExp, "\n")
  if (doEquityCurveAnalysis) {
    # Calculate equity curves
    # Assumption that risk express a percentage of capital e.g. risk = 1 implies each trade risk 1% etc.
    equityCurves <- sapply(
      result,
      function(x) {        
        data <- attr(x, 'data')
        balances <- c(openingBalance)            
        for (idx in 2:nrow(data)) {
          if ( balances[idx-1] <= 0 ) {
            newBalance <- 0
          } else {
            newBalance <- ifelse(data$outcome[idx-1], 
                                 balances[idx-1]*(1 + data$pay.off[idx-1]/100), 
                                 balances[idx-1]*(1 - data$risk[idx-1]/100))
          }
          balances <- c(balances, newBalance)
        }
        balances
      }
    )
        
    # Plot equity curves
    col.best <- rgb(0, 204/255, 51/255)
    col.average <- rgb(51/255, 153/255, 1)
    col.worst <-rgb(154/255, 0, 51/255)
    col.plume <- rgb(0.8, 0.8, 0.8, .5)
    png(
      paste(outputPath, paste(experimentName, "_equitycurves.png", sep=''), sep='/'), 
      width=1920, height=1080
    ) 
    best <- which(equityCurves[100,] == max(equityCurves[100,]))[1]
    worst <- which(equityCurves[100,] == min(equityCurves[100,]))[1]
    plot(
      x=c(1, numTrades), y=range(c(0, pretty(equityCurves))), type='n', 
      main=paste("Equity curves (Experiment: ", experimentName, ")", sep=""), 
      xlab='Trade', ylab='Equity ($)'
    )
    abline(h=openingBalance, lwd=1, col='black')
    lower <- apply(equityCurves, 1, min)
    upper <- apply(equityCurves, 1, max)
    polygon(c(1:numTrades, numTrades:1), c(lower, rev(upper)), col=col.plume, border=NA)
    lines(x=1:numTrades, y=equityCurves[, best], lwd=3, col=col.best)
    lines(x=1:numTrades, y=apply(equityCurves, 1, mean), lwd=2, lty=2, col=col.average)  
    lines(x=1:numTrades, y=equityCurves[, worst], lwd=3, col=col.worst)
    leg1 <- legend(
      "topleft", 
      c("Best", "Worst", "Average", "Data range"), 
      col=c(col.best, col.worst, col.average, col.plume), bty='n', lty=c(1,1,2,1), lwd=c(2,2,2,0), 
      pch=c(NA,NA,NA, 15), cex=2, pt.cex=2
    )    
    dev.off()
    
    
    # Save equity curves?
    if (saveEquityCurves) {
      write.csv(equityCurves, paste(outputPath, paste(experimentName, "_equitycurves.csv", sep=''), sep='/'))
    }    
  }  
  
  # Plot expectation results
  sdExp <- sd(theExps)
  
  png(
    paste(outputPath, paste(experimentName, "_expectancy_hist.png", sep=''), sep='/'), 
    width=1920, height=1080
  )
  expDensity <- density(theExps)
  xmax <- max(expDensity$x)
  hist(theExps, prob=T, xlab=sprintf("Expectancy (p=%3.2f)",prWin),  
       main=sprintf("Experiment %s expectancy distribution (E=%3.3f, N=%d)", experimentName, meanExp, simulations),
       pty='m', xlim = c(0, round(xmax) + sign(xmax) * 0.5)
  )
  abline(v=meanExp, lty=1, lwd=5, lend=1, col=rgb(1,0,0,0.5))
  abline(v=meanExp-c(1:2)*sdExp, lwd=5, lend=1, col=rgb(0,1,0,0.5))
  lines(expDensity, lty=2, lwd=2, col='blue')
  text(
    meanExp-c(1:2)*sdExp, par()$usr[4] *.8, 
    c(expression(-sigma), expression(-2*sigma)), adj=c(0,-.6)
  )  
  dev.off()
  
  # Save the expectancies
  write.csv(theExps, paste(outputPath, paste(experimentName, "_expectancies.csv", sep=''), sep='/'))
  
  # Plot a histogram of pay off probabilties
  png(
    paste(outputPath, paste(experimentName, "_payoff_hist.png", sep=''), sep='/'), 
    width=1920, height=1080
  )  
  hist(
    sample(attr(result, "payoffs"), numTrades, T, attr(result, "payoff.probs")), 
    prob=T, xlab = "Payoff",
    main=paste("Histogram  of payoff samples (N=", numTrades, ")", sep="")
  )
  dev.off()
}

# Close any open graphical devices (useful for script runtime errors etc).
while (! is.null(dev.list()) ) dev.off()

outputPath <- "./output"
for (prWin in seq(0.25, 0.90, 0.05)) {
  cat("Pr(win) =", prWin, "\n")
  result <- expectationAnalysis(
    experimentName=sprintf("%dperc_variaible_payoff", as.integer(prWin*100)),
    prWin=prWin, numTrades=100, 
    payoff.range=c(0.1,10),  payoff.mean=2,
    risk=1, openingBalance=100, simulations=10000, outputPath=outputPath,
    saveEquityCurves=T, doEquityCurveAnalysis=T    
  )
}
