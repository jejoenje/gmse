source('gui/goose_predict_gui.R')

example_file <- "/home/jm241/Documents/docs/000_ConFooBio/gmse/data/example data.csv"

alldat <- goose_clean_data(example_file)
dat_1999 <- alldat[1:13,]

alldat_paras <- get_goose_paras(alldat)
dat_1999_paras <- get_goose_paras(dat_1999)

par(mfrow=c(2,2))

plot(alldat$Year, alldat$Count, type='p', pch=21, col='black', bg='grey', main='Predictions using data <2000 only', xlab='Year', ylab='Population count')
# This is the prediction using pars estimated from data up to 1999 only (ie before shooting)
lines(alldat$Year, goose_pred(dat_1999_paras$par, alldat), col='red', lwd=0.75)     
# The dashed horizontal red line represents the carrying capacity estimated from this period's data only:
abline(a=mean(dat_1999$AIG)*dat_1999_paras$par[2], b=0, lty='dashed', col='red')
abline(a=min(dat_1999$AIG)*dat_1999_paras$par[2], b=0, lty='dotted', col='red', lwd=0.5)
abline(a=max(dat_1999$AIG)*dat_1999_paras$par[2], b=0, lty='dotted', col='red', lwd=0.5)

plot(alldat$Year, alldat$Count, type='p', pch=21, col='black', bg='grey', main='Predictions using all available data', xlab='Year', ylab='Population count')
# This is the prediction using pars from all available years
lines(alldat$Year, goose_pred(alldat_paras$par, alldat), col='blue', lwd=0.75)     
# The dashed horizontal red line represents the carrying capacity estimated from this period's data only:
abline(a=mean(alldat$AIG)*alldat_paras$par[2], b=0, lty='dashed', col='blue')
abline(a=min(alldat$AIG)*alldat_paras$par[2], b=0, lty='dotted', col='blue', lwd=0.5)
abline(a=max(alldat$AIG)*alldat_paras$par[2], b=0, lty='dotted', col='blue', lwd=0.5)

plot(alldat$Year, alldat$Count, type='p', pch=21, col='black', bg='grey', main='Prediction comparison', xlab='Year', ylab='Population count')
lines(alldat$Year, goose_pred(dat_1999_paras$par, alldat), col='red', lwd=0.75)     
lines(alldat$Year, goose_pred(alldat_paras$par, alldat), col='blue', lwd=0.75)     
