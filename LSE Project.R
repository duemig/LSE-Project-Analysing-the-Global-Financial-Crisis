#~ 1. Setup 

# Clear Global Environment
rm(list=ls(all=TRUE)) 

# Load Libraries
library(QRM)
library(xts)
library(GGally)
library(ggplot2)
library(PerformanceAnalytics)

# Create functions

# Multiple plot function

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.

# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title="") {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (nchar(title)>0){
    layout<-rbind(rep(0, ncol(layout)), layout)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), heights =if(nchar(title)>0){unit(c(0.5, rep(5,nrow(layout)-1)), "null")}else{unit(c(rep(5, nrow(layout))), "null")} )))
    
    # Make each plot, in the correct location
    if (nchar(title)>0){
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)))
    }
    
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Set As Working Directory (can be found under More) --> 
setwd("~/Desktop/LSE/Project")

# Read CSV
dataraw_df<-read.csv("stocks.csv")

# Seperate unique ids
ids<-unique(dataraw_df[,1])

# Count stocks_df
nAssets<-length(ids)

# Count observations
nObs<-nrow(dataraw_df)/nAssets

# Create matrix
data_m<-matrix(NA,nObs,nAssets+1)
data_m[,1]<-dataraw_df[dataraw_df[,1]==ids[1],2]
for (i in 1:nAssets){
  data_m[,i+1]<-dataraw_df[dataraw_df[,1]==ids[i],3]
}

# Name Ticker
ticker<-c("Dates", "Price.KO", "Price.XOM", "Price.GE", "Price.JNJ", "Price.DIS", "Price.JPM", "Price.INTC", "Price.MS", "Price.CCC", "Price.GS")
colnames(data_m)[1:(nAssets+1)]<-ticker




#~ 2. Total Period (2006-07-01 to 2009-06-30)

#~ Stock Price Dynamics

# Create a Data Frame
stocks_df <- as.data.frame(data_m)
stocks_df$Dates <- as.Date(as.character(stocks_df$Dates),"%Y%m%d")

#Create Plots
plot1<- ggplot(stocks_df, aes(x=Dates, y=Price.KO)) +
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Coca Cola") +
  xlab("") + 
  ylab("") 

plot2<- ggplot(stocks_df, aes(x=Dates, y=Price.XOM)) +
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Exxon") +
  xlab("") + 
  ylab("") 

plot3<- ggplot(stocks_df, aes(x=Dates, y=Price.GE)) +
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("General Electric") +
  xlab("") + 
  ylab("") 

plot4<- ggplot(stocks_df, aes(x=Dates, y=Price.JNJ))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Johnson & Johnson") +
  xlab("") + 
  ylab("") 

plot5<- ggplot(stocks_df, aes(x=Dates, y=Price.DIS))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Disney") +
  xlab("") + 
  ylab("") 

plot6<- ggplot(stocks_df, aes(x=Dates, y=Price.JPM))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("JPM") +
  xlab("") + 
  ylab("") 

plot7<- ggplot(stocks_df, aes(x=Dates, y=Price.INTC))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Intel") +
  xlab("") + 
  ylab("") 

plot8<- ggplot(stocks_df, aes(x=Dates, y=Price.MS))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Morgan Stanley") +
  xlab("") + 
  ylab("") 

plot9<- ggplot(stocks_df, aes(x=Dates, y=Price.CCC))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Citigroup") +
  xlab("") + 
  ylab("") 

plot10<- ggplot(stocks_df, aes(x=Dates, y=Price.GS))+
  geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2008-03-14")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-09-15")), col="red", linetype="dotted", size=0.7) +
  geom_vline(xintercept=as.numeric(as.Date("2008-10-15")), col="red", linetype="dotted", size=0.7) +
  ggtitle("Goldman Sachs") +
  xlab("") + 
  ylab("") 

multiplot(plot1,plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, cols=2, title="")

#~ Returns

# Compute simple absolute returns
returns.a_df <- as.data.frame(lapply(stocks_df[,2:ncol(stocks_df)], diff))
colnames(returns.a_df) <- c("raKO", "raXOM", "raGE", "raJNJ", "raDIS", "raWFC", "raINTC", "raMS", "raCCC", "raGS")

# Compute simple relative returns
returns.r_df <- as.data.frame(returns.a_df/stocks_df[,2:ncol(stocks_df)][-nrow(stocks_df),])
colnames(returns.r_df) <- c("rrKO", "rrXOM", "rrGE", "rrJNJ", "rrDIS", "rrWFC", "rrINTC", "rrMS", "rrCCC", "rrGS")

# Compute log prices
logprices_df <- as.data.frame(lapply(stocks_df[,2:ncol(stocks_df)], log))
colnames(logprices_df) <- c("logpKO", "logpXOM", "logpGE", "logpJNJ", "logpDIS", "logpWFC", "logpINTC", "logpMS", "logpCCC", "logpGS")

# Compute log returns
logreturns_df <- as.data.frame(lapply(logprices_df, diff))
colnames(logreturns_df) <- c("logrKO", "logrXOM", "logrGE", "logrJNJ", "logrDIS", "logrWFC", "logrINTC", "logrMS", "logrCCC", "logrGS")

# Discard the first price observation and combine data
stocks.combi_df <- cbind(stocks_df[-1,], returns.r_df, returns.a_df, logprices_df[-1,], logreturns_df)

# Plot time series of log returns
p1<- ggplot(stocks.combi_df, aes(x=Dates, y=logrKO))+
  geom_line() + 
  ggtitle("Coca Cola") +
  xlab("") + 
  ylab("") 

p2<- ggplot(stocks.combi_df, aes(x=Dates, y=logrXOM))+
  geom_line() + 
  ggtitle("Exxon") +
  xlab("") + 
  ylab("") 

p3<- ggplot(stocks.combi_df, aes(x=Dates, y=logrGE))+
  geom_line() + 
  ggtitle("General Electric") +
  xlab("") + 
  ylab("") 

p4<- ggplot(stocks.combi_df, aes(x=Dates, y=logrJNJ))+
  geom_line() + 
  ggtitle("Johnson & Johnson") +
  xlab("") + 
  ylab("") 

p5<- ggplot(stocks.combi_df, aes(x=Dates, y=logrDIS))+
  geom_line() + 
  ggtitle("Disney") +
  xlab("") + 
  ylab("") 

p6<- ggplot(stocks.combi_df, aes(x=Dates, y=logrWFC))+
  geom_line() + 
  ggtitle("JPM") +
  xlab("") + 
  ylab("") 
     
p7<- ggplot(stocks.combi_df, aes(x=Dates, y=logrINTC))+
  geom_line() + 
  ggtitle("Intel") +
  xlab("") + 
  ylab("") 

p8<- ggplot(stocks.combi_df, aes(x=Dates, y=logrMS))+
  geom_line() + 
  ggtitle("Morgan Stanley") +
  xlab("") + 
  ylab("") 

p9<- ggplot(stocks.combi_df, aes(x=Dates, y=logrCCC))+
  geom_line() + 
  ggtitle("Citigroup") +
  xlab("") + 
  ylab("") 

p10<- ggplot(stocks.combi_df, aes(x=Dates, y=logrGS))+
  geom_line() + 
  ggtitle("Goldman Sachs") +
  xlab("") + 
  ylab("") 

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, cols=2, title="")

#~ Correlation

# Plot scatter plots of log return pairs
ggpairs(logreturns_df, aes(alpha = 0.1)) +
        ggplot2::labs(title = "2006-07-01 to 2009-06-30")

# Plot corr plot of log return pairs
ggcorr(logreturns_df, palette = "RdYlGn", name = "rho", 
       label = FALSE) +
       ggplot2::labs(title = "2006-07-01 to 2009-06-30")




#~ 3. Portfolio Change over total Period (2006-07-01 to 2009-06-30)

# Select returns of portfolio components
returns.components.pf_df <- stocks.combi_df[, c("Dates","rrKO", "rrXOM", "rrGE", "rrJNJ", "rrDIS", "rrWFC", "rrINTC", "rrMS", "rrCCC", "rrGS")]
colnames(returns.components.pf_df) <- c("Dates", "KO", "XOM", "GE", "JNJ", "DIS", "JPM", "INTC", "MS", "CCC", "GS")

# Convert to xts
returns.components.pf_xts <- xts(returns.components.pf_df[,-1], order.by=returns.components.pf_df[,1])

# Define portfolio weights
weights.pf <- c(0.1,0.1,0.1,0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

# Define portfolio value
value.pf <- 10000

# Compute portfolio changes
change.pf_xts <- Return.portfolio(returns.components.pf_xts[,1:10], weights=weights.pf, verbose=TRUE, value=value.pf)

# Compute absolute daily canges of components
return.a.components.pf_xts <- change.pf_xts$EOP.Value-change.pf_xts$BOP.Value

# Compute absolute daily canges of portfolio
return.a.pf_xts <- .xts(x = rowSums(return.a.components.pf_xts), .index(return.a.components.pf_xts))

# Plot CumReturns Returns
chart.CumReturns(change.pf_xts$returns, main= "Portfolio return",  ylab = "")

# Plot weight changes
chart.StackedBar(change.pf_xts$BOP.Weight, main= "Portfolio weights", ylab = "")

# Plot value changes
chart.StackedBar(change.pf_xts$BOP.Value, main= "Portfolio value",  ylab = "")




#~ 4. Period 1 (2006-07-01 to 2007-12-31) --> VaR and ES Computation

#~ Correlation

# Select data for Period 1 
stocks.combi.1_df <- stocks.combi_df[stocks.combi_df$Dates >= "2006-07-01" & stocks.combi_df$Dates <= "2007-12-31",]

# Select log returns
logreturns.1_df <- stocks.combi.1_df[, c("logrKO", "logrXOM", "logrGE", "logrJNJ", "logrDIS", "logrWFC", "logrINTC", "logrMS", "logrCCC", "logrGS")]

# Plot scatter plots of log return pairs
ggpairs(logreturns.1_df, aes(alpha = 0.1)) +
  ggplot2::labs(title = "06-07-01 to 2007-12-31") 
 
  
# Plot corr plot of log return pairs
ggcorr(logreturns.1_df, palette = "RdYlGn", name = "rho", 
       label = "FALSE") +
       ggplot2::labs(title = "06-07-01 to 2007-12-31")


#~ Portfolio Changes (2006-07-01 to 2007-12-31) 

#  Select portfolio changes

# Select returns of portfolio components
returns.pf.1_xts <- change.pf_xts$contribution["2006-07-01/2007-12-31"]

# Select return of portfolio
return.pf.1_xts <- change.pf_xts$returns["2006-07-01/2007-12-31"]

# Select weights of portfolio components
weights.pf.1_xts <- change.pf_xts$BOP.Weight["2006-07-01/2007-12-31"]

# Select values of portfolio components
values.pf.1_xts <- change.pf_xts$BOP.Value["2006-07-01/2007-12-31"]

# Plot CumReturns Returns
chart.CumReturns(return.pf.1_xts, main= "Portfolio return",  ylab = "")

# Plot weight changes
chart.StackedBar(weights.pf.1_xts, main= "Portfolio weights", ylab = "")

# Plot value changes
chart.StackedBar(values.pf.1_xts, main= "Portfolio value",  ylab = "")


#~ Compute Risk Measurements

# Set quantile probability
p <- 0.95
alpha <- 1-p

# Compute Value at Risk 
VaR.hs <- VaR(return.pf.1_xts, p=alpha, portfolio_method="component", method = "historical", invert=FALSE)
VaR.normal <- VaR(return.pf.1_xts, p=alpha, portfolio_method="component", method = "gaussian")

# Compute Expected Shortfall
ES.hs <- ES(return.pf.1_xts, portfolio_method="single", method = "historical")
ES.normal <- ES(return.pf.1_xts, portfolio_method="single", method = "gaussian")

# convert to data.frame
return.pf.1_df <- return.pf.1_xts

# Plot Risk Measurements
ggplot(data=return.pf.1_df, aes(portfolio.returns))+
  geom_histogram(binwidth = 0.001, colour="black") +
  geom_vline(aes(xintercept=-VaR.hs$hVaR), color="black", linetype="dashed", size=0.5) +
  geom_text(aes(x=-VaR.hs$hVaR, label="VaR.hs", y=20), colour="grey", angle=90, vjust = 1.4, size = 4) +
  geom_vline(aes(xintercept=ES.hs), color="black", linetype="dashed", size=0.5) +
  geom_text(aes(x=ES.hs, label="ES.hs", y=20), colour="grey", angle=90, vjust = -0.7, size = 4) +
  geom_vline(aes(xintercept=-VaR.normal$VaR), color="black", linetype="dashed", size=0.5) +
  geom_text(aes(x=-VaR.normal$VaR, label="VaR.normal", y=20), colour="grey", angle=90, vjust = -0.5, size = 4) +
  geom_vline(aes(xintercept=ES.normal), color="black", linetype="dashed", size=0.5) +
  geom_text(aes(x=ES.normal, label="ES.normal", y=20), colour="grey", angle=90, vjust = -0.7, size = 4) +
  xlab("Daily Return Distribution") + ylab("Count") + ggtitle("Portfolio Risk")




# 5. Period 2 (2008-01-01 to 2009-06-30) --> Backtesting 

#~ Correlation

# Select data for Period 2
stocks.combi.2_df <- stocks.combi_df[stocks.combi_df$Dates >= "2008-01-01" & stocks.combi_df$Dates <= "2009-06-30",]

# Select log returns
logreturns.2_df <- stocks.combi.2_df[, c("logrKO", "logrXOM", "logrGE", "logrJNJ", "logrDIS", "logrWFC", "logrINTC", "logrMS", "logrCCC", "logrGS")]

# Plot scatter plots of log return pairs
ggpairs(logreturns.2_df, aes(alpha = 0.1)) +
  ggplot2::labs(title = "2008-01-01 to 2009-06-30")

# Plot corr plot of log return pairs
ggcorr(logreturns.2_df, palette = "RdYlGn", name = "rho", 
       label = FALSE) +
       ggplot2::labs(title = "2008-01-01 to 2009-06-30")


#~ Portfolio Changes (2008-01-01 to 2009-06-30)

# Select returns of portfolio components
returns.pf.2_xts <- change.pf_xts$contribution["2008-01-01/2009-06-30"]

# Select return of portfolio
return.pf.2_xts <- change.pf_xts$returns["2008-01-01/2009-06-30"]

# Select weights of portfolio components
weights.pf.2_xts <- change.pf_xts$BOP.Weight["2008-01-01/2009-06-30"]

# Select values of portfolio components
values.pf.2_xts <- change.pf_xts$BOP.Value["2008-01-01/2009-06-30"]

# Plot CumReturns Returns
chart.CumReturns(return.pf.2_xts, main= "Portfolio return",  ylab = "")

# Plot weight changes
chart.StackedBar(weights.pf.2_xts, main= "Portfolio weights", ylab = "")

# Plot value changes
chart.StackedBar(values.pf.2_xts, main= "Portfolio value",  ylab = "")


#~ Backtesting
# Compute violations for historical simulation
violations.hs <- sum(return.pf.2_xts < as.numeric(-VaR.hs$hVaR))
violations.hs.percent <- violations.hs/nrow(return.pf.2_xts)*100

# Compute violations for normal
violations.normal <- sum(return.pf.2_xts < -VaR.normal$VaR)
violations.normal.percent <- violations.normal/nrow(return.pf.2_xts)*100


#~ Copulas

# Construct pseudo copula data
# Display scatter plots of all data pairs 
# generated from Empirical Distribution Function edf
Y <- data.matrix(returns.components.pf_df[, c("JPM", "MS", "CCC", "GS")])
copulaY <- data.frame(apply(Y,2,edf,adjust=TRUE))
plot(copulaY)
ggpairs(copulaY, aes(alpha = 0.1))

# Fit t copula 
copulaYt <- fit.tcopula(copulaY)
copulaYt$ll.max

# Fit Gauss copula 
copulaXGauss <- fit.gausscopula(copulaY)
copulaXGauss$ll.max

# Fit clay copula 
copulaXClay <- fit.AC(copulaY,"clayton")
copulaXClay$ll.max