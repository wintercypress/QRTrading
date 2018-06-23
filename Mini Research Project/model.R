# Load packages
library(quantmod)
library(tidyverse)
library(XLConnect)
library(tidyquant)
library(tseries)
library(reshape2)
library(readxl)


# Function: Calculate return
get_return <- function(symbol, start, end, compression = 'm'){
        price <- get.hist.quote(instrument = symbol, start = start, end = end, compression = compression, quote = "Close")
        Date <- index(price)

        price <- (price %>% as.data.frame()
                  %>% mutate(Date = Date)
                  %>% select(Date, everything())
        )

        returns <- price %>% mutate(returns = Close/lag(Close) - 1) %>% select(-Close) %>% filter(complete.cases(returns))

        returns <- returns %>% as.tibble()
        return(returns)
}

# Data range and query stock

start <- '2012-06-01'

end <- '2018-06-22'

query_stock <- 'APA'

## Carhart four-factor model

# Query stock returns

returns <- get_return(query_stock, start, end)

returns$Date <- as.yearmon(returns$Date)


# Fama French 5 factors

ff_factors <- read.csv("F-F_Research_Data_Factors.CSV", header = TRUE, skip = 3, nrows = 1102)

colnames(ff_factors)[1] <- 'Date'

ff_factors$Date <- as.yearmon(as.Date(paste(ff_factors$Date, '01', sep = ''), 
                                      format='%Y%m%d')
                              )
ff_factors[,-1] <- ff_factors[,-1]/100

# Fama French Momentum

ff_mom <- read.csv("F-F_Momentum_Factor.CSV", header = TRUE, skip = 13, nrows = 1095)

colnames(ff_mom)[1] <- 'Date'

ff_mom$Date <- as.yearmon(as.Date(paste(ff_mom$Date, '01', sep = ''), 
                                      format='%Y%m%d')
)

ff_mom[,-1] <- ff_mom[,-1]/100


# Merge stock returns, ff_mom, ff_factors by Date

df <- Reduce(function(x,y) merge(x, y, by = 'Date'), list(ff_factors, ff_mom, returns))

# Calculate excess stock return

df <- df %>% mutate(Ex.return = returns - RF) 

# Run regression

df <- df %>% select(-c(RF, returns))

summary(lm(Ex.return ~ ., data = df[,-1]))


## PCA Analysis

# Filter out stocks in the same sub-sector as query_stock
# Sub Industry based on GICS

sub_industry <- read_xlsx("SP500 Sectors.xlsx", sheet = 1) 

query_industry <- sub_industry %>% filter(`Ticker symbol` == query_stock) %>% select(`GICS Sub Industry`) %>% unlist()

peers <- sub_industry %>% filter(`GICS Sub Industry` == query_industry) %>% select(`Ticker symbol`)

colnames(peers) <- 'symbol'

# Calculate returns for stocks in the selected sub-sector

peers_return <- peers %>% mutate(returns = map(symbol, function(.x) get_return(.x, start, end))) 

peers_return <- peers_return %>% unnest()

peers_return <- dcast(peers_return, Date ~ symbol) 

# Exclude stocks with shorter history than the data range selected 
peers_return <- peers_return[, colSums(is.na(peers_return)) == 0]

# PCA analysis
pca <- prcomp(peers_return[,-1])

# Select retained pcs based on Scaled Average Eigenvalues
# Keep eigenvalues larger than the scale

keep_scale <- mean(pca$sdev^2)*0.7
n <- sum(pca$sdev^2 > keep_scale)

# Regression on PCs

pcs <- pca$x[,c(1:n)]

pc_tbl <- peers_return %>% select(Date) %>% cbind(pcs)

pc_tbl$Date <- as.yearmon(pc_tbl$Date)

tbl <- merge(returns, pc_tbl)

summary(lm(returns ~., data = tbl[,-1]))

# PCA Summary
summary(pca)$importance[,1:3]

# PCs Loading
pca$rotation[,1:n]

# All stocks have positive weights in PC1, check its correlation with Oil Price

WTI <- read.csv('WTI.csv', header = TRUE)
WTI$Date <- as.yearmon(as.Date(WTI$Date, format='%m/%d/%Y'))

WTI <- WTI %>% mutate(WTI.return = WTI/lag(WTI)-1) %>% select(-WTI) 

df_new <- merge(tbl, WTI)
cor(df_new[,-1])

