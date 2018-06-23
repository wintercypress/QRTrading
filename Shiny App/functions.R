library(XLConnect)
library(tidyquant)
library(quantmod)
library(tidyverse)
library(Hmisc)

# Get SP500 stocks tickers

sp500 <- tq_index("SP500") %>% as.data.frame() %>% arrange(symbol)

# Transform stock ticker into proper form

sp500$symbol <- gsub('\\.', '-', sp500$symbol) 

sp500$symbol <- gsub('CCL-U', 'CCL', sp500$symbol)

#Store tickers as tibble

sp500 <- sp500 %>% as.tibble()



# Function: Get stock prices

get_prc <- function(symb, start, end){
        price <- getSymbols(symb, src = "yahoo", 
                            from = start,
                            to = end,
                            auto.assign = FALSE)
        
        Date <- index(price)
        
        price <- (price %>% as.data.frame() 
                       %>% mutate(Date = Date) 
                       %>% select(Date, everything())
                  )
        
        price <- price %>% as.tibble()
        
        return(price)
}


# Function: Calculate returns

cal_ret <- function(prc){
        price <- xts(prc[,-1], order.by = prc$Date)
        
        ret <- dailyReturn(price) %>% as.data.frame()
        
        ret <- (ret %>% mutate(Date = rownames(ret)) 
                   %>% select(Date, everything())
                )
        
        ret <- ret %>% as.tibble()
        return(ret)
}

# Function: Correlation table for all SP500 stocks

all_corr <- function(start, end){
        
        #Get price and calculate returns for each stock
        
        sp500_tbl <- sp500 %>% mutate(price = map(symbol, function(.x) get_prc(.x, start, end)),
                                      returns = map(price, function(.x) cal_ret(.x))
        )
        
        # Return table for stocks
        
        sp500_return <- sp500_tbl %>% select(symbol, returns) %>% unnest() 
        
        sp500_spread <- sp500_return %>% spread(key = symbol, value = daily.returns) 
        
        # Pairwise correlations for all SP500 stocks
        
        sp500_cor <- (sp500_spread %>% select(-Date) 
                                   %>% cor(use = 'pairwise.complete.obs')
                                   %>% round(3)
                                   %>% as.data.frame()
                     )
        
        sp500_cor <- sp500_cor %>% rownames_to_column('symbol')
        
        return(sp500_cor)
}

# Function: Query_stock and peers correlation table

corr_func <- function(query_stock, sp500_cor){
     
        # Filter out 5 stocks with highest correlation with query_stock
        
        peers <- eval(substitute(
                sp500_cor %>% select(symbol, x) 
                %>% top_n(6, x) %>% arrange(desc(x)) 
                %>% select(symbol) %>% unlist(),
                list(x = as.name(query_stock))
        )
        )
        
        # Pairwise correlation table for query_stock and peers
        
        cor_matrix <- (sp500_cor %>% filter(symbol %in% peers) 
                                 %>% select(symbol, peers) 
                                 %>% arrange(match(symbol, peers))
                       )
        
        return(cor_matrix)
}


