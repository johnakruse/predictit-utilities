profits <- function(x){
    profit <- sub('\\$','',as.character(x[['ProfitLoss']]))
    profit <- sub('\\(','-',as.character(profit))
    profit <- sub('\\)','',as.character(profit))
    profit <- as.numeric(profit)
    profit
}

cumulative <- function(x) {
    profit <- profits(x[order(x$DateExecuted),])
    total <- cumsum(profit)
    total
}

percent_prof <- function(x){
    initial <- cumulative(x)
    initial <- c(0, initial[1:length(initial)-1])
    cumul <- cumulative(x)
    percent_change <- cumul/initial
    percent_change
}
