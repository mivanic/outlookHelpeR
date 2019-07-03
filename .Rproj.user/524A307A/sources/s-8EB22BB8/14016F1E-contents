#' Excute a chain of data transformations.
#'
#' \code{chainedTransfromations} returns the dataset with all transformation executed in order
#'
#' @inheritParams manyLaggedValues
#' @param ... Data transforming functions function(data){... returns(data)}
#' @return A dataframe
#' @export

chainedTransfromations <- function(data, ...) {
  return(Reduce(function(ac, ad) {
    return(ad(ac))
  }, list(...), data))
}

#' Generate a series of lagged values.
#'
#' \code{manyLaggedValues} returns the dataset with lagged values (suffix Lx where x is the lag number)
#'
#' @param data A dataframe with data and a monthly period column
#' @param columnNames Column names that are to be lagged
#' @param timeColumn The name of the period column (YYYY-MM-DD)
#' @param period The period name (deafult = months)
#' @param lag The lag expressed in the number of months
#' @return A dataframe
#' @export

manyLaggedValues <-
  function(data,
           columnNames,
           timeColumn,
           lag,
           period = 'months')

    Reduce(function(a, b) {
      a[, paste(b, 'L', lag, sep = '')] = laggedValue(data, timeColumn, b, lag, period)
      return(a)
    },
    columnNames,
    data)

#' Generate a vector of a lagged value for a single column.
#'
#' \code{laggedValue} returns a dataframe with a lagged column)
#'
#' @inheritParams manyLaggedValues
#' @param var The name of the variable to be lagged
#' @return A vector
#' @export

laggedValue <-
  function(data, timeColumn, var, lag, period = 'months') {
    if (class(data[, timeColumn]) == 'character')
      data$tempTimeColumn = format(as.Date(data[, timeColumn]), '%Y-%m-%d')
    else if (class(data[, timeColumn]) == 'Date')
      data$tempTimeColumn = format(data[, timeColumn], '%Y-%m-%d')
    else
      data$tempTimeColumn = format(as.Date(data[, timeColumn], origin = '1970-01-01'), '%Y-%m-%d')


    data1 = data[, c('tempTimeColumn', var)]


    data1$lagged = sapply(data$tempTimeColumn, function(x) {
      return (format(seq(
        as.Date(x),
        length = 2,
        by = paste(lag, period)
      )[2], '%Y-%m-%d'))
    })

    data2 = data1
    data2$tempTimeColumn = data2$lagged
    data[, var] = NULL

    data$orderColumn=1:nrow(data)

    merged=merge(data, data2, by = 'tempTimeColumn', all.x = T, sort=F)

    return(merged[order(merged$orderColumn), var])
  }


