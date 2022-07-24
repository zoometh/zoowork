#' Read a XLSX dataset
#' @name zoo_order_period
#' @description Test if the periods existing in the dataset are also in the order
#' reference period list. If not return an error
#'
#' @param df a dataframe
#' @param period_column the column name of assemblage periods 
#' @param lorder_period a vector with the ordered periods
#'
#' @return the ordered list of periods existing in the dataframe
#'
#' @examples
#' 
#' df <- zoo_read()
#' lorder_period <- zoo_order_period(df)
#'
#' @export
zoo_order_period <- function(df = NA,
                             period_column = "period",
                             lorder_period = c("EN", "MN", "LN", "EBA", "MBA", "LBA", "EIA", "MIA1", "MIA2", "RR", "ERE", "LRE", "LA")){
  def_per <- unique(df[ , period_column])
  notIn_def_per <- setdiff(lorder_period, def_per)
  notIn_lorder_period <- setdiff(def_per, lorder_period)
  if(length(notIn_def_per) > 0){
    new_lorder_period <- intersect(lorder_period, def_per)
    warning(paste0("Your dataset doesn't have all the periods of reference.
                 The periods '", toString(notIn_def_per),"' are in your periods of reference but not in your dataset.
                 The new periods of reference will be:\n", toString(new_lorder_period)))
  }
  if(length(notIn_lorder_period) > 0){
    stop(paste0("*These periods exist in your dataset but not in the periods of reference: ",
                toString(notIn_lorder_period), ". You need to add them in the periods of reference variable")
    )
  }
  return(intersect(lorder_period, def_per))
}
