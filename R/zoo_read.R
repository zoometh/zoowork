#' Read a XLSX dataset
#' @name zoo_read
#' @description Read a XLSX dataset
#'
#' @param xlsx the complete path of the XLSX dataset, by default the test one
#' @param sheet the index or the name of the XLSX sheet to read, by default the
#' first one
#' @param num_column the column name of assemblage numbers
#' @param key_column the column name of assemblage keys
#' @param period_column the column name of assemblage periods
#'
#' @return A dataframe
#'
#' @examples
#' 
#' df <- zoo_read()
#'
#' @export
zoo_read <- function(xlsx = paste0(getwd(), "/inst/extdata/Type_Site_PlosOne_CAT_14_TOM_rvTH_6_ZOOWORK.xlsx"),
                     sheet = 1,
                     num_column = "num",
                     key_column = "key",
                     period_column = "period"){
  print(paste0("* read NISP from '", DescTools::SplitPath(xlsx)$fullfilename,"' file *"))
  CATnisp <- openxlsx::read.xlsx(xlsx,
                                 sheet = sheet,
                                 rowNames = F,
                                 skipEmptyRows = TRUE)
  CATnisp$num.site <- as.numeric(gsub(CATnisp$num, pattern = "[[:alpha:]]", replacement = ""))
  # lbl to match BIOM
  CATnisp$lbl <- paste0(CATnisp$key,'.', CATnisp$period)
  return(CATnisp)
}


