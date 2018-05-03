#'Restructure Data
#'
#'Combine data from several rows into a single row based on common data in
#'selected columns.
#'
#'The function \code{group2row} combines data from several rows into a single
#'row. Certain columns in the input dataset are said to be "collected." Other
#'columns may be "carried" into the output dataset by listing them in
#'\code{carryColumns}. A new row will be created for each unique combination of
#'values in the \code{carryColumns}. The output row consists of the carried
#'columns plus new columns that are named by the unique values in the
#'\code{splitColumn} concatenated with the names in the \code{collectColumns}.
#'The number of columns in the output data frame is equal to the number of
#'\code{carryColumns} plus the number of unique values in the
#'\code{splitColumn} times the number of names in the \code{collectColumns}.\cr
#'
#'The strategy for collecting columns is to use a set of index values defined
#'by the \code{splitColumn}. The maximum number of input rows collected for
#'each output row is equal to the number of unique values defined in the
#'\code{splitColumn}. The \code{splitColumn} is used to identify a column from
#'the input data that contains output column information. If a row of input has
#'a value in this column that matches one of the index values, then that row's
#'data will be included in the output in the column positions corresponding to
#'the matched index. The index values are concatenated with the input column
#'names of the collected columns to derive output column names.\cr
#'
#' Added from smwrBase.
#'
#' @param data a data frame containing the columns to be combined.
#' @param carryColumns the names of the columns that form a row in the output
#'dataset. Each unique combination of values in these columns will be a new row
#'in the output dataset.
#' @param splitColumn the name of a single column. For each unique value in
#'\code{splitColumn} and for each column in \code{collectColumns}, a new column
#'is created in the output.
#' @param collectColumns the names of the columns to be collected. See
#'\bold{Details}.
#' @keywords internal manip
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(QWstacked)
#'group2row(QWstacked, c("site_no", "sample_dt", "sample_tm"), "parm_cd", 
#'  c("result_va", "remark_cd"))
#'}
#' @export
group2row <- function(data, carryColumns, splitColumn, collectColumns) {
  ## Coding history:
  ##    2002Apr30 DLLorenz Original coding.
  ##    2005Mar07 DLLorenz Modified to preserve original classes of carryColumns.
  ##    2005Mar10 DLLorenz Bug fix.
  ##    2005Mar11 DLLorenz Fix to use information on column formatting
  ##    2005Jul14 DLLorenz Date fix
  ##    2005Sep19 DLLorenz Bug fix.
  ##    2005Dec01 DLLorenz Modified to associate columns together (like value and remark)
  ##    2011Mar14 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##    2013Jul01 DLLorenz Bug fix for blanks and NAs
  ##
  ## The convert.chardata function:
  convert.chardata <- function(target, type, ...) {
    ## Fix NA and revert blank strings
    target <- miss2na(target, "NA")
    target <- sub("_blank_string_", "", target, fixed=TRUE)
    if(type %in% c("factor", "ordered"))
      retval <-(do.call(type, list(x=target, ...)))
    else {  ## make it an as. call
      type <- paste('as', type, sep='.')
      retval <- (do.call(type, list(target)))
    }
    if(all(is.na(retval)))
      retval <- target # recover from invalid or unconvertible format
    return(retval)
  }
  ## The get.col.type function:
  get.col.type <- function(Col) {
    Type <- data.class(Col)
    if(Type %in% c("factor", "ordered")) # get levels
      return(list(Type=Type, Levels=levels(Col)))
    if(Type == "numeric") # get storage mode
      Type <- storage.mode(Col)
    return(list(Type=Type))
  }
  ##
  ## Identify original classes of the carryColumns
  carryClass <- lapply(data[1L, carryColumns, drop=FALSE], get.col.type)
  ## First, make an index of all carryColumns
  group.index <- as.character(data[[carryColumns[1L]]])
  N <- length(carryColumns)
  if(N > 1L)
    for(i in seq(2L, N)) {
      ## This logic is required to protect against empty strings that do not 
      ## get put together correctly in the call to strsplit later!
      putin <- as.character(data[[carryColumns[i]]])
      putin <- ifelse(putin == "", "_blank_string_", putin)
      group.index <- paste(group.index, putin, sep="\001")
    }
  group.table <- unique(group.index)
  group.index <- match(group.index, group.table)
  ## group.index now points to the row number in the target matrix, which is
  ## indexed by group.table
  ##
  ## Next, make an index of the splitColumn
  Columns <- as.character(unique(data[[splitColumn]]))
  Columns <- Columns[!is.na(Columns)]
  column.index <- match(data[[splitColumn]], Columns)
  ##
  ## Now, create a data.frame of of the groups and convert back to
  ## original data type
  df <- as.data.frame(matrix(unlist(strsplit(group.table, split="\001",
                                             fixed=TRUE)),
                             ncol=length(carryColumns), byrow=TRUE),
                      stringsAsFactors=FALSE)
  names(df) <- carryColumns
  for(i in carryColumns) {
    if(carryClass[[i]]$Type %in% c("factor", "ordered"))
      df[[i]] <- convert.chardata(as.character(df[[i]]), carryClass[[i]]$Type,
                                  levels=carryClass[[i]]$Levels)
    else
      df[[i]] <- convert.chardata(as.character(df[[i]]), carryClass[[i]]$Type)
  }
  ##
  ## Combine the indices
  mat.index <- cbind(group.index, column.index)
  ##
  ## For each of collectColumns, create a matrix of NAs, populate it and
  ## cbind it to the data frame
  for(i in collectColumns) {
    temp.mat <- matrix(NA_integer_, nrow=length(group.table), ncol=length(Columns))
    temp.mat[mat.index] <- seq(along=data[[i]])
    temp.mat <- as.data.frame(temp.mat, stringsAsFactors=FALSE)
    # This should work for any kind of data, not just simple vectors
    # as long as data.frame operations work for the data type.
    for(k in seq(length(temp.mat))) {
      temp.mat[[k]] <- data[[i]][temp.mat[[k]]]
    }
    names(temp.mat) <- paste(Columns, i, sep=".")
    df <- cbind(df, temp.mat)
  }
  names(df) <- make.names(names(df))
  df
}
