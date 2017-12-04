#' Re-attribute df based on previous df
#'
#' Re-attribute df based on previous df. This is useful if you run a drop column command
#' run an aggregate function. This types of functions drop the attributes. This function
#' examines to original and new dfs and adds the attributes from the original df
#' to the new df whenever the new df doesn't have a particular attribute. (This navigates
#' around the issue of changed structure.)
#'
#' @param df1 new data frame
#' @param df0  old data frame
#' @examples
#' # create data frame
#' df0 <- data.frame (sta=c("A","A"), lay=c("B","C"), x1 =c(NA,2), x2 =c( 4,14))
#'
#' #add simple attribute
#' attr(df0, "Attribute1") <- "Test attribute1"
#'
#' #run aggregate -- loose attributes
#' df1 <- aggregate(x2 ~ sta, data=df0, mean, na.action=na.pass, na.rm=TRUE)
#' df2 <- .reAttDF(df1, df0)
#' @return n/a
#' @export
.reAttDF <- function(df1, df0) {
  for (i in 1: length(names(attributes(df0)))) {
    if(!(names(attributes(df0))[i] %in% names(attributes(df1)))) {
      attr(df1, names(attributes(df0))[i]) <- attr(df0,names(attributes(df0))[i])
    }
  }
  return(df1)
}
