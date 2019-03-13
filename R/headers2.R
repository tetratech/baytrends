# -----< Change history >--------------------------------------------
# 11Oct2016: JBH: Change to leading '.' function calls
# 27Apr2016: JBH: Explicit use of "::"  for non-base functions added.
# 12Mar2019: EWL: Document undocumented parameters, F to .F in example

# -----< Figure Title   >--------------------------------------------
#' Print out figure title  (customization of pandoc.emphasis and pandoc.strong )
#'
#' @param text text of figure title
#' @param n figure number
#' @param t emphasis or stong
#' @examples
#' text<-"Hello World!"
#' .F(text)
#' .F(text, 4)
#' .F(text, 4,'e')
#' .F(text, 4,'s')
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.F <- function(text, n=NULL, t='e') {
#  pkgTest("pander")
  if(is.null(n)) {
    title <- paste0("Figure: ",text)
  } else {
    title <- paste0("Figure ", n, ". ", text)
  }

  .P()
  if(t=="e") {
    pander::pandoc.emphasis(title)
  } else {
    pander::pandoc.strong(title)
  }
  .P()

}

# -----< Print Header   >--------------------------------------------
#' Print out header (shortened pandoc.header)
#'
#' @param text text of header
#' @param n header level number
#' @examples
#' .H("1st level header",1)
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.H <- function(text, n=1) {
#  pkgTest("pander")
  pander::pandoc.header(text, n)
}

# -----< Print Header Level 1 >--------------------------------------
#' Print out 1st level header (shortened pandoc.header)
#'
#' @param text text of header
#' @examples
#' .H1("1st level header")
#' .H3("3rd level header")
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.H1 <- function(text) {
#  pkgTest("pander")
  pander::pandoc.header(text, 1)
}

# -----< Print Header Level 2 >--------------------------------------
#' Print out 2nd level header (shortened pandoc.header)
#'
#' @param text text of header
#' @examples
#' .H2("2nd level header")
#' .H3("3rd level header")
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.H2 <- function(text) {
#  pkgTest("pander")
  pander::pandoc.header(text, 2)
}

# -----< Print Header Level 3 >--------------------------------------
#' Print out 3rd level header (shortened pandoc.header)
#'
#' @param text text of header
#' @examples
#' .H2("2nd level header")
#' .H3("3rd level header")
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.H3 <- function(text) {
#  pkgTest("pander")
  pander::pandoc.header(text, 3)
}

# -----< Print Header Level 4 >--------------------------------------
#' Print out 4th level header (shortened pandoc.header)
#'
#' @param text text of header
#' @examples
#' .H2("2nd level header")
#' .H4("4th level header")
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.H4 <- function(text) {
#  pkgTest("pander")
  pander::pandoc.header(text, 4)
}

# -----< Print Header Level 5 >--------------------------------------
#' Print out 5th level header (shortened pandoc.header)
#'
#' @param text text of header
#' @examples
#' .H2("2nd level header")
#' .H5("5th level header")
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.H5 <- function(text) {
#  pkgTest("pander")
  pander::pandoc.header(text, 5)
}

# -----< Print Paragraph      >--------------------------------------
#' Paragraph  (customization of pandoc.p)
#'
#' @param text text of paragraph
#' @examples
#' .P()
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.P <- function(text=NULL) {
#  pkgTest("pander")
  if(is.null(text)) {
    pander::pandoc.p(' ')
  } else {
    pander::pandoc.p(text)
  }
}

# -----< Print Table Title >-----------------------------------------
#' Print out table title (customization of pandoc.emphasis and pandoc.strong )
#'
#' @param text text of table title
#' @param n table number
#' @param t emphasis or stong
#' @examples
#' text<-"Hello World!"
#' .T(text)
#' .T(text, 4)
#' .T(text, 4,'e')
#' .T(text, 4,'s')
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.T <- function(text, n=NULL, t='e') {
#  pkgTest("pander")
  if(is.null(n)) {
    title <- paste0("Table: ",text)
  } else {
    title <- paste0("Table ", n, ". ", text)
  }

  .P()
  if(t=='e') {
    pander::pandoc.emphasis(title)
  } else {
    pander::pandoc.strong(title)
  }
  .P()

}

# -----< Print Text     >--------------------------------------------
#' Print out text  (blended pandoc.emphasis, .verbatim, and .strong)
#'
#' @param text text
#' @param t emphasis or stong or verbatim
#' @examples
#' .V("Hello World!",'v')
#' .V("Hello World!",'e')
#' .V("Hello World!",'s')
#' .V("Hello World!")
#' @return n/a
#' @keywords internal
#' @seealso .F .H .H2 .H3 .H4 .P .T .V
#' @export
#'
.V <- function(text=" ", t="v") {
#  pkgTest("pander")
  if(t=="v") {
    pander::pandoc.verbatim(text)
  } else if(t=="e") {
    pander::pandoc.emphasis(text)
  } else {
    pander::pandoc.strong(text)
  }
}

# -----< Print Vector in a Wrapped Table Format >--------------------
#' Print out character vector table in wrapped mode
#'
#' @param x character vector
#' @param width wrap width [default=65]
#' @keywords internal
#' @export
#'
.vTable <- function(x=c(' '), width=65) {

  # turn off 'continue statement from pander
  pander::panderOptions('table.continues', '')

  # determine number of columns and rows to convert vector into
  iCol <- trunc(width/ max(nchar(x)))
  iCol <- ifelse(iCol>6, 6, iCol)
  iRow <- ceiling(length(x)/iCol)

  # fill out the vector with enough null values to make a clean looking matrix
  iLen <- iCol*iRow
  iFill<- iLen - length(x)
  y <- c(x, rep("", times = iFill) )

  # make the matrix
  y <- matrix(y, ncol=iCol, byrow=TRUE)

  # create a vector j with enough 'left' to left justify all columns
  j <- c(rep("left", times=iCol))

  # print out the table
  pander::pandoc.table(y, ncol=iCol, justify = j)

  # turn 'continue' statement back on
  pander::panderOptions('table.continues', 'Table continues below')

}
