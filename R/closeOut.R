###########################################################
#' Document Processing Time and Other Session Time
#'
#' Document Processing Time and Other Session Time
#'
#' @param timeProcess Processing time
#' @param contInfo Contact information
#' @param sessInfo Session information
#' @examples
#' closeOut()
#' @return Reports out processing time, contact information and session information
#' @export
#
closeOut <-function(timeProcess=TRUE, contInfo=TRUE, sessInfo=TRUE) {

# -----< Change history >--------------------------------------------
# 14Mar2017: JBH: removed .iTest
# 05May2016: JBH: added Harcum to contact list
# 27Apr2016: JBH: Explicit use of "::" for non-base functions added.
#

  if(timeProcess | contInfo | sessInfo ) {

    .H2(" ")
    .H1("Close Out")

    if(timeProcess) {
      .H3("Total Processing Time")
      finish <- Sys.time()
      if(exists("begin")) {
        elapse <- finish - begin
        .V(paste0("Begin processing time ", begin, "."))
        .P()
      }
      .V(paste0("End processing time ", finish, "."))
      .P()
      if(exists("begin")) {
        .V(paste0("The total processing time was ", elapse, "."))
      }
      .P()
    }

    if(contInfo) {
      .H3("Distribution and Contact Information")

      contactInformation <- data.frame(
        Category=c("Name", "Organization", "Address", "Voice phone", "email"),
        Description=c("Rebecca Murphy, Ph.D.",
                      "Chesapeake Bay Program Office",
                      "410 Severn Ave., Suite 112, Annapolis, MD 21403",
                      "(410)267-9837",
                      "rmurphy@chesapeakebay.net"))
      print(knitr::kable(contactInformation))
      .P()

      contactInformation <- data.frame(
        Category=c("Name",  "Address", "Voice phone", "email"),
        Description=c("Elgin S. Perry, Ph.D.",
                      "377 Resolutions Rd., Colonial Beach, VA 22443",
                      "(410)610-1473",
                      "eperry@chesapeake.net"))
      print(knitr::kable(contactInformation))
      .P()

      contactInformation <- data.frame(
        Category=c("Name", "Organization", "Address", "Voice phone", "email"),
        Description=c("Jon B. Harcum, Ph.D.",
                      "Tetra Tech, Inc.",
                      "313 Kings Way, Clemson, SC 29631",
                      "(864)650-5815",
                      "jon.harcum@tetratech.com"))
      print(knitr::kable(contactInformation))
      .P()

    }

    if(sessInfo) {
      .H2(" ")
      .H3("Session Information")
      tmp <- (devtools::session_info())
      .H4("Platform")
      print((tmp[[1]]))
      .H4("Packages")
      print(knitr::kable(tmp[[2]][c(1,3,4)]))

    }

  }
}
