#' Fill the ROI using the same integer value to specify the colony edge
#'
#' The ROI of proteolytic activity assay is near a ring, if we want to fill in the area within the ROI, a specific function
#' \code{pro_act_regionfill} is designed here to accomplish the task.
#' @param labels0 An array containing labelled objects,which are pixel sets with the same unique integer value to
#'                distinguish colony edge and background.
#' @param row_id  An integer specifies the position of the colony in horizontal direction
#' @param col_id  An integer specifies the position of the colony in vertical direction(for
#'                the condition of only a few colonies,fix it to 1).
#'
#' @return  An array containing labelled objects after the ROI within the edge is filled with the same integer value.
#'
#'
#' @examples
#'
regionfill<-function(labels0){
  labels1<-EBImage::fillHull(labels0)
  return(labels1)
}
