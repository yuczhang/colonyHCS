#' Fill the ROI of proteolytic activity assay using the same integer value to specify the colony edge
#'
#' The ROI of proteolytic activity assay is near a ring, if we want to fill in the area within the ROI, a specific function
#' \code{pro_act_regionfill} is designed here to accomplish the task.
#' @param labels0 An array containing labelled objects,which are pixel sets with the same unique integer value to
#'                distinguish colony edge and background
#' @param row_id  An integer specifies the position of the colony need adjustment in horizontal direction
#' @param col_id  An integer specifies the position of the colony need adjustment in vertical direction(for
#'                the condition of only a few colonies,fix it to 1).
#'
#' @return  An array containing labelled objects after the ROI within the edge is filled with the same integer value.
#'
#'
#' @examples
#'
pro_act_regionfill<-function(labels0){
  notnull1<-c()
  notnull2<-c()
  notnull<-c()
  ori_label<-labels0
  fill_label1<-names(sort(table(ori_label[which(ori_label!=0)]),decreasing = TRUE)[1])%>%as.numeric
  fill_label2<-names(sort(table(ori_label[which(ori_label!=0)]),decreasing = TRUE)[2])%>%as.numeric
  # bound_pixset1<-apply(labels0,1,function(s) grep(fill_label1,s))
  # bound_pixset2<-apply(labels0,1,function(s) grep(fill_label2,s))
  bound_pixset1<-apply(ori_label,1,function(s) grep(fill_label1,s))
  bound_pixset2<-apply(ori_label,1,function(s) grep(fill_label2,s))
  for (k in 1:length(bound_pixset1)){
    notnull1[k]<-(length(bound_pixset1[[k]])!=0)
  }
  for (k in 1:length(bound_pixset2)){
    notnull2[k]<-(length(bound_pixset2[[k]])!=0)
  }
  rm_notnull1<-which(notnull1&notnull2)
  # notnull1[rm_notnull1]<-FALSE
  # notnull<-notnull1
  for (k in which(notnull1!=0)){
    if(k %in% rm_notnull1){
      ori_label[k,min(bound_pixset1[[k]]):min(bound_pixset2[[k]])]<-fill_label1
      ori_label[k,max(bound_pixset2[[k]]):max(bound_pixset1[[k]])]<-fill_label1
    }
    else{
      ori_label[k,min(bound_pixset1[[k]]):max(bound_pixset1[[k]])]<-fill_label1
    }
  }
  labels1<-ori_label
  return(labels1)
}
