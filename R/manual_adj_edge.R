#' Manual adjustment for edge detection of some microorganism colonies
#'
#' As the unified parameters \code{Canny_sigma},\code{low_thr},\code{high_thr} and \code{mor_size}may not fit for some
#' colonies, \code{manual_adj_edge} is designed for adjusting the edge detection manually for the colonies in need.
#' @param expr_mode A string indicates the phenotypic experiment type, as different experiments require differnt
#'                  processing steps
#' @param row_id    An integer specifies the position of the colony need adjustment in horizontal direction.
#' @param col_id    An integer specifies the position of the colony need adjustment in vertical direction(for
#'                  the condition of only a few colonies,fix it to 1).
#' @param intensity A matrix specifies the intensity value of a colony patch,whose value is normalized into [0,1]
#' @param labels    An array containing labelled objects,which are pixel sets with the same unique integer value to
#'                  distinguish colony edge and background.
#' @param Canny_sigma A numeric specifies the Gaussian filter variance in Canny detector.
#' @param low_thr  lower threshold value of the Canny detector.
#' @param high_thr upper threshold value of the Canny detector.
#' @param mor_size A numeric containing the size of the brush in pixels. This should be an odd number; even numbers
#'                 are rounded to the next odd one, i.e., size = 4 has the same effect as size = 5.
#' @param high_connectivity A logic 4(false)- or 8(true)-connectivity in 2d case, and between 6(false)- or 26(true)-connectivity in 3d case.
#'                          Default is TRUE.
#'
#' @return  A list contains 1) An array containing labelled objects. Labelled objects are pixel sets with the same unique integer value.
#'          2) An array containing labelled objects after the ROI within the edge is filled with the same integer value.
#'          3) An array, containing the painted version of a certain colony.
#'
#' @examples rslt<-manual_adj_edge(expr_mode="Congo_red",i,j,Canny_sigma=2,
#'                                 low_thr=12,high_thr=25,mor_size=13)
#'
manual_adj_edge<-function(expr_mode="Congo_red",row_id,col_id,intensity,labels,Canny_sigma=7,low_thr=6,
                  high_thr=20,mor_size=9,high_connectivity=TRUE){
  edges_pic4<-matrix(list(),row_id,col_id)
  im_afmor<-matrix(list(),row_id,col_id)
  labels0<-matrix(list(),row_id,col_id)
  labels2<-matrix(list(),row_id,col_id)
  edges_pic4[[row_id,col_id]]<-image.CannyEdges::image_canny_edge_detector(intensity[[row_id,col_id]] * 255,
                                                                           s = Canny_sigma, low_thr = low_thr,
                                                                            high_thr =high_thr)$edges
  kern = EBImage::makeBrush(mor_size, shape='disc')
  im_afmor[[row_id,col_id]]<-EBImage::closing(edges_pic4[[row_id,col_id]],kern)
  # label function in imager package
  cimg_patch<-imager::as.cimg(im_afmor[[row_id,col_id]])
  labels0[[row_id,col_id]] <- imager::label(imager::as.pixset(cimg_patch), high_connectivity = TRUE) #8-neighbour
  temp<-EBImage::rotate(imager::cimg2im(labels0[[row_id,col_id]])$v,-90) #rotate 90 degreee
  labels[[row_id,col_id]]<-temp
  if (expr_mode!="pro_act"){
    labels2[[row_id,col_id]]<-regionfill(labels[[row_id,col_id]])
  }
  else
    labels2[[row_id,col_id]]<-pro_act_regionfill(labels[[row_id,col_id]])
  if(expr_mode!="Congo_red"){
   patch_edge[[row_id,col_id]]<-EBImage::paintObjects(x=labels[[row_id,col_id]],EBImage::toRGB(img_patch$patch[[row_id,col_id]]),
                                            col=c("red","black"),opac=c(1, 0.3))
  }
  else{
    patch_edge[[row_id,col_id]]<-EBImage::paintObjects(x=labels[[row_id,col_id]],img_patch$color_patch[[row_id,col_id]],
                                              col=c("blue","black"), opac=c(1, 0.3))
  }
  labels1[[row_id,col_id]]<-labels2[[row_id,col_id]]
  # fill_label<-names(sort(table(labels[[row_id,col_id]]),decreasing = TRUE)[2])%>%as.numeric
  # # fill_label<-3
  # bound_pixset<-apply(labels[[row_id,col_id]],1,function(s) grep(fill_label,s))
  # notnull<-c()
  # for (k in 1:length(bound_pixset)){
  #   notnull[k]<-(length(bound_pixset[[k]])!=0)
  # }
  # for (k in which(notnull!=0)){
  #   labels1[[row_id,col_id]][k,min(bound_pixset[[k]]):max(bound_pixset[[k]])]<-fill_label
  # }
  # bound_pixset_x<-apply(labels[[row_id,col_id]],1,function(s) grep(fill_label,s))
  # bound_pixset_y<-apply(labels[[row_id,col_id]],2,function(s) grep(fill_label,s))
  # notnull_x<-c()
  # notnull_y<-c()
  # for (k in 1:length(bound_pixset_x)){
  #   notnull_x[k]<-(length(bound_pixset_x[[k]])!=0)
  # }
  # for (k in 1:length(bound_pixset_y)){
  #   notnull_y[k]<-(length(bound_pixset_y[[k]])!=0)
  # }
  # for (m in which(notnull_x!=0)){
  #   labels1[[row_id,col_id]][m,min(bound_pixset_x[[m]]):max(bound_pixset_x[[m]])]<-fill_label
  # }
  return(list(label0=labels[[row_id,col_id]],filled_label=labels1[[row_id,col_id]],
              edge=patch_edge[[row_id,col_id]]
              ))
}

