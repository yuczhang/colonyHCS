#' Edge detection to segment the forground of the microoganism colonies.
#'
#' In order to characterize the phenotypic change of mutant, the Canny edge detection algotithm is applied to realzie the segmentation.
#' @param expr_mode   a string indicates the phenotypic experiment type, as different experiments require differnt
#'                    processing steps
#' @param colony_patch a matrix specifies the intensity value of a colony patch,whose value is normalized into [0,1]
#' @param color_patch  An 'Image' object or an array of specifies a colony patch in Color mode,or the same as the colony_patch for some
#'                     phenotypic experiment that does not focus on color change
#' @param Canny_sigma  A numeric specifies the Gaussian filter variance in Canny detector.Defaults to 7 in the
#'                     examples.
#' @param low_thr      lower threshold value of the Canny detector.
#' @param high_thr     upper threshold value of the Canny detector.
#' @param mor_size     A numeric containing the size of the brush in pixels. This should be an odd number; even numbers
#'                     are rounded to the next odd one, i.e., size = 4 has the same effect as size = 5. Default is 9.
#' @param mor_shape    A character vector indicating the shape of the brush. Can be box, disc, diamond, Gaussian or line. Default is disc.
#' @param high_connectivity A logic 4(false)- or 8(true)-connectivity in 2d case, and between 6(false)- or 26(true)-connectivity in 3d case.
#'                          Default is TRUE.
#'
#' @return 	A list contains 1) An array containing labelled objects. Labelled objects are pixel sets with the same unique integer value.
#'          2) An array, containing the painted version of a certain colony.
#'
#' @export
#' @seealso
#' @examples
#' edge_rslt<-colony_edge(expr_mode="swarming",colony_patch=img_patch$patch[[i,j]],
#'                        color_patch=img_patch$color_patch[[i,j]],Canny_sigma=8,
#'                        low_thr=6,high_thr=20,mor_size=9)
colony_edge<-function(expr_mode="Congo_red",colony_patch,color_patch=NULL,Canny_sigma=7,low_thr=6,
                      high_thr=20,mor_size=9,mor_shape='disc',high_connectivity=TRUE){
  #library(pixmap)
  library(image.CannyEdges)
  #library(wvtool)
  library(imager)
  edges_pic<-image.CannyEdges::image_canny_edge_detector(colony_patch * 255, s = Canny_sigma, low_thr = low_thr, high_thr = high_thr)$edges
  kern = EBImage::makeBrush(size=mor_size, shape=mor_shape)
  im_afmor<-EBImage::closing(edges_pic,kern)
  # label function in imager package
  cimg_patch<-imager::as.cimg(im_afmor)
  labels0 <- imager::label(imager::as.pixset(cimg_patch), high_connectivity = high_connectivity) #8邻域
  labels <-EBImage::rotate(imager::cimg2im(labels0)$v,-90) #旋转90度
  if(expr_mode!="Congo_red"){
    patch_edge<-EBImage::paintObjects(x=labels,EBImage::toRGB(colony_patch),col=c("red","black"), opac=c(1, 0.3))
  }
  else{
    patch_edge<-EBImage::paintObjects(x=labels,color_patch,col=c("blue","black"), opac=c(1, 0.3))
  }
  return(list(labels=labels,patch_edge=patch_edge))
}
