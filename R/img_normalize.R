#' Normalize an original phenotypic image into the gray mode and specific size.
#'
#' The collected experimental images are always in color mode with various size, which should be
#' normalzied into gray mode and unified resonable size before analysis.
#' @param dir a string that gives the directory storing the raw phenotypic image(s)
#'
#' @return a list contains two 'Image' objects or array stores original and normalized
#'         phenotypic image,respectively
#' @export
#'
#' @examples
#' img<-img_normalize("directory to/expr_swarming.jpg")
img_normalize<-function(dir='the directory storing the raw phenotypic image(s)'){
  img_ori0<-list()
  img_ori0<-EBImage::readImage(dir)
  ref_img<-EBImage::channel(img_ori0,"gray")
  dim_ref<-dim(ref_img)
  if (min(dim_ref)>1024){
    target<-EBImage::resize(ref_img,1024,1024)
    img_ori<-EBImage::resize(img_ori0,1024,1024)
  } else{
    target<-ref_img
    img_ori<-img_ori0
  }
  return(list(ori_img=img_ori,gray_target=target))
}

