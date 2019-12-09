#' Cropping the phenotypic image of the colony array on agar plates into single patches.
#'
#' Before the processing and extraction of phenotypic signitures,if the experiment was done on agar plates,
#' it is recommeded to crop the raw images into patches containing single colony using \code{colony_auto_crop}
#'
#' @param expr_mode a string indicates the phenotypic experiment type, as different experiments require differnt
#'                  processing steps
#' @param img_gray  an 'Image' object or an array stores the raw image in gray mode and specific size
#' @param img_ori   an 'Image' object or an array stores the raw phenotypic image in color or gray mode
#' @param smth_win  the length of the Gaussian smoothing window of the raw or column sum of the image intensity,if an integer,represents
#'                  number of items, else, if a value between 0 and 1, represents the proportion of the input vector
#' @param row_num   an integer,represents the number of row within the raw image
#' @param col_num   an integer,represents the number of row within the raw image
#'
#' @return a list contains the segmented colony patches,both in color and gray mode, and R,G,B channels(for the
#'         phenotypes rely on color change),as well as the row and column intensity sum and cutting coordinates
#' @export
#'
#' @examples
#' img_patch<-single_colony_seg(expr_mode="swarming",img_gray=img$gray_target,
#'                              img_ori=img$ori_img,smth_win=0.09,row_num=7,col_num=7)
#'
#' @seealso \code{\link{colony_manual_crop}}
colony_auto_crop<-function(expr_mode="swarming",img_gray,img_ori,smth_win=0.09,row_num,col_num){
  patch<-matrix(list(),row_num,col_num)
  color_patch<-matrix(list(),row_num,col_num)
  # library(smoother)
  tt<-as.data.frame(img_gray)
  tt<-as.matrix(tt)
  x1<-1:dim(tt)[2]
  x2<-1:dim(tt)[1]
  cols<-colSums(tt,na.rm =TRUE)
  rows<-rowSums(tt,na.rm =TRUE)
  ys= smoother::smth(cols,window = smth_win,method = "gaussian") #SMOOTHING
  xs= smoother::smth(rows,window = smth_win,method = "gaussian") #SMOOTHING
  # Find the local maximum of smoothed sum of columns
  ys_diff<-diff(ys,lag = 1, differences = 1)
  # Find the local maximum of smoothed sum of rows
  xs_diff<-diff(xs,lag = 1, differences = 1)
  ny<-length(ys_diff)
  nx<-length(xs_diff)
  d1<-ys_diff[1:ny-1]
  d2<-ys_diff[2:ny]
  d3<-xs_diff[1:nx-1]
  d4<-xs_diff[2:nx]
  if(expr_mode!="pro_act"){
    y_indmax<-which(d1*d2<0 & d1<0)+1
    x_indmax<-which(d3*d4<0 & d3<0)+1
    x_cut<-c(0,x_indmax,dim(tt)[1])
    y_cut<-c(0,y_indmax,dim(tt)[2])
    # x_cut<-c(0,145,290,439,572,738,847,1024)
    # y_cut<-c(0,149,300,439,573,750,883,1024)
  } else {
    y_indmax<-which(d1*d2<0 & d1>0)+1
    x_indmax<-which(d3*d4<0 & d3>0)+1
    x_cut<-c(0,x_indmax[c(2,4,6,8,10,12)],dim(tt)[1])
    y_cut<-c(0,y_indmax[c(2,4,6,8,10,12)],dim(tt)[2])
  }
  color_patch<-matrix(list(),row_num,col_num)
  if(expr_mode!="Congo_red"){
    for (i in 1:row_num){
      for (j in 1:col_num){
        patch[[i,j]]<-tt[x_cut[j]:x_cut[j+1],y_cut[i]:y_cut[i+1]]
        color_patch[[i,j]]<-patch[[i,j]]
      }
    }
    return(list(x1=x1,cols=cols,ys=ys,x2=x2,rows=rows,xs=xs,x_cut=x_cut,y_cut=y_cut,
                patch=patch,color_patch=color_patch))
  }
  else{
    tt_red<-EBImage::channel(img_ori,"red")
    tt_green<-EBImage::channel(img_ori,"green")
    tt_blue<-EBImage::channel(img_ori,"blue")
    img_patch_red<-matrix(list(),row_num,col_num)
    img_patch_green<-matrix(list(),row_num,col_num)
    img_patch_blue<-matrix(list(),row_num,col_num)
    # # index of each patch should use img_patch[[i,j]]
    # x_cut<-c(0,x_cut0[1:6],1024)
    # y_cut<-c(0,y_cut0,1024)
    for (i in 1:row_num){
      for (j in 1:col_num){
        patch[[i,j]]<-tt[x_cut[j]:x_cut[j+1],y_cut[i]:y_cut[i+1]]
        img_patch_red[[i,j]]<-tt_red[x_cut[j]:x_cut[j+1],y_cut[i]:y_cut[i+1]]
        img_patch_green[[i,j]]<-tt_green[x_cut[j]:x_cut[j+1],y_cut[i]:y_cut[i+1]]
        img_patch_blue[[i,j]]<-tt_blue[x_cut[j]:x_cut[j+1],y_cut[i]:y_cut[i+1]]
        color_patch[[i,j]]<-EBImage::rgbImage(img_patch_red[[i,j]],img_patch_green[[i,j]],
                                       img_patch_blue[[i,j]])
        }
    }
    return(list(x1=x1,cols=cols,ys=ys,x2=x2,rows=rows,xs=xs,x_cut=x_cut,y_cut=y_cut,
                patch=patch,color_patch=color_patch,red=img_patch_red,green=img_patch_green,
                blue=img_patch_blue))
   }
}
#########################################
