#' Cropping of the phenotypic image of the colony(colonies) on single culture dish into single patches.
#'
#' Before the processing and extraction of phenotypic signitures ,if the experiment was in quite small scale,
#' it is recommeded to get the cropped patch(es) containing single colony based on manually decided coordinates
#' using \code{colony_manual_crop}
#' @param expr_mode a string indicates the phenotypic experiment type, as different experiments require differnt
#'                  processing steps
#' @param img_gray  an 'Image' object or an array stores the raw image in gray mode and specific size
#' @param img_ori   an 'Image' object or an array stores the raw phenotypic image in color or gray mode
#' @param row_num   an integer,represents the number of coloies within the culture dish
#' @param col_num   an integer,represents the number of row within the raw image(for the condition of only a few colonies,fix it to 1)
#' @param cut_coord a matrix with the dimension row_num*(col_num*4)(4 is the number of combined position of x and y direction,which
#'                  could be obtained based on the interactive visualization using EBImage)
#'
#' @return a list contains the segmented colony patches,both in color and gray mode, and R,G,B channels(for the
#'         phenotypes rely on color change),as well as the dimension of the preliminary cropped rectangular image
#'
#' @examples
#' img_patch<-single_colony_seg(expr_mode="swarming",img_gray=img$gray_target,img_ori=img$ori_img,smth_win=0.09,
#'                              row_num=5,col_num=1,cut_coord=cut_coord)
#' @seealso \code{\link{colony_auto_crop}}   \code{\link[EBImage]{display}}
colony_manual_crop<-function(expr_mode="swarming",img_gray,img_ori,row_num=5,col_num=1,cut_coord){
  patch<-matrix(list(),row_num,col_num)
  color_patch<-matrix(list(),row_num,col_num)
  # library(smoother)
  tt<-as.data.frame(img_gray)
  tt<-as.matrix(tt)
  x1<-1:dim(tt)[2]
  x2<-1:dim(tt)[1]
  # cols<-colSums(tt,na.rm =TRUE)
  # rows<-rowSums(tt,na.rm =TRUE)
  # ys= smoother::smth(cols,window = smth_win,method = "gaussian") #SMOOTHING
  # xs= smoother::smth(rows,window = smth_win,method = "gaussian") #SMOOTHING
  # # Find the local maximum of smoothed sum of columns
  # ys_diff<-diff(ys,lag = 1, differences = 1)
  # # Find the local maximum of smoothed sum of rows
  # # xs_diff<-diff(xs,lag = 1, differences = 1)
  # ny<-length(ys_diff)
  # nx<-length(xs_diff)
  # d1<-ys_diff[1:ny-1]
  # d2<-ys_diff[2:ny]
  # d3<-xs_diff[1:nx-1]
  # d4<-xs_diff[2:nx]
  # if(expr_mode!="pro_act"){
  #   y_indmax<-which(d1*d2<0 & d1<0)+1
  #   x_indmax<-which(d3*d4<0 & d3<0)+1
  #   x_cut<-c(0,x_indmax,dim(tt)[1])
  #   y_cut<-c(0,y_indmax,dim(tt)[2])
  #   # x_cut<-c(0,145,290,439,572,738,847,1024)
  #   # y_cut<-c(0,149,300,439,573,750,883,1024)
  # } else {
  #   y_indmax<-which(d1*d2<0 & d1>0)+1
  #   x_indmax<-which(d3*d4<0 & d3>0)+1
  #   x_cut<-c(0,x_indmax[c(2,4,6,8,10,12)],dim(tt)[1])
  #   y_cut<-c(0,y_indmax[c(2,4,6,8,10,12)],dim(tt)[2])
  # }
    if(expr_mode!="Congo_red"){
    for (i in 1:row_num){
      for (j in 1:col_num){
        patch[[i,j]]<-tt[cut_coord[i,(j-1)*4+1]:cut_coord[i,(j-1)*4+2],cut_coord[i,(j-1)*4+3]:cut_coord[i,j*4]]
        color_patch[[i,j]]<-patch[[i,j]]
      }
    }
    return(list(x1=x1,x2=x2,patch=patch,color_patch=color_patch))
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
        patch[[i,j]]<-tt[cut_coord[i,(j-1)*4+1]:cut_coord[i,(j-1)*4+2],cut_coord[i,(j-1)*4+3]:cut_coord[i,j*4]]
        img_patch_red[[i,j]]<-tt_red[cut_coord[i,(j-1)*4+1]:cut_coord[i,(j-1)*4+2],cut_coord[i,(j-1)*4+3]:cut_coord[i,j*4]]
        img_patch_green[[i,j]]<-tt_green[cut_coord[i,(j-1)*4+1]:cut_coord[i,(j-1)*4+2],cut_coord[i,(j-1)*4+3]:cut_coord[i,j*4]]
        img_patch_blue[[i,j]]<-tt_blue[cut_coord[i,(j-1)*4+1]:cut_coord[i,(j-1)*4+2],cut_coord[i,(j-1)*4+3]:cut_coord[i,j*4]]
        color_patch[[i,j]]<-EBImage::rgbImage(img_patch_red[[i,j]],img_patch_green[[i,j]],
                                              img_patch_blue[[i,j]])
      }
    }
    return(list(x1=x1,x2=x2,patch=patch,color_patch=color_patch,red=img_patch_red,green=img_patch_green,
                blue=img_patch_blue))
  }
}
#########################################

