#' Compute the image features for the colonies after edge detection
#'
#' Computes morphological and texture features from the segmented colonies.
#' @param obj 	An array containing labelled objects. Labelled objects are pixel sets with the same unique integer value.
#' @param img_intensity A matrix or a list of matrices, containing the intensity values of the colony images.
#' @param type A character vector containing the function names to be called to compute features with reference intensities.
#'             The choices are \code{"basic"},\code{"moment"} \code{"shape"} and \code{"haralick"}.
#' @param sample_name A character vector containing the names of colonies(mutants),the order is column-based(top-down
#'                    for every column of mutant array).
#' @param row_num   An integer specifies the number of the colony need analysis in horizontal direction.
#' @param col_num   An integer specifies the number of the colony need analysis in vertical direction(for
#'                  the condition of only a few colonies,fix it to 1).
#' @Details
#' @return A matrix of n coloies(mutants) times p features, where p depends on the options of feature types given
#'         to the function.Returns NULL if no object is present.
#'
#'
#'
#' @examples
# feature_compute<-function(obj,img_intensity,type,sample_name,row_num,col_num){
#    comb_fea<-matrix(list(),row_num,col_num)
#    temp_basic<-matrix(list(),row_num,col_num)
#    temp_shape<-matrix(list(),row_num,col_num)
#    temp_moment<-matrix(list(),row_num,col_num)
#    temp_haralick<-matrix(list(),row_num,col_num)
#    basic_mat<-matrix()
#    shape_mat<-matrix()
#    moment_mat<-matrix()
#    haralick_mat<-matrix()
#    # compute_fea_input<-list(edge=obj,img=img_intensity)
#    fill_label<-sapply(obj,function(x) names(sort(table(x[which(x!=0)]),decreasing = TRUE)[1])%>%as.numeric)
#    if(is.null(c("basic","shape","moment","haralick") %in% type)){
#       comb_fea<-NULL
#       return(comb_fea)
#    }
#    else if ("basic" %in% type){
#      basic_fea<-matrix(list(),row_num,col_num)
#      basic_fea<-mapply(computeFeatures.basic,com_fea_input$edge,com_fea_input$img)
#      temp_basic<-mapply(function(x,y) x[y,],basic_fea,fill_label)
#      basic_mat<- do.call(rbind, temp_basic)
#    }
#    else if("shape" %in% type){
#      shape_fea<-matrix(list(),row_num,col_num)
#      shape_fea<-mapply(computeFeatures.shape,com_fea_input$edge,com_fea_input$img)
#      temp_shape<-mapply(function(x,y) x[y,],shape_fea,fill_label)
#      shape_mat<- do.call(rbind, temp_shape)
#    }
#    else if("moment" %in% type){
#      moment_fea<-matrix(list(),row_num,col_num)
#      moment_fea<-mapply(computeFeatures.moment,com_fea_input$edge,com_fea_input$img)
#      temp_moment<-mapply(function(x,y) x[y,],moment_fea,fill_label)
#      moment_mat<- do.call(rbind, temp_moment)
#    }
#    else ("haralick" %in% type){
#      haralick_fea<-matrix(list(),row_num,col_num)
#      haralick_fea<-mapply(computeFeatures.haralick,com_fea_input$edge,com_fea_input$img)
#      temp_haralick<-mapply(function(x,y) x[y,],haralick_fea,fill_label)
#      haralick_mat<- do.call(rbind, temp_haralick)
#    }
#
# }

