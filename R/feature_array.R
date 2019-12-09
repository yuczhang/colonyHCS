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
#'
#' @Details
#' @return A matrix of n coloies(mutants) times p features, where p depends on the options of feature types given
#'         to the function.Returns NULL if no object is present.
#'
#'
#'
#' @examples
feature_array<-function(obj,img_intensity,type,sample_name,row_num,col_num){
   # library(EBImage)
   comb_fea<-matrix(data=NA,ncol=row_num*col_num)
   # temp_basic<-matrix(list(),row_num,col_num)
   # temp_shape<-matrix(list(),row_num,col_num)
   # temp_moment<-matrix(list(),row_num,col_num)
   # temp_haralick<-matrix(list(),row_num,col_num)
   basic_mat<-matrix()
   shape_mat<-matrix()
   moment_mat<-matrix()
   haralick_mat<-matrix()
   # compute_fea_input<-list(edge=obj,img=img_intensity)
   fill_label<-sapply(obj,function(x) names(sort(table(x[which(x!=0)]),decreasing = TRUE)[1])%>%as.numeric)
   # if(is.null(c("basic","shape","moment","haralick") %in% type)){
   #    comb_fea<-NULL
   #    return(comb_fea)
   #    print("No feature type is specified!")
   # }
   # else if ("basic" %in% type){
     #basic_fea<-matrix(list(),row_num,col_num)
     # basic_fea<-mapply(computeFeatures.basic,obj,img_intensity)
     # if (!(class(basic_fea)=="matrix"))
     # basic_mat<-mapply(function(x,y) x[y,],basic_fea,fill_label)
     # else
     # basic_mat<-basic_fea
     #basic_mat<- do.call(rbind, temp_basic)
   # }
   # else if("shape" %in% type){
   #   #shape_fea<-matrix(list(),row_num,col_num)
   #   shape_fea<-mapply(computeFeatures.shape,obj,img_intensity)
   #   if (!(class(shape_fea)=="matrix"))
   #   shape_mat<-mapply(function(x,y) x[y,],shape_fea,fill_label)
   #   else
   #   shape_mat<-shape_fea
   #   #shape_mat<- do.call(rbind, temp_shape)
   # }
   # else if("moment" %in% type){
   #   #moment_fea<-matrix(list(),row_num,col_num)
   #   moment_fea<-mapply(computeFeatures.moment,obj,img_intensity)
   #   if (!(class(moment_fea)=="matrix"))
   #   moment_mat<-mapply(function(x,y) x[y,],moment_fea,fill_label)
   #   else
   #   moment_mat<-moment_fea
   #   #moment_mat<- do.call(rbind, temp_moment)
   # }
   # else ("haralick" %in% type){
   #   #haralick_fea<-matrix(list(),row_num,col_num)
   #   haralick_fea<-mapply(computeFeatures.haralick,obj,img_intensity)
   #   if (!(class(haralick_fea)=="matrix"))
   #   haralick_mat<-mapply(function(x,y) x[y,],haralick_fea,fill_label)
   #   else
   #   haralick_mat<-haralick_fea
   #   #haralick_mat<- do.call(rbind, temp_haralick)
   # }
   switch(type,
     "basic"={
        basic_fea<-mapply(computeFeatures.basic,obj,img_intensity)
        if (!(class(basic_fea)=="matrix"))
           basic_mat<-mapply(function(x,y) x[y,],basic_fea,fill_label)
        else
           basic_mat<-basic_fea
        rownames(basic_mat)<-c("b.mean","b.sd","b.mad","b.q001","b.q005","b.q05","b.q095","b.q099")
     },
     "shape"={
          shape_fea<-mapply(computeFeatures.shape,obj,img_intensity)
          if (!(class(shape_fea)=="matrix"))
          shape_mat<-mapply(function(x,y) x[y,],shape_fea,fill_label)
          else
          shape_mat<-shape_fea
          rownames(shape_mat)<-c("s.area","s.perimeter","s.radius.mean","s.radius.sd","s.radius.min","s.radius.max")
     },
     "moment"={
          moment_fea<-mapply(computeFeatures.moment,obj,img_intensity)
          if (!(class(moment_fea)=="matrix"))
          moment_mat<-mapply(function(x,y) x[y,],moment_fea,fill_label)
          else
          moment_mat<-moment_fea
          rownames(moment_mat)<-c("m.cx","m.cy","m.majoraxis","m.eccentricity","m.theta")
     },
     "haralick"={
          haralick_fea<-mapply(computeFeatures.haralick,obj,img_intensity)
          if (!(class(haralick_fea)=="matrix"))
          haralick_mat<-mapply(function(x,y) x[y,],haralick_fea,fill_label)
          else
          haralick_mat<-haralick_fea
          rownames(haralick_mat)<-c("h.asm.s1","h.con.s1","h.cor.s1","h.var.s1","h.idm.s1","h.sav.s1","h.sva.s1","h.sen.s1",
                                 "h.ent.s1","h.dva.s1","h.den.s1","h.f12.s1","h.f13.s1","h.asm.s2","h.con.s2","h.cor.s2",
                                 "h.var.s2","h.idm.s2","h.sav.s2","h.sva.s2","h.sen.s2","h.ent.s2","h.dva.s2","h.den.s2",
                                 "h.f12.s2","h.f13.s2")

     },
     # "LBP"={
     #
     # }
     stop("No accepted feature type is specified!")
      )
   ###### Combine the feature matrices into one matrix
   fea_mat<-list(basic_mat,shape_mat,moment_mat,haralick_mat)
   names(fea_mat)<-c("basic","shape","moment","haralick")
   check<-sapply(fea_mat,function(x) dim(x)[2])
   fea_mat[which(check!=(row_num*col_num))]<-NULL
   comb_fea<-do.call(rbind,fea_mat)
   return(comb_fea=comb_fea)
}

