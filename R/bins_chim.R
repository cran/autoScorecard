#' Chi-Square Binning
#' Chi-square binning, using the ChiMerge algorithm for bottom-up merging based on the chi-square test.
#' @param df A data.frame with independent variables and target variable.
#' @param key_var A name of index variable name.
#' @param y_var A name of target variable.
#' @param alpha Significance level(discretization);
#'
#' @return A data frame, including the contents of the bin, the upper bound of the bin, the lower bound of the bin, and all the contents returned by the get_IV function.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata", "accepts.csv" , package = "autoScorecard" ))
#' feature2 <- stats::na.omit( accepts[1:200,c(1,3,7:23)] )
#' all3 <- bins_chim( df = feature2 , key_var = "application_id", y_var = "bad_ind" , alpha=0.1 )
bins_chim<-function(  df, key_var, y_var, alpha  ){



 # library(discretization)

  df_bin <- df[, c(key_var, y_var)]

  Xvar_df<- df[-which(colnames(df) %in% c(key_var, y_var))]
  lst_bin<- list()


  j<-0

  for (col in colnames(Xvar_df)) {


    if (    !is.character( df[, col] )   ) {



      chi1 <- discretization::chiM(data = df[,c(col,y_var)]  , alpha =  alpha)

      splits <- sort( chi1$cutp[[1]]  )

      vec<-NULL

      if (length(splits) == 1) {
        vec[df[, col] < splits[1]] <- 1
        vec[df[, col] >= splits[1]] <- 2
        inter_bin<- rbind(c(1, -Inf, splits[1]), c(2, splits[1], Inf))
      }
      else if (length(splits) > 1) {
        splits <- sort(splits)
        vec[df[, col] < splits[1]] <- 1
        inter_bin<- matrix(c(1, -Inf, splits[1]), nrow=1)
        rownames(inter_bin)<-1

        for (i in 1:(length(splits) - 1)) {
          vec[df[, col] >= splits[i] & df[, col] < splits[i + 1]] <- i + 1
          inter_bin<- rbind(inter_bin, c(i+1, splits[i], splits[i + 1]))
          rownames(inter_bin)[i+1]<-i+1
        }
        vec[df[, col] >= splits[length(splits)]] <-length(splits) + 1
        inter_bin<- rbind(inter_bin, c(length(splits) + 1, splits[length(splits)], Inf))

        rownames(inter_bin)[length(splits)+1]<-length(splits)+1
      }
      else {
        vec <- 1
        inter_bin<- matrix(c(1, -Inf, Inf), nrow=1)
      }


      df_bin[, col]<-vec



      df_bin2 <- df[, c(key_var, y_var)]
      df_bin2[, c('equal_width')] <- vec
      df_bin2$equal_width_2 <- ''
      depreciation <- sort(splits )



      lower   <- rep(NA,length( length( depreciation )+1 ))
      upper   <- rep(NA,length( length( depreciation )+1 ))
      bin     <- rep(NA,length( length( depreciation )+1 ))
      class   <- rep(NA,length( length( depreciation )+1 ))
      variable <- rep(col,length( length( depreciation )+1 ))


      cnt=0
      for ( jj in 1:( length( depreciation )+1) ) {

        cnt=cnt +1

        if( cnt ==1){

          df_bin2$equal_width_2[which(df_bin2$equal_width== cnt)]<- paste("(",'-Inf' ,',', depreciation[1] ,"]", sep = "")
          class[1]<- paste("(",'-Inf' ,',', depreciation[1] ,"]", sep = "")
          lower[1]   <- -Inf
          upper[1]   <-  depreciation[1]
          bin[1]     <- 1

        }else if(cnt == ( length( splits ) +1) ){

          df_bin2$equal_width_2[which(df_bin2$equal_width== cnt)]<-  paste("(",depreciation[cnt-1] ,',','Inf',"]", sep = "")
          class[length( splits ) +1]<- paste("(",depreciation[cnt-1] ,',','Inf',"]", sep = "")
          lower[length( splits ) +1]   <-  depreciation[cnt-1]
          upper[length( splits ) +1]   <-  Inf
          bin[length( splits ) +1]     <-  length( splits ) +1


        }else{

          df_bin2$equal_width_2[which(df_bin2$equal_width== cnt)]<-  paste("(",depreciation[cnt-1] ,',', depreciation[cnt] ,"]", sep = "")
          class[cnt]<- paste("(",depreciation[cnt-1] ,',', depreciation[cnt] ,"]", sep = "")
          lower[cnt]   <-  depreciation[cnt-1]
          upper[cnt]   <-  depreciation[cnt]
          bin[cnt]     <-  cnt


        }


      }



      head<- data.frame( variable,bin,class,lower ,upper )
      df_bin2[,col] <- df_bin2$equal_width_2


      iv_1 = get_IV(df=df_bin2 ,feat= col , label=y_var  )
      iv_1<-merge(x= head ,y= iv_1  ,by=c("variable",'class') ,all.x=T)



      j<-j+1

      if(j==1){

        ggg1<- iv_1

      }else{

        ggg1<- rbind( ggg1,iv_1 )

      }


    }


  }

  return(ggg1)


}
