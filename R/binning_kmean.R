#' The K-means Binning
#' The k-means binning method first gives the center number, classifies the observation points using the Euclidean distance calculation and the distance from the center point,
#' and then recalculates the center point until the center point no longer changes, and uses the classification result as the binning of the result.
#' @param df A data.frame with independent variables and target variable.
#' @param feat A name of index variable name.
#' @param label A name of target variable.
#' @param nbins Number of bins,default:3.
#'
#' @return A data frame, including the contents of the bin, the upper bound of the bin, the lower bound of the bin, and all the contents returned by the get_IV function.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata" , "accepts.csv" , package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' ddd <- binning_kmean( df = feature, feat= 'loan_term', label = 'bad_ind', nbins = 3)
binning_kmean<- function( df, feat ,label , nbins = 3 ){

  k_means <- stats::kmeans(df[,c(feat)], nbins)
  depreciation<- rep(NA, length(  nbins ) )

  variable<- rep(feat, length(  nbins ) )
  class<- rep(NA, length(  nbins ) )
  lower   <- rep(NA,length( nbins ))
  upper <- rep(NA,length( nbins ))

  method <- rep( 'k_means' , length( nbins ) )
  df$equal_width <- k_means$cluster
  df$equal_width_2<-''


  for (b in 1:nbins ) {

    depreciation[b]<-max(  df[,c(feat)][which(  df$equal_width == b)]   )

  }

  depreciation <- sort(depreciation )

  df<- df[,c(label,'equal_width','equal_width_2')]

  cnt=0
  for ( j in depreciation) {

    cnt=cnt +1

    if( cnt ==1){

      df$equal_width_2[which(df$equal_width== cnt)]<- paste("(",'-inf' ,',', depreciation[1] ,"]", sep = "")
      class[1]<- paste("(",'-inf' ,',', depreciation[1] ,"]", sep = "")
      lower[1]<- -Inf
      upper[1]<- depreciation[1]

    }else if(cnt == nbins ){

      df$equal_width_2[which(df$equal_width== cnt)]<-  paste("(",depreciation[cnt-1] ,',','inf',"]", sep = "")
      class[nbins]<- paste("(",depreciation[cnt-1] ,',','inf',"]", sep = "")
      lower[nbins]<- depreciation[cnt-1]
      upper[nbins]<- Inf

    }else{

      df$equal_width_2[which(df$equal_width== cnt)]<-  paste("(",depreciation[cnt-1] ,',', depreciation[cnt] ,"]", sep = "")
      class[cnt]<- paste("(",depreciation[cnt-1] ,',', depreciation[cnt] ,"]", sep = "")
      lower[cnt]<- depreciation[cnt-1]
      upper[cnt]<- depreciation[cnt]
    }


  }

  df2<- df

  head<- data.frame( method, variable,class ,lower,upper  )
  df2[,c(feat)] <- df$equal_width_2

  iv_1 = get_IV(df=df2 ,feat= feat , label=label  )
  iv_2<-merge(x= head ,y= iv_1  ,by=c("variable",'class') ,all.x=T)

  return(   iv_2 )

}
