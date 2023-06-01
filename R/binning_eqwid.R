#' Equal Width Binning
#'
#' @param df A data.frame with independent variables and target variable.
#' @param feat A name of dependent variable.
#' @param label A name of target variable.
#' @param nbins Number of bins,default:3.
#'
#' @return A data frame, including the contents of the bin, the upper bound of the bin, the lower bound of the bin, and all the contents returned by the get_IV function.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata", "accepts.csv" , package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' binning_eqwid1 <- binning_eqwid( df = feature, feat = 'tot_derog', label = 'bad_ind', nbins = 3 )
binning_eqwid<- function( df, feat,label , nbins=3){

  equal_width <- infotheo::discretize(df[,c(feat)],"equalwidth",nbins)
  width <- (max(df[,c(feat)])-min(df[,c(feat)]))/nbins

  depreciation <- width * c(1:nbins) + min(df[,c(feat)])

  variable<- rep(feat, length(  nbins ) )
  class<- rep(NA, length(  nbins ) )
  lower   <- rep(NA,length( nbins ))
  upper <- rep(NA,length( nbins ))
  method <- rep( 'equal_width' , length( nbins ) )
  df$equal_width <- equal_width$X
  df$equal_width_2<-''


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

  head<- data.frame(method, variable,class ,lower,upper )
  df2[,c(feat)] <- df$equal_width_2

  iv_1 = get_IV(df=df2 ,feat= feat , label=label  )
  iv_2<-merge(x= head ,y= iv_1  ,by=c("variable",'class') ,all.x=T)

  return(   iv_2)

}
