#' Equal Frequency Binning
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
#' accepts <- read.csv( system.file( "extdata", "accepts.csv", package ="autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' binning_eqfreq1 <- binning_eqfreq( df= feature, feat= 'tot_derog', label = 'bad_ind', nbins = 3)
binning_eqfreq<- function( df, feat , label , nbins =3 ){

  equal_freq <- infotheo::discretize(df[,c(feat)],"equalfreq",nbins)
  depreciation<- rep(NA, length(  nbins ) )


  if(  length(  unique(equal_freq$X) ) != nbins  ){

    variable<- rep(feat,   nbins  )

    class<- rep(NA,  nbins  )
    method <- rep( 'equal_freq' ,  nbins  )

    lower   <- rep(NA, nbins )
    upper <- rep(NA, nbins )

    outcome_0 <- rep(NA,nbins )
    outcome_1 <- rep(NA, nbins )
    pct_0 <- rep(NA, nbins )
    pct_1 <- rep(NA, nbins )
    odds<- rep(NA,nbins )
    woe<- rep(NA, nbins )
    miv<- rep(NA, nbins )

    so<- data.frame(variable,class ,method ,lower ,upper,outcome_0,outcome_1 ,pct_0 ,pct_1,odds,woe,miv )
    return( so )
  }



  variable<- rep(feat, length(  nbins ) )
  class<- rep(NA, length(  nbins ) )
  method <- rep( 'equal_freq' , length( nbins ) )
  lower   <- rep(NA,length( nbins ))
  upper <- rep(NA,length( nbins ))


  df$equal_width <- equal_freq$X
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

      df$equal_width_2[which(df$equal_width== cnt)]<- paste("(",'-Inf' ,',', depreciation[1] ,"]", sep = "")
      class[1]<- paste("(",'-Inf' ,',', depreciation[1] ,"]", sep = "")

      lower[1]<- -Inf
      upper[1]<- depreciation[1]

    }else if(cnt == nbins ){

      df$equal_width_2[which(df$equal_width== cnt)]<-  paste("(",depreciation[cnt-1] ,',','Inf',"]", sep = "")
      class[nbins]<- paste("(",depreciation[cnt-1] ,',','Inf',"]", sep = "")

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
  head<- data.frame(method, variable,class,lower, upper )
  df2[,c(feat)] <- df$equal_width_2

  iv_1 = get_IV(df=df2 ,feat= feat , label=label  )
  iv_2<-merge(x= head ,y= iv_1  ,by=c("variable",'class') ,all.x=T)


  return(   iv_2)

}
