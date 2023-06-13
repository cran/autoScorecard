#' Data Description Function
#'
#' @param df A data.
#' @param key_var A name of index variable name.
#' @param y_var A name of target variable.
#'
#' @return A data frame of data description.
#' @export
#'
#' @examples
#' accepts <- read.csv(system.file("extdata", "accepts.csv", package = "autoScorecard" ))
#' aaa <- data_detect( df = accepts, key_var = c("application_id","account_number") ,
#'  y_var = "bad_ind" )
data_detect<-function( df , key_var , y_var  ){


  df_bin <- df[, c(key_var, y_var)]
  Xvar_df<- df[-which(colnames(df) %in% c(key_var, y_var))]


  variable<-  rep(NA, ncol(Xvar_df) )
  class    <-  rep(NA, ncol(Xvar_df) )
  nrow    <- rep(NA, ncol(Xvar_df) )
  missing_rate <- rep(NA, ncol(Xvar_df) )
  unique_count<- rep(NA, ncol(Xvar_df) )
  identical_rate <- rep(NA, ncol(Xvar_df) )

  min    <- rep(NA, ncol(Xvar_df) )
  p25    <- rep(NA, ncol(Xvar_df) )
  p50    <- rep(NA, ncol(Xvar_df) )
  p75    <- rep(NA, ncol(Xvar_df) )
  max     <- rep(NA, ncol(Xvar_df) )
  mean    <- rep(NA, ncol(Xvar_df) )
  sd     <- rep(NA, ncol(Xvar_df) )
  cv     <- rep(NA, ncol(Xvar_df) )

  j<-0
  for (col in colnames(Xvar_df)) {
    j=j+1

    variable[j]  <- col
    class[j]       <- class(  df[,c(col)]  )
    nrow[j]    <-  NROW( df[,c(col)]   )
    missing_rate[j]  <- length( which( is.na(  df[,c(col)]  )   )     )/NROW( df[,c(col)]   )
    unique_count[j]  <-  length(   unique( df[,c(col)]  ) )

    identical_rate[j] <- max(  table(df[,c(col)]) )/NROW( df[,c(col)]   )

    if( is.character(  df[,c(col)] )  ){


    }else{

      min[j]    <-  min(    df[,c(col)]    ,na.rm=T    )
      p25[j]    <-  stats::quantile(df[,c(col)] ,probs=0.25 ,na.rm=T )
      p50[j]    <-  stats::quantile(df[,c(col)] ,probs=0.50,na.rm=T)
      p75[j]    <-  stats::quantile(df[,c(col)] ,probs=0.75,na.rm=T)
      max[j]     <- max(    df[,c(col)]   ,na.rm=T       )
      mean[j]    <- mean(    df[,c(col)]    ,na.rm=T      )
      sd[j]     <-   sd(    df[,c(col)]      ,na.rm=T    )
      cv[j]     <-   sd(    df[,c(col)]     ,na.rm=T     )  /  mean(    df[,c(col)]     ,na.rm=T     )


    }


  }

  head<- data.frame( variable , class,nrow,missing_rate,unique_count,identical_rate,min,p25,p50,p75,max,mean,sd,cv )
  return(head  )
}
