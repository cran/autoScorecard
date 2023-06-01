#' Function to Calculate IV Value
#'
#' @param df A data.frame with independent variables and target variable.
#' @param feat   A name of dependent variable.
#' @param label  A name of target variable.
#' @param E Constant, should be set to [0,1], used to prevent calculation overflow due to no data in binning.
#' @param woeInf.rep Woe replaces the constant, and when woe is positive or negative infinity, it is replaced by a constant.
#'
#' @return A data frame including counts, proportions, odds, woe, and IV values for each stratum.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata", "accepts.csv", package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' iv1 = get_IV( df= feature ,feat ='tot_derog' , label ='bad_ind'  )
get_IV<-function(df , feat , label,E=0, woeInf.rep=0.0001 ){
  bin_values =  unique(df[,c(feat)]  )

  good_total_num = length(df[,c(label)][which( df[,c(label)]==1 )])
  bad_total_num =  length(df[,c(label)][which( df[,c(label)]==0 )])

  feat_IV = 0

  variable<- rep(NA, length( bin_values ) )
  class<-rep(NA,length( bin_values )  )
  miv<- rep(NA, length( bin_values )  )
  outcome_0<-  rep(NA, length( bin_values ) )
  outcome_1<-   rep(NA, length( bin_values ) )
  pct_0<- rep(NA, length( bin_values ) )
  pct_1<- rep(NA, length( bin_values ) )
  odds<-  rep(NA, length( bin_values ) )
  woe<-   rep(NA, length( bin_values ) )

  j=0
  for (i in bin_values) {

    j=j+1
    good_num = length( df[,c(label)][which(  ( df[,c(label)]==1)  & ( df[,c(feat)]==i ) ) ]  )
    bad_num =  length(df[,c(label)][which( df[,c(label)]==0 & df[,c(feat)]==i )])

    WOE <- log(( (  bad_num +E )   /bad_total_num)/(  (   good_num +E ) /good_total_num),base = exp(1))

    if(  (  woeInf.rep !='null' )  & ( WOE %in% c(Inf,-Inf)  )   ){

      WOE <- woeInf.rep

    }



    IV<- ((bad_num/bad_total_num)-(good_num/good_total_num))*WOE

    variable[j]<-feat
    class[j]<- i
    miv[j]<- IV
    outcome_0[j]<-  bad_num
    outcome_1[j]<-  good_num
    pct_0[j]<- bad_num / bad_total_num
    pct_1[j]<- good_num/good_total_num
    odds[j]<-  bad_num/( bad_num + good_num )  /( 1- (  bad_num/( bad_num + good_num )   )    )
    woe[j]<- WOE

    #feat_IV = feat_IV + IV
    #print(   paste(feat,"-feat_IV: ",feat_IV, sep = "")    )

  }

  S_IV= data.frame(variable,class,outcome_0,outcome_1,pct_0,pct_1,odds,woe,miv)

  return( S_IV)
}

