#' Manually Input Parameters to Generate Scorecards
#'
#' @param bins_card Binning template
#' @param fit See glm {stats}.
#' @param points0 Base point.
#' @param odds0 odds.
#' @param pdo Point-to Double Odds.
#' @param bins_woe  A data frame of woe with independent variables and target variable.
#'
#' @return a dataframe with score ratings
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata", "accepts.csv" , package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' d = sort( sample( nrow( feature ), nrow( feature )*0.7))
#' train <- feature[d,]
#' test <- feature[-d,]
#' treebins_train <- bins_tree( df = train, key_var = "application_id", y_var="bad_ind",
#' max_depth=3, p=0.1)
#' woe_train <- rep_woe( df= train , key_var = "application_id", y_var = "bad_ind" ,
#' tool = treebins_train ,var_label = "variable",col_woe = 'woe', lower = 'lower' , upper = 'upper')
#' woe_test <- rep_woe(  df = test , key_var ="application_id", y_var= "bad_ind",
#' tool = treebins_train ,var_label= "variable",
#'     col_woe = 'woe', lower = 'lower' ,upper = 'upper'  )
#' lg <- stats::glm( bad_ind~. , family = stats::binomial( link = 'logit' ) , data = woe_train )
#' lg_both <- stats::step( lg , direction = "both")
#' Score <- noauto_scorecard( bins_card= woe_test , fit =lg_both , bins_woe = treebins_train ,
#' points0 = 600 , odds0 = 1/20 , pdo = 50 )
noauto_scorecard<- function( bins_card , fit , bins_woe , points0 = 600, odds0 = 1/19, pdo = 50 ){
  re2<-bins_card
  coe = (fit$coefficients)

  p <- pdo/log(2)
  q <- points0 - pdo*log(odds0)/log(2)

  get_score<- function( coe  ,q,p,re2 ){
    score=0

    for (i in 1:length(coe)) {

      if( i ==1  ){
        score= score + q + p*as.numeric(coe[1])

      }else{

        score= score +  p*as.numeric(coe[i])*re2[,names(coe[i]  )]


      }

    }


    return(score)
  }


  re2$Score<-  round( get_score( coe=coe  ,q=q,p=p,re2=re2),0  )




  base_score =  q + p*as.numeric(coe[1])


  bin_score<- function( bins_woe,q,p,coe  ){

    aaa <- bins_woe
    aaa[,"var_socre"]<- 0.0

    for (  s in 1:nrow( aaa )) {

      c1<-0.0

      for ( i in 2:length(coe) ) {

        if(  aaa[,"variable"][s] ==    names(coe[i] )     ){
          c1 <- as.numeric(coe[i])

        }
      }

      if(  i==length(coe) &  c1==0  ){

      }else{
        aaa[s,"var_socre"] <- round(   p*c1* as.numeric( aaa[s,'woe'] ),0 )

      }

    }


    aaa2<- subset(  aaa   ,aaa$variable %in%   names( coe[-1]  )   )


    return( aaa2  )
  }


  bin_score<- bin_score(bins_woe=bins_woe  ,q=q,p=p,coe=coe  )


  data_score<-re2
  result<-list()
  result['data_score']<-  list( as.data.frame(data_score  )  )
  result['base_score']<-as.data.frame(base_score)
  result['bin_score'] <-list(as.data.frame(bin_score))

  return(  result )

}

