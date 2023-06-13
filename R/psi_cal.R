#' PSI Calculation Function
#'
#' @param df_train Train data.
#' @param df_test Test data.
#' @param feat  A name of index variable name.
#' @param label A name of target variable.
#' @param nbins Number of bins.
#'
#' @return A data frame of PSI.
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
#' Score_2 <- noauto_scorecard( bins_card= woe_test , fit =lg_both , bins_woe = treebins_train ,
#' points0 = 600 , odds0 = 1/20 , pdo = 50 )
#'
#' Score_1<- noauto_scorecard( bins_card = woe_train, fit = lg_both, bins_woe = treebins_train,
#' points0 = 600, odds0 = 1/20, pdo = 50 )
#' psi_1<- psi_cal( df_train = Score_1$data_score , df_test = Score_2$data_score,
#' feat = 'Score',label ='bad_ind' , nbins =10 )
psi_cal<- function( df_train , df_test,feat,label , nbins=10){

  equal_width <- infotheo::discretize(df_train[,c(feat)],"equalwidth",nbins)
  width <- (max(df_train[,c(feat)])-min(df_train[,c(feat)]))/nbins
  depreciation <- width * c(1:nbins) + min(df_train[,c(feat)])

  ######

  variable<- rep(feat, length(  nbins ) )
  class<- rep(NA, length(  nbins ) )
  lower   <- rep(NA,length( nbins ))
  upper <- rep(NA,length( nbins ))

  PSI<-   rep(NA, length( nbins ) )

  train_cnt <- rep(NA, length( nbins ) )
  test_cnt <- rep(NA, length( nbins ) )
  ##########

  df1 <- df_train
  df2 <- df_test
  df1_total_num <- length(df1[,c(label)])
  df2_total_num <- length(df2[,c(label)])

  cnt=0
  for ( j in depreciation) {

    cnt=cnt +1

    if( cnt ==1){

      df1_cnt = length(   df1[,c(feat)][which(df1[,c(feat)] <= depreciation[1]  )]       )
      df2_cnt = length(   df2[,c(feat)][which(df2[,c(feat)] <= depreciation[1]  )]       )
      train_cnt[1]<- df1_cnt
      test_cnt[1]<- df2_cnt

      class[1]<- paste("(",'-Inf' ,',', depreciation[1] ,"]", sep = "")
      lower[1]<- -Inf
      upper[1]<- depreciation[1]

      PSI[1]<- sum(    (  df2_cnt/ df2_total_num ) -(  df1_cnt / df1_total_num )    )* log(  (  df2_cnt/ df2_total_num )  / (  df1_cnt / df1_total_num )    ,base = exp(1))


    }else if(cnt == nbins ){


      df1_cnt =  length(  df1[,c(feat)][which(   df1[,c(feat)] > depreciation[cnt-1]  )]  )
      df2_cnt =  length(  df2[,c(feat)][which(   df2[,c(feat)] > depreciation[cnt-1]  )]  )
      train_cnt[nbins]<- df1_cnt
      test_cnt[nbins]<- df2_cnt

      class[nbins]<- paste("(",depreciation[cnt-1] ,',','Inf',"]", sep = "")
      lower[nbins]<- depreciation[cnt-1]
      upper[nbins]<- Inf
      PSI[nbins]<- sum(    (  df2_cnt/ df2_total_num ) -(  df1_cnt / df1_total_num )    )* log(  (  df2_cnt/ df2_total_num )  / (  df1_cnt / df1_total_num )    ,base = exp(1))


    }else{


      df1_cnt = length(  df1[,c(feat)][which( (  df1[,c(feat)] > depreciation[cnt-1]  )&(   df1[,c(feat)] <= depreciation[cnt]  )       )] )

      df2_cnt = length(  df2[,c(feat)][which( (  df2[,c(feat)] > depreciation[cnt-1]  )&(   df2[,c(feat)] <= depreciation[cnt]  )       )] )

      train_cnt[cnt]<- df1_cnt
      test_cnt[cnt]<- df2_cnt

      class[cnt]<- paste("(",depreciation[cnt-1] ,',', depreciation[cnt] ,"]", sep = "")
      lower[cnt]<- depreciation[cnt-1]
      upper[cnt]<- depreciation[cnt]

      PSI[cnt]<- sum(    (  df2_cnt/ df2_total_num ) -(  df1_cnt / df1_total_num )    )* log(  (  df2_cnt/ df2_total_num )  / (  df1_cnt / df1_total_num )    ,base = exp(1))


    }


  }


  head<- data.frame( variable,class ,lower,upper,train_cnt,test_cnt,PSI )


  ###
  abc<- head
  p1<-  abc[,c("class","train_cnt"  )]
  names(p1)[2]<- "cnt"
  p1$variable<-deparse(substitute(df_train) )

  p1$rate<- round(  p1$cnt  /sum( p1$cnt  )  , 3) *100

  p2<-  abc[,c("class","test_cnt"  )]
  names(p2)[2]<- "cnt"
  p2$variable<-deparse(substitute(df_test) )

  p2$rate<- round(  p2$cnt  /sum( p2$cnt  )  , 3) *100


  abc2<- rbind(    p1  ,   p2   )


  plot1<-ggplot2::ggplot(abc2,  ggplot2::aes(class,abc2[,c("rate")],fill=variable))+

    ggplot2::geom_bar(stat = "identity",color="black",position = ggplot2::position_dodge(),

             width = 0.7,size=0.25) + ggplot2::scale_y_continuous(expand = c(0,0),limits = c(0,25)) +

    ggplot2::theme_classic() +

    ggplot2::theme(panel.background=   ggplot2::element_rect(fill="white",colour="black",size=0.25),

          axis.line = ggplot2::element_line(colour="black",size=0.25),

          axis.title= ggplot2::element_text(size=13,color="black"),

          axis.text = ggplot2::element_text(size=8,color="black"),


          legend.position= c(0.15,0.85)

    )  +ggplot2::labs(x='Score',y='Proportion of score(%)',title= paste( 'PSI:', round(  sum( head[,c("PSI")]  ,na.rm = T)  , 4)     ,sep = " "))

  print( plot1)

  return(   head )

}
