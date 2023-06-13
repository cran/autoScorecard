#' Functions to Automatically Generate Scorecards
#'
#' @param feature   A data.frame with independent variables and target variable.
#' @param key_var   A name of index variable name.
#' @param y_var   A name of target variable.
#' @param sample_rate   Training set sampling percentage.
#' @param points0   Base point.
#' @param odds0   odds.
#' @param pdo   Point-to Double Odds.
#' @param missing_rate   Data missing rate, variables smaller than this setting will be deleted.
#' @param single_var_rate   The maximum proportion of a single variable, the variable greater than the setting will be deleted.
#' @param iv_set   IV value minimum threshold, variable IV value less than the setting will be deleted.
#' @param char_to_number   Whether to convert character variables to numeric.
#' @param na.omit   na.omit returns the object with incomplete cases removed.
#' @param max_depth   Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines.
#' @param tree_p   Meet the following conversion formula: minbucket = round(  p*nrow( df )).Smallest bucket(rpart):Minimum number of observations in any terminal <leaf> node.
#' @param k Each scale doubles the probability of default several times.
#' @param base0 Whether the scorecard base score is 0.
#'
#' @return A list containing data, bins, scorecards and models.
#' @export
#'
#' @examples
#' accepts <- read.csv(system.file("extdata", "accepts.csv", package = "autoScorecard" ))
#' auto_scorecard1 <- auto_scorecard( feature = accepts[1:2000,], key_var= "application_id",
#' y_var = "bad_ind",sample_rate = 0.7, points0 = 600, odds0=1/20, pdo = 50, max_depth = 3,
#' tree_p = 0.1, missing_rate = 0, single_var_rate = 1, iv_set = 0.02,
#' char_to_number = TRUE , na.omit = TRUE)
auto_scorecard<-function( feature = accepts, key_var = "application_id", y_var = "bad_ind"
                          ,sample_rate = 0.7 , base0=FALSE , points0 = 600, odds0 = 1/20, pdo = 50, k=2, max_depth = 3, tree_p = 0.1
                          ,missing_rate = 0 ,  single_var_rate = 1, iv_set = 0.02, char_to_number = TRUE , na.omit = TRUE){


  feature<- filter_var( df=feature , key_var=key_var, y_var=y_var , missing_rate=missing_rate ,single_var_rate=single_var_rate,iv_set=iv_set, char_to_number=char_to_number , na.omit=na.omit)


  #select training sample
  d = sort(sample(nrow(feature), nrow(feature)*sample_rate))
  d = (sample(nrow(feature), nrow(feature)*sample_rate))

  train<-feature[d,]
  test <-feature[-d,]


  treebins_train <- bins_tree(df= train, key_var= key_var, y_var=y_var, max_depth=max_depth, p=tree_p)

  woe_train <- rep_woe(  df= train ,key_var=key_var, y_var=y_var  , tool=treebins_train ,var_label= "variable",col_woe='woe', lower='lower' ,upper ='upper'  )
  woe_test <-  rep_woe(  df= test ,key_var=key_var, y_var=y_var  , tool=treebins_train ,var_label= "variable",col_woe='woe', lower='lower' ,upper ='upper'  )


  lg<-stats::glm(bad_ind~.,family=stats::binomial(link='logit'),data= woe_train)

  lg_both<-stats::step(lg,direction = "both")





  logit<-stats::predict(lg_both,woe_test)
  woe_test$lg_both_p<-exp(logit)/(1+exp(logit))


  pred_both <- ROCR::prediction(woe_test$lg_both_p, woe_test$bad_ind)
  perf_both <- ROCR::performance(pred_both,"tpr","fpr")


  #graphics::plot(perf_both,col='green',main="ROC of Models")

  #graphics::abline(0,1,lty=2,col='red')

  #lr_m_auc<-round(as.numeric( ROCR::performance(pred_both,'auc')@y.values),3)
  #lr_m_str<-paste("Mode_both-AUC:",lr_m_auc,sep="")
  #graphics::legend(0.5,0.55,c(lr_m_str),2:8)


  coe = (lg_both$coefficients)

  cor1<-stats::cor(  Xvar_df<- woe_train[-which(colnames(feature) %in% c(key_var, y_var))]       )

  corrplot::corrplot(cor1)
  corrplot::corrplot(cor1,method = "number")

   if(base0){
     Score<-noauto_scorecard2( bins_card= woe_test, fit= lg_both,bins_woe=treebins_train  ,points0 = points0, odds0 = odds0, pdo = pdo ,k = k)
     Score_2<-noauto_scorecard2( bins_card= woe_train, fit= lg_both,bins_woe=treebins_train  ,points0 = points0, odds0 = odds0, pdo = pdo ,k = k)


   }else{

     Score<-noauto_scorecard( bins_card= woe_test, fit= lg_both,bins_woe=treebins_train  ,points0 = points0, odds0 = odds0, pdo = pdo ,k = k)
     Score_2<-noauto_scorecard( bins_card= woe_train, fit= lg_both,bins_woe=treebins_train  ,points0 = points0, odds0 = odds0, pdo = pdo ,k = k)

   }

  Score['train']   <-  list( as.data.frame(train )  )
  Score['test']    <-  list( as.data.frame(test )  )
  Score['woe_train']<- list( as.data.frame(woe_train )  )
  Score['woe_test']<-  list( as.data.frame(woe_test )  )

  Score['lg']     <-   list( lg )
  Score['lg_both']<-   list( lg_both  )

  data_train <- Score_2$data_score
  data_test <- Score$data_score
  psi_1<-psi_cal( df_train = data_train , df_test = data_test,feat='Score',label='bad_ind' , nbins=10)
  Score['PSI']<-   list( psi_1  )


  plot_board( label= woe_test$bad_ind, pred = woe_test$lg_both_p   )

  return( Score  )
}

