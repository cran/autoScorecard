#' Data Painter Function
#' Draw K-S diagram, Lorenz diagram, lift diagram and  AUC diagram.
#'
#' @param label A target variable.
#' @param pred A predictor variable.
#'
#' @return No return value, called for side effects
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
#' lg<-stats::glm(bad_ind~.,family=stats::binomial(link='logit'),data= woe_train)
#' lg_both<-stats::step(lg,direction = "both")
#' logit<-stats::predict(lg_both,woe_test)
#' woe_test$lg_both_p<-exp(logit)/(1+exp(logit))
#' plot_board( label= woe_test$bad_ind, pred = woe_test$lg_both_p   )
plot_board<- function( label  , pred  ){


  #library(ROCR)
  pred_both <- ROCR::prediction( pred , label)

  perf_both <- ROCR::performance(pred_both,"tpr","fpr")
  graphics::plot(perf_both,col='blue',main="ROC of Models")

  graphics::abline(0,1,lty=2,col='red')

  lr_m_auc<-round(as.numeric(ROCR::performance(pred_both,'auc')@y.values),3)
  lr_m_str<-paste("AUC:",lr_m_auc,sep="")
  graphics::legend(x=0.5,y=0.55,c(lr_m_str), 2:8)




  #Lorenz
  pred_Tr <- ROCR::prediction(  pred , label)
  tpr <- ROCR::performance(pred_Tr,measure='tpr')@y.values[[1]]
  depth <- ROCR::performance(pred_Tr,measure='rpp')@y.values[[1]]
  graphics::plot(depth,tpr,type='l',col='red',main='Lorenz',ylab='tpr',xlab='depth')


  #lift
  #library(ROCR)
  pred_Tr <- ROCR::prediction(  pred , label)
  lift <- ROCR::performance(pred_Tr,measure='lift')@y.values[[1]]
  depth <- ROCR::performance(pred_Tr,measure='rpp')@y.values[[1]]
  graphics::plot(depth,lift,type='l',col='red',main='lift',ylab='lift',xlab='depth')



  #K-S
  pred_Tr <- ROCR::prediction(  pred , label)
  tpr <- ROCR::performance(pred_Tr,measure='tpr')@y.values[[1]]
  fpr <- ROCR::performance(pred_Tr,measure='fpr')@y.values[[1]]
  ks<-(tpr-fpr)
  depth <- ROCR::performance(pred_Tr,measure='rpp')@y.values[[1]]
  graphics::plot(depth,ks,type='l',col='red',main='K-S',ylab='KS',xlab='depth')
  kslable<-paste("KS:",max(ks),sep="")
  legend(0.3,0.2,c(kslable),2:8)


}
