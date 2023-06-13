## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(autoScorecard)

#Quick Modeling

##step 1: Import Data
accepts <- read.csv(system.file("extdata", "accepts.csv", package = "autoScorecard" ))

##step 2: Create scorecard
##Considering efficiency and readability, many parameters of this automatic modeling function 
##are default, which requires high-precision modeling and needs to be manually established step by step
auto_scorecard1 <- auto_scorecard( feature = accepts[1:2000,], key_var= "application_id",
                                   y_var = "bad_ind",sample_rate = 0.7, points0 = 600, odds0=1/20, pdo = 50,
                                   max_depth =3,tree_p = 0.1, missing_rate = 0, single_var_rate = 1, iv_set=0.02,
                                   char_to_number = TRUE , na.omit = TRUE)





#Step-by-Step  Modeling

##step 1: Import Data
accepts <- read.csv(system.file("extdata", "accepts.csv", package = "autoScorecard" ))



##step 2: Data Description
data_detect1 <- data_detect( df = accepts, key_var = c("application_id","account_number") ,
                    y_var = "bad_ind" )

head(data_detect1)


##step 3: Data Filtering
feature<- filter_var( df = accepts , key_var = c("application_id","account_number"), y_var = "bad_ind" , 
                      missing_rate = 0 , single_var_rate = 1 , 
                      iv_set = 0.02, char_to_number = TRUE , na.omit = TRUE )



##step 4: Select Training Sample 
d = sort( sample( nrow( feature ), nrow( feature )*0.7))
train <- feature[d,]
test  <- feature[-d,]




##step 5: Data Distribution Comparison
comparison_two_data( df1 = train , df2 = test ,key_var = c("application_id","account_number"), y_var="bad_ind")




##step 6: Automatic binning of data
##Decision Tree Binning
treebins_train <- bins_tree(df= train, key_var= c("application_id","account_number"), y_var="bad_ind", 
                            max_depth=3, p=0.1)


##Equal Frequency Binning
binning_eqfreq1 <- binning_eqfreq( df= train, feat= 'tot_derog', label = 'bad_ind', nbins = 3)


##Equal Width Binning
binning_eqwid1 <- binning_eqwid( df = train, feat = 'tot_derog', label = 'bad_ind', nbins = 3)



##The K-means Binning
binning_kmean1 <- binning_kmean( df = train, feat= 'loan_term', label = 'bad_ind', nbins = 3)



##Chi-Square Binning
bins_chim1 <- bins_chim( df = train[1:200,], key_var = "application_id", y_var = "bad_ind" , alpha=0.1 )



##Unsupervised Automatic Binning Function
f_1 <-bins_unsupervised(  df = feature[1:200,] , id="application_id" , label="bad_ind" ,
                          methods = c("k_means", "equal_width","equal_freq")  ,  bin_nums=5  )



##The Combination of Two Bins Produces the Best Binning Result
best1 <- best_iv( df=f_1 ,bin=c('bins') , method = c('method') ,variable= c( "variable" ) ,label_iv='miv'  )
vs1   <- best_vs( df1 = treebins_train[,-c(3)], df2 = best1[,-c(1:2)] ,variable="variable" ,label_iv='miv' )





##step 7: Replace Feature Data by Binning Template
woe_train <- rep_woe(  df= train ,key_var="application_id", y_var="bad_ind"  , tool=treebins_train ,
                       var_label= "variable",col_woe='woe', lower='lower' ,upper ='upper'  )


woe_test <-  rep_woe(  df= test ,key_var="application_id", y_var="bad_ind"  , tool=treebins_train ,
                       var_label= "variable",col_woe='woe', lower='lower' ,upper ='upper'  )


##step 8: Modeling
lg<-stats::glm(bad_ind~.,family=stats::binomial(link='logit'),data= woe_train)

lg_both<-stats::step(lg,direction = "both")


logit<-stats::predict(lg_both,woe_test)
woe_test$lg_both_p<-exp(logit)/(1+exp(logit))


pred_both <- ROCR::prediction(woe_test$lg_both_p, woe_test$bad_ind)
perf_both <- ROCR::performance(pred_both,"tpr","fpr")




##step 9: Correlation Diagram
coe = (lg_both$coefficients)
cor1<-stats::cor(  Xvar_df<- woe_train[-which(colnames(feature) %in% c("application_id","account_number","bad_ind"))])

corrplot::corrplot(cor1)
corrplot::corrplot(cor1,method = "number")



##step 10: Manually Input Parameters to Generate Scorecards
##scorecard
Score<-noauto_scorecard( bins_card= woe_test, fit= lg_both,bins_woe=treebins_train  ,points0 = 600, 
                         odds0 = 1/20, pdo = 50 ,k = 2)
Score_2<-noauto_scorecard( bins_card= woe_train, fit= lg_both,bins_woe=treebins_train  ,points0 = 600, 
                           odds0 = 1/20, pdo = 50 ,k = 3)


##scorecard2
Score<-noauto_scorecard2( bins_card= woe_test, fit= lg_both,bins_woe=treebins_train  ,points0 = 600,
                          odds0 = 1/20, pdo = 50 ,k = 2)
Score_2<-noauto_scorecard2( bins_card= woe_train, fit= lg_both,bins_woe=treebins_train  ,points0 = 600, 
                            odds0 = 1/20, pdo = 50 ,k = 3)





##step 11: PSI
data_train <- Score_2$data_score
data_test <- Score$data_score
psi_1<-psi_cal( df_train = data_train , df_test = data_test,feat='Score',label='bad_ind' , nbins=10)


##step 12: Data Painter
plot_board( label= woe_test$bad_ind, pred = woe_test$lg_both_p   )






