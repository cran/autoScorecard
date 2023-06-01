#' The Combination of Two Bins Produces the Best Binning Result
#'
#' @param df1 A binned data.
#' @param df2 A binned data.
#' @param variable A name of X variable.
#' @param label_iv  A name of target variable.
#'
#' @return A data frame of best IV.
#' @export
#'
#' @examples
#' accepts <- read.csv(system.file( "extdata", "accepts.csv", package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' all2 <- bins_tree(df = feature, key_var= "application_id", y_var= "bad_ind"
#' , max_depth = 3, p = 0.1 )
#' f_1 <-bins_unsupervised(  df = feature , id="application_id" , label="bad_ind" ,
#' methods = c("k_means", "equal_width","equal_freq"  )  ,  bin_nums=10  )
#' best1 <- best_iv( df=f_1 ,bin=c('bins') ,  method = c('method') ,
#' variable= c( "variable" )  ,label_iv='miv'  )
#' vs1 <- best_vs( df1 = all2[,-c(3)], df2 = best1[,-c(1:2)] ,variable="variable" ,label_iv='miv' )
best_vs<- function( df1  ,  df2 ,variable="variable" ,label_iv='miv'  ){


  variable1_values  =  unique(df1[,c(variable)]  )
  variable2_values  =  unique(df2[,c(variable)]  )


  cnt_1 = 0
  for (j in variable1_values) {

    cnt_1 = cnt_1 +1



    iv_vs1 = sum(df1[,c(label_iv)][which(     df1[,c(variable)]==j      )],na.rm = T)

    iv_vs2 = sum(df2[,c(label_iv)][which(     df2[,c(variable)]==j      )],na.rm = T)

    if(  iv_vs1  > iv_vs2      ){

      if( cnt_1 ==1){

        result1 <- subset( df1 ,  variable == j  )
        result2 <- result1
      }else{

        result1 <- subset( df1 ,  variable == j  )
        result2 <-  rbind(  result2,  result1 )
      }


    }else{

      if( cnt_1 ==1){

        result1 <- subset( df2 ,  variable == j  )
        result2 <- result1
      }else{

        result1 <- subset( df2 ,  variable == j  )
        result2 <-  rbind(  result2,  result1 )
      }


    }



  }


  return(  result2 )

}
