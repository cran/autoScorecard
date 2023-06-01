#' Calculate the Best IV Value for the Binned Data
#'
#' @param df A data.frame with independent variables and target variable.
#' @param variable Name of variable.
#' @param bin  Name of bins.
#' @param method  Name of method.
#' @param label_iv Name of IV.
#'
#' @return A data frame of best IV, including the contents of the bin, the upper bound of the bin, the lower bound of the bin, and all the contents returned by the get_IV function.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata" , "accepts.csv" , package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' f_1 <-bins_unsupervised(  df = feature , id="application_id" , label="bad_ind" ,
#' methods = c("k_means", "equal_width","equal_freq"  )  ,  bin_nums=10  )
#' best1 <- best_iv( df=f_1 ,bin=c('bins') ,  method = c('method') ,
#' variable= c( "variable" )  ,label_iv='miv'  )
best_iv<-function( df ,variable,bin ,method,label_iv  ){


  #df[,'bin_method'] <-''
  df[,c('bin_method')] <-  paste(df[,c(bin)] , df[,c(method)] , sep = "")

  #bin_values = unique(df[,c(bin)]  )
  variable_values =  unique(df[,c(variable)]  )

  bin_method_values = unique(df[,c('bin_method')]  )

  max_iv=0
  max_id=0
  max_bin=0

  cnt_1 = 0
  for (j in variable_values) {
    cnt_1 = cnt_1 +1

    for (  i in bin_method_values) {

      iv_vs = sum(df[,c(label_iv)][which(   ( df[,c('bin_method')]==i  )  & (  df[,c(variable)]==j   )   )],na.rm = T)

      if(  ( max_iv < iv_vs  )&(  iv_vs < Inf )    ){

        max_iv <-  iv_vs
        max_bin <- i


      }

    }

    if( cnt_1 ==1){

      result1 <- subset( df , df[,c('bin_method')] == max_bin & variable== j  )
      result2 <- result1
    }else{

      result1 <- subset( df , df[,c('bin_method')] == max_bin & variable== j  )
      result2 <-  rbind(  result2,  result1 )
    }




  }



  result2$bin_method <-NULL

  return( result2   )


}
