#' Compare the Distribution of the Two Data
#'
#' @param df1 A data.
#' @param df2 A data.
#' @param key_var A name of index variable name.
#' @param y_var A name of target variable.
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
#' comparison_two_data( df1 = train , df2 =  test ,
#' key_var = c("application_id","account_number"), y_var="bad_ind"   )
comparison_two_data<-function( df1  , df2   , key_var  ,  y_var        ){

  Xvar_df<- df1[-which(colnames(df1) %in% c(key_var, y_var))]


  for (col in colnames(Xvar_df) ) {

    comparison_two( var_A = df1[,c(col)] ,var_B = df2[,c(col)] , name_A = paste( deparse(substitute(df1)) , col ,sep = "--") , name_B =  paste(deparse(substitute(df2)) , col ,sep = "--") )
  }

}
