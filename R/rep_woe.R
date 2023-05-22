#' Replace Feature Data by Binning Template
#'
#' @param df  A data.frame with independent variables and target variable.
#' @param key_var A name of index variable name.
#' @param y_var A name of target variable.
#' @param tool Binning template.
#' @param var_label The name of the characteristic variable.
#' @param col_woe The name of the woe variable
#' @param lower The name of the binning lower bound.
#' @param upper The name of the binning upper bound.
#'
#' @return A data frame of woe
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata", "accepts.csv", package ="autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' all2 <- bins_tree( df = feature, key_var = "application_id", y_var = "bad_ind",
#' max_depth = 3, p= 0.1)
#' re2 <- rep_woe(  df= feature ,key_var = "application_id", y_var = "bad_ind",
#' tool = all2, var_label = "variable",col_woe ='woe', lower ='lower',upper ='upper')
rep_woe<- function(  df  ,key_var, y_var , tool, var_label, col_woe, lower ,upper  ){

  df_result <- df[, c(key_var, y_var)]
  Xvar_df<- df[-which(colnames(df) %in% c(key_var, y_var))]

  for (col in colnames(Xvar_df)) {
    tool_temp <- subset( tool , variable == col  )
    df_result[, c(col)] <- Xvar_df[, c(col)]

    for (  j in 1:nrow( tool_temp)) {

      df_result[, c(col)][  which(  (  df_result[, c(col)] > tool_temp[, lower][j] )  & (df_result[, c(col)] <= tool_temp[, upper][j] )   )      ] <- tool_temp[, col_woe ][j]

    }

  }

  return( df_result  )

}

