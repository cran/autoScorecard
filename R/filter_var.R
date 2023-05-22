#' Data Filtering
#'
#' @param df A data.frame with independent variables and target variable.
#' @param key_var A name of index variable name.
#' @param y_var A name of target variable.
#' @param missing_rate Data missing rate, variables smaller than this setting will be deleted.
#' @param single_var_rate The maximum proportion of a single variable, the variable greater than the setting will be deleted.
#' @param iv_set IV value minimum threshold, variable IV value less than the setting will be deleted.
#' @param char_to_number Whether to convert character variables to numeric.
#' @param na.omit na.omit returns the object with incomplete cases removed.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata" , "accepts.csv",package = "autoScorecard" ))
#' fff1 <- filter_var( df = accepts, key_var = "application_id", y_var = "bad_ind", missing_rate = 0,
#' single_var_rate = 1, iv_set = 0.02 )
filter_var<-function( df , key_var, y_var , missing_rate ,single_var_rate , iv_set, char_to_number = TRUE , na.omit = TRUE){

  if( na.omit  ){
    df<- stats::na.omit(df)

  }


  df_result <- df[, c(key_var, y_var)]

  Xvar_df<- df[-which(colnames(df) %in% c(key_var, y_var))]


  for (col in colnames(Xvar_df)) {

    col_missing_rate= (  length(  which( is.na(df[,col ]   ) )  )  )/(   length(  df[,col ]      ) )


    if( (  char_to_number  ) &   is.character(  df[, col]  )    ){

      df[,col ] <- as.numeric(  as.factor(  df[,col ] )  )
    }


    if (  ( 1-col_missing_rate )   >=    missing_rate     ){


      df_IV = get_IV(df=df ,feat= col , label=y_var  )

      if( sum( df_IV$miv ,na.rm = T ) >= iv_set ){


        if(     max( max(   df_IV$outcome_0 +   df_IV$outcome_1  ) / length(  df[,col ]),1 ) <= single_var_rate      ){


          df_result[, c(col)] <- df[, c(col)]

        }


      }


    }


  }


  return( df_result  )
}
