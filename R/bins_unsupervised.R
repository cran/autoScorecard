#' Unsupervised Automatic Binning Function
#' By setting bin_nums, perform three unsupervised automatic binning
#' @param df A data.frame with independent variables and target variable.
#' @param id A name of index.
#' @param label A name of target variable.
#' @param methods Simultaneously calculate three kinds of unsupervised binning("k_means","equal_width","equal_freq" ), the parameters only determine the final output result.
#' @param bin_nums Number of bins.
#'
#' @return A data frame, including the contents of the bin, the upper bound of the bin, the lower bound of the bin, and all the contents returned by the get_IV function.
#' @export
#'
#' @examples
#' accepts <- read.csv( system.file( "extdata" , "accepts.csv" , package = "autoScorecard" ))
#' feature <- stats::na.omit( accepts[,c(1,3,7:23)] )
#' f_1 <-bins_unsupervised(  df = feature , id="application_id" , label="bad_ind" ,
#' methods = c("k_means", "equal_width","equal_freq"  )  ,  bin_nums=10  )
bins_unsupervised<-function(  df , id , label , methods=c( "k_means","equal_width","equal_freq"  )  ,  bin_nums  ){

  feature<-  df
  feature[,c(id)]<- NULL
  feature[,c(label)]<- NULL

  feature_names <-names(  feature )


  i=0

  for ( k in  feature_names   ) {
    i=i+1

    id<- rep(i, length( bin_nums ) )


    j=0
    for ( bin_nums_i in 2:bin_nums) {

      j=j+1

      bins<- rep(bin_nums_i, bin_nums_i )

      #method <- rep( method , bin_nums_i )

      eqwid<- binning_eqwid( df=df, feat= k, label=label,nbins=bin_nums_i)

      eqfreq<-binning_eqfreq( df=df, feat= k, label=label,nbins=bin_nums_i)

      kmean<-binning_kmean( df=df, feat= k, label=label,nbins=bin_nums_i)


      ggg_eqwid<-data.frame( bins  ,eqwid )

      if( length(eqfreq ) !=0 ){

        ggg_eqfreq <-data.frame( bins  ,eqfreq )

      }else{

        ggg_eqfreq <- data.frame()

      }



      ggg_kmean  <-data.frame( bins  ,kmean )

      if(j==1){
        ggg2_eqwid<- ggg_eqwid

        ggg2_kmean<- ggg_kmean

        ggg2_eqfreq<- ggg_eqfreq

      }else{
        ggg2_eqwid<-  rbind( ggg2_eqwid,ggg_eqwid  )

        ggg2_kmean<-  rbind( ggg2_kmean,ggg_kmean  )
        ggg2_eqfreq<-  rbind( ggg2_eqfreq,ggg_eqfreq  )
      }


    }



    if(i==1){
      eee1_eqwid<-data.frame( id  ,ggg2_eqwid )
      eee2_eqwid<- eee1_eqwid

      eee1_kmean<-data.frame( id  ,ggg2_kmean )
      eee2_kmean<- eee1_kmean

      eee1_eqfreq<-data.frame( id  ,ggg2_eqfreq )
      eee2_eqfreq<- eee1_eqfreq


    }else{
      eee1_eqwid<-data.frame( id  ,ggg2_eqwid )
      eee2_eqwid<-  rbind( eee2_eqwid,eee1_eqwid  )

      eee1_kmean<-data.frame( id  ,ggg2_kmean )
      eee2_kmean<-  rbind( eee2_kmean,eee1_kmean  )

      eee1_eqfreq <-  data.frame( id  ,ggg2_eqfreq )
      eee2_eqfreq <-  rbind( eee2_eqfreq,eee1_eqfreq  )

    }

  }




  result <-rbind( eee2_eqwid ,eee2_kmean,eee2_eqfreq )

  result<- subset( result , result[,c('method')]  %in% methods   )

  return( result )
}
