#' Compare the Distribution of the Two Variable
#' Draw box plots, cdf plot , QQ plots and histograms for two data.
#' @param var_A A variable.
#' @param var_B A variable.
#' @param name_A The name of data A.
#' @param name_B The name of data B.
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' accepts <- read.csv(system.file("extdata", "accepts.csv", package = "autoScorecard" ))
#' comparison_two( var_A = accepts$purch_price ,var_B = accepts$tot_rev_line ,
#' name_A = 'purch_price' , name_B = "tot_rev_line"  )
comparison_two<-function( var_A ,var_B , name_A , name_B  ){


  graphics::boxplot(var_A ,var_B ,col = c("orange", "yellow"),cex=0.5, pch=20,outcol="#F0027F",
          main = "Boxplot",ylab="Value",xlab="Feature")
  graphics::legend(2, 9, c(  name_A , name_B   ),
         fill = c("yellow", "orange"))



  h1<-graphics::hist(var_A  , breaks = 20,plot =F )
  h2<-graphics::hist(var_B  , breaks = 20,plot =F)

  graphics::par(mfrow=c(1,2))
  plot(h1,col =grDevices::rgb(0.1,0.5,0.1 ,alpha=0.5),xlab=  name_A  , main= "Histogram"    )
  plot(h2,col =grDevices::rgb(0.2,0.2,0.5 ,alpha=0.5),xlab=  name_B  , main= "Histogram"    )



  ###


  graphics::par(mfrow=c(1,1))
  plot(stats::density( var_A ,na.rm = T )
       ,col = "red",type="l",lwd=2,
       main = "Kernel Density Estimation",xlab= name_A  )

  graphics::lines(stats::density(var_B ,na.rm = T )
        ,col = "blue",type="l",lwd=2)


  graphics::legend("topright",legend=c(  name_A , name_B  )
         ,fill=c("red","blue"))

  ###

  CDF1 <- stats::ecdf(var_A )
  CDF2 <- stats::ecdf(var_B )
  # draw the cdf plot
  plot( CDF1 ,col = "red"  , main = "Cumulative Distribution" )
  graphics::lines(CDF2 ,col = "blue" ,lwd=2  )

  graphics::legend("right",legend=c(  name_A , name_B  )
         ,fill=c("red","blue"))



  ###
  graphics::par(mfrow=c(1,2))
  stats::qqnorm(var_A,col = "red",main = paste("Normal Q-Q Plot" , name_A  ,sep = "--") )
  stats::qqline(var_A)

  stats::qqnorm(var_B,col= "blue",main = paste("Normal Q-Q Plot" , name_B  ,sep = "--") )
  stats::qqline(var_B)
  graphics::legend("center",legend=c(  name_A , name_B  )
         ,fill=c("red","blue"))
}
