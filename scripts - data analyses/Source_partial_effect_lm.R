
#' Plot effect for one variable from your linear regression model output
#' @description This function takes the linear regression model you estimate with lm() for your final
#' projects and returns a plot of an effect for a selected variable.
#'
#' @param model The object name of your linear regression model. Example - if you run the
#' following command: model1 <- lm(y~x), then enter 'model1' for this argument.
#'
#' @param var The variable to plot change in predicted values of 'y.' Your 'x' variable.
#'
#' @param level Confidence level, .95 is the default.
#'
#' @param categoricalIV Indicator for whether the plotted IV is a dummy variable, default is FALSE
#'
#' @param x.label Option to change x-axis label in plot
#' @param y.label Option to change y-axis label in plot
#' @param plot.title Option to add title to plot
#'
#' @param grid Option to plot with grid lines, default is FALSE
#'
#' @param rug Option to plot with rug to represent data density in plot, default is FALSE
#'
#' @param ci.color Color for confidence interval polygons, default is 'lightgray'.
#' @param line.width Option to change fit regression line thickness, default is 1.
#' @param pt.color Point color for average estimated effect of dummy independent variable, default is 'black'.
#' @param pt.size  Point size for average estimated effect of dummy independent variable, default is 1.
#' @param pt.type Point style of regression parameter point estimate. Default is 16, circle. See ?points
#' for information on different styles.
#' @param plot.title Optional plot title.
#' @param x.value.labels Optional list of character names (must match length of categories)
#'
#' @param connect For categorical IV plots, add connecting line between points.
#' @param connect.lwd Optional line width argument for connecting line in categorical IV plots.
#' @param connect.col Optional line color option for connecting line in categorical IV plots.
#'
#' @examples
#' # Estimate a regression using 'mtcars' data:
#' m <- lm(mpg~disp+hp+wt+am,data=mtcars)
#'
#' # Plot effect of 'wt' variable
#' effect(m,'wt')
#'
#' # Plot effect of dummy variable, 'am' with custom labels
#' effect(m,'am',categoricalIV=TRUE,x.value.labels=c('Automatic','Manual'))
#'
#' # Plot with user-defined x-axis label
#' effect(m,'wt',x.label='Vehicle Weight')
#'
#' # Plot with grid lines and rug
#' effect(m,'wt',x.label='Vehicle Weight',grid=TRUE,rug=TRUE)


effect <- function(model,var,level=0.95,categoricalIV=F,
                   x.label=NULL,y.label=NULL,title=NULL,
                   grid=F,rug=F,ci.color='lightgray',
                   line.width=1,line.color='black',
                   pt.color='black',pt.size=1,pt.type=16,
                   plot.title=NULL,x.value.labels=NULL,
                   connect=FALSE,connect.lwd=1,connect.col='black') {
  
  # Model values
  b <- model$coefficients
  n <- nrow(model.matrix(model))
  k <- length(b)
  
  # Model data
  d <- tryCatch( {mean.mat(model,var)}, error=function(x) {stop(paste('You have selected a variable not in your original model. Check your spelling.'),call.=F)} )
  x <- d[var][,1]
  y <- model.frame(model)[,1]
  
  # Calculate critical t-value based on user defined level
  t.val <- qt(((1-level)/2)+level, n - k)
  
  
  
  # REGRESSION QUANTITIES OF INTEREST ###############################
  
  # SSE and MSE
  sse <- sum((y - model$fitted.values)^2)
  mse <- sse / (n - k)
  
  # Predicted values
  y.fit <- as.numeric(as.matrix(d) %*% b)
  
  # Standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - k)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  
  # Calculate lower and upper bounds of predicted values
  slope.upper <- y.fit + t.val * se
  slope.lower <- y.fit - t.val * se
  
  
  # Store predicted values and ses in ordered data frame
  d <- data.frame('x'=x,
                  'y.fit'=y.fit,
                  'y.lci'=slope.lower,
                  'y.uci'=slope.upper)
  
  d <- d[order(x), ]
  
  
  
  # PLOT ############################################################
  
  # Set up plot environment
  plot.x.lab <- ifelse(!is.null(x.label),x.label,'IV')
  
  if(categoricalIV==F){
    
    with(d,plot(x='',y='',
                xlim=c(min(x),max(x)),
                ylim=c(-40,100),
                xlab=plot.x.lab,
                ylab=ifelse(!is.null(y.label),y.label,'DV'),
                main=ifelse(!is.null(plot.title),plot.title,''),
                yaxt='n'))
    axis(2,las=1)
    if(grid==T) grid()
    if(rug==T)  rug(x)
    
    # Add confidence intervals
    with(d,polygon(c(x,rev(x)),c(y.lci,rev(y.uci)),col=ci.color,border=NA))
    
    # Add predicted values line
    with(d,lines(x,y.fit,lwd=line.width,col=line.color))
    
  }
  
  else{
    d <- unique(d)
    
    with(d, plot(x='',y='',
                 xlim=c(head(unique(d$x),n=1)-0.25,tail(unique(d$x),n=1)+0.25),
                 ylim=c(-40,100),
                 xlab=plot.x.lab <- ifelse(!is.null(x.label),x.label,'IV'),
                 ylab=ifelse(!is.null(y.label),y.label,'DV'),
                 xaxt='n',yaxt='n'))
    axis(1,
         at     = unique(d$x),
         labels = unlist(ifelse(!is.null(x.value.labels),
                                list(x.value.labels),
                                list(as.character(unique(d$x))))))
    
    axis(2,las=1)
    
    if(grid==T) grid(NA,NULL)
    
    with(d,segments(x0=x,y0=y.lci,
                    x1=x,y1=y.uci,
                    lty=1,lwd=line.width))
    
    with(d,segments(x0=x-0.025,y0=y.lci,
                    x1=x+0.025,y1=y.lci,
                    lty=1,lwd=line.width))
    
    with(d,segments(x0=x-0.025,y0=y.uci,
                    x1=x+0.025,y1=y.uci,
                    lty=1,lwd=line.width))
    
    with(d,points(x=x,y=y.fit,pch=pt.type,col=pt.color,cex=pt.size))
    
    if(connect==T) lines(d$x,d$y.fit,lty=2,lwd=connect.lwd,col=connect.col)
  }
  
  # Add title if desired
  if(!is.null(plot.title)){title(plot.title)}
  
}

mean.mat <- function(model,var) {
  x <- as.numeric(model.matrix(model)[,var])
  
  d <- colMeans(model.matrix(model))
  
  d <- data.frame(as.matrix(t(d),nrow=1))
  colnames(d) <- variable.names(model)
  
  d <- d[rep(seq_len(nrow(d)), each=length(x)),]
  rownames(d) <- 1:nrow(d)
  d[var] <- x
  
  return(d)
}

# add line to other plot
effect2 <- function(model,var,level=0.95,categoricalIV=F,
                   x.label=NULL,y.label=NULL,title=NULL,
                   grid=F,rug=F,ci.color='lightgray',
                   line.width=1,line.color='black',line.type = 1,
                   pt.color='black',pt.size=1,pt.type=16,
                   plot.title=NULL,x.value.labels=NULL,
                   connect=FALSE,connect.lwd=1,connect.col='black') {
  
  # Model values
  b <- model$coefficients
  n <- nrow(model.matrix(model))
  k <- length(b)
  
  # Model data
  d <- tryCatch( {mean.mat(model,var)}, error=function(x) {stop(paste('You have selected a variable not in your original model. Check your spelling.'),call.=F)} )
  x <- d[var][,1]
  y <- model.frame(model)[,1]
  
  # Calculate critical t-value based on user defined level
  t.val <- qt(((1-level)/2)+level, n - k)
  
  
  
  # REGRESSION QUANTITIES OF INTEREST ###############################
  
  # SSE and MSE
  sse <- sum((y - model$fitted.values)^2)
  mse <- sse / (n - k)
  
  # Predicted values
  y.fit <- as.numeric(as.matrix(d) %*% b)
  
  # Standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - k)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  
  # Calculate lower and upper bounds of predicted values
  slope.upper <- y.fit + t.val * se
  slope.lower <- y.fit - t.val * se
  
  
  # Store predicted values and ses in ordered data frame
  d <- data.frame('x'=x,
                  'y.fit'=y.fit,
                  'y.lci'=slope.lower,
                  'y.uci'=slope.upper)
  
  d <- d[order(x), ]
  
  
  
  # PLOT to other plot ######################################################
  
    # Add confidence intervals
   # with(d,polygon(c(x,rev(x)),c(y.lci,rev(y.uci)),col=ci.color,border=NA))
    
    # Add predicted values line
    with(d,lines(x,y.fit,lwd=line.width,col=line.color,lty=line.type))

}
