library("dygraphs")
library("reshape")
library("ggplot2")

mainColor= "#E69F00"
secondaryColor = "#333333"

pnl_plot_interactive <- function(x,...){
  x = remove_initialization_time(x)
  pargs<-as.list(match.call(expand.dots=TRUE))
  if(!"ylab" %in% names(pargs)) ylab<-"Return" else ylab<-pargs$ylab #deparse(substitute(x)) 
  if(!"main" %in% names(pargs)) main<-paste(names(pnl_stats(x)),pnl_stats(x),sep=":",collapse=" ") else main<-pargs$main
  plot_arguments<-c(list(data=cumsum(x*100) ,ylab=ylab, main=main),
                    pargs[setdiff(names(pargs),c("","x","ylab","main"))])
  plot_arguments$expand.dots = NULL

  
  CustomAxisLabel <- 'function (d, gran) {
                      return Dygraph.zeropad(d.getMonth()+1) + "/"+ Dygraph.zeropad(d.getFullYear());
                    }'
  CustomValueFormat = 'function (ms) {
                      var d = new Date(ms);
                      return Dygraph.zeropad(d.getMonth()+1) + "/"+ Dygraph.zeropad(d.getFullYear());
                     }'
  
  do.call(dygraph,plot_arguments)  %>% 
    dyRangeSelector() %>%
    dySeries("V1", label = "Cumulative Return (%)",color=mainColor) %>%
    dyAxis("x",pixelsPerLabel=48,axisLabelFormatter =JS(CustomAxisLabel),valueFormatter=JS(CustomValueFormat),ticker="Dygraph.dateTicker") 
}

plotComponentBars <- function(Main_Factors,Comparison_Factors,futures_data,comp,threshold=0.3)  {
  df <- rbind(      Main_Factors[rownames(Comparison_Factors)[!is.na(Comparison_Factors[,comp])],comp],
              Comparison_Factors[rownames(Comparison_Factors)[!is.na(Comparison_Factors[,comp])],comp])
  
  keep <- apply(abs(df) > threshold,2,sum) > 0
  df <- df[,keep]
  
  colnames(df) <- rownames(Comparison_Factors)[keep]
  rownames(df) <- c("2001-2016",paste(head(rownames(futures_data),1),"-", tail(rownames(futures_data),1) ,sep=""))
  
  #order bars
  orderedbars = unique(colnames(df)[sort(df[1,],index.return=T,decreasing=T)$ix])
  df <- melt(t(df))
  df <- transform( df,X1 = ordered(df$X1, levels = orderedbars))
  
  colnames(df) <-c("Component","Period","Value")
  ggplot(df,aes(x=Component,y=Value,fill=Period)) + 
    geom_bar(stat="identity",position=position_dodge(width=0.8),width=0.6)+
    ggtitle(paste("Risk Factor",comp))+
    ylab("Factor Loading")+
    scale_fill_manual(values=c(secondaryColor, mainColor))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background=element_rect(colour = NA, fill = "white"),
          panel.grid.major.y=element_line(colour="#999999"), 
          panel.grid.minor=element_blank(),
          legend.position="top",
          axis.text = element_text(size=14),
          axis.title= element_text(size=16))
}
