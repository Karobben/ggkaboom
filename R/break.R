library(patchwork)
library(stringr)

Kaboom_break <-  function(p,  Break = c(0, 10, 40, 100),  R=c(1,1)){
    Lay = c()
    for(i in c(1:length(R))){
        Lay = c(Lay, rep(letters[i], R[length(R)-i+1]))
    }
  layout=""
  for( i in Lay){
    layout=paste(layout,i,sep="\n")
  }
  P_list = list()
  for(i in c(1:round((length(Break)/2)-1))){
      p_tmp <- p +  coord_cartesian(ylim = c(Break[i*2+1],Break[i*2+2]))+
      theme(axis.text.x = element_blank(),
      axis.title = element_blank(), axis.ticks.x = element_blank(),
      legend.position =   'none',
      panel.grid = element_blank(),
      panel.border = element_blank(), axis.line.y = element_line(colour = "black"))
      P_list = append(P_list, list(p_tmp))
  }

  CMD = ""
  if(length(P_list)==1){
      CMD = "P_list[[1]]"
  }else{
      for(i in c(1:length(P_list))) {
      CMD = paste(CMD, "/P_list[[", length(P_list)-i+1, "]]",sep="")

    }
  }
  CMD = str_replace_all(CMD, "^/", "")


  p_end <- p +  coord_cartesian(ylim = c(Break[1], Break[2]))+ theme(title = element_blank(), plot.title = element_blank(), panel.grid = element_blank(),
    panel.border = element_blank(), axis.line = element_line(colour = "black"))
  CMD = paste(CMD, "/p_end + plot_layout(design = layout)",sep="")
  eval(parse(text=CMD))
}
