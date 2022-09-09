library(patchwork)
library(stringr)

Kaboom_break <-  function(p,  Break = c(0, 10, 40, 100),  R=c(1,1),
    panel.grid = element_line(colour = 'grey'),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.num = 10,
    panel.grid.scale = F,
    legend.position = 'bottom',
    legend.direction = 'vertical'
    ){

    R = rev.default(R)
    Lay = c()
    for(i in c(1:length(R))){
        Lay = c(Lay, rep(letters[i], R[length(R)-i+1]))
    }
  layout=""
  for( i in Lay){
    layout=paste(layout,i,sep="\n")
  }
  P_list = list()
  Num = 0
  for(i in c(1:round((length(Break)/2)-1))){
      Num = Num +1
      p_tmp <- p +  coord_cartesian(ylim = c(Break[i*2+1],Break[i*2+2]))+
      theme(axis.text.x = element_blank(),
      axis.title = element_blank(), axis.ticks.x = element_blank(),
      legend.position =   'none',
      panel.grid = panel.grid,
      panel.background = panel.background,
      panel.border = panel.border,
      axis.line.y = element_line(colour = "black"))

      if (panel.grid.scale=="len"){
        p_tmp <- p_tmp + scale_y_continuous(
          breaks = seq(Break[i*2+1],Break[i*2+2],
              len =1 + round(R[-1][Num] * panel.grid.num/sum(R))))
      }

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


  p_end <- p +  coord_cartesian(ylim = c(Break[1], Break[2]))+
    theme(title = element_blank(), plot.title = element_blank(),
          panel.grid = panel.grid, panel.background = panel.background,
          panel.border = panel.border, legend.position =   'none',
          axis.line = element_line(colour = "black"))
  if (panel.grid.scale=="len"){
    p_end <- p_end + scale_y_continuous(
        breaks = seq(Break[1],Break[2],
        len =1 + round(R[1] * panel.grid.num/sum(R))))
  }
  CMD = paste(CMD, "/p_end + plot_layout(design = layout, guides = 'collect')",sep="")
  P <- eval(parse(text=CMD))
  P <- P + theme( legend.position = legend.position,
      legend.direction = legend.direction
    )
  return(P)
}
