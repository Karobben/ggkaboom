
Kaboom_tb_g <- function(TB, Var1= "X", Var2 ="Y", T_font=14, T_fill= "#0f0f0f", T_color= "white" ,T_grid = 'grey', fill=FALSE){
    TB <- melt(as.matrix(TB))
    colnames(TB)[1:2] <- c(Var2, Var1)
    P <- ggplot(TB, aes(TB[[Var1]], TB[[Var2]])) +
    facet_grid(TB[[Var2]]~TB[[Var1]], scales = 'free', switch='y') +
    theme(panel.background = element_blank(),
        panel.border = element_rect(color = T_grid, fill=NA),
        strip.background = element_rect(fill=T_fill),
        strip.text = element_text(color= T_color),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + geom_text(aes(label=value)) +
    theme(panel.spacing = unit(0.01, 'lines'), strip.text.y.left = element_text(size = T_font, angle = 0),  strip.text.x = element_text(size = T_font) ) +
    geom_text(aes(label=value))+
    labs(x=Var2, y = Var1)

    if(fill!=FALSE){
        P <- P + geom_point(aes(color=value),size=1000)
    }
    return(P)
}
