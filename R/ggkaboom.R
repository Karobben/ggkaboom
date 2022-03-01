TB_summary <- function(data, x){
	List <- unique(data[[x]])
	TB = data.frame()
	for(i in List){
		TMP = data[data[x]==i,]
		TMP = TMP[-which(colnames(TMP)==x)]
		Mean = apply(TMP, 2, mean, na.rm = TRUE)
		SD = apply(TMP, 2, sd, na.rm = TRUE)
		SEM = SD/sqrt(nrow(TMP))
		tmp = data.frame(x=i, Mean=Mean, Sd=SD, Sem=SEM)
		tmp$Variable = row.names(tmp)
		colnames(tmp)[1] = x
		TB = rbind(TB, tmp)
	}
	return(TB)
}

Kaboom_bar <- function(data, x,
		 Col= FALSE, Var="SD", fill = FALSE,
		 Pos = "dodge", BarW = .9, BarAl = .6, ErbW = .3,
	   Plate = "Set1",
	   Facet = "wrap", Facet_row = FALSE, scales = "fixed", space="fixed",
	   Vari_level= FALSE, Frow_level = FALSE){

	if(Col==FALSE){
		TB = TB_summary(data, x)
	}else{
		List <- unique(data[[x]])
		TB = data.frame()
		for(i in List){
			TMP = data[data[x]==i,]
			TMP = TMP[-which(colnames(TMP)==x)]
			TMP = TB_summary(TMP, Col)
			TMP[x]= i
			TB = rbind(TB, TMP)
		}
	}
	print(head(TB))


	# Factor inheritance
	if( class(data[[x]]) == "factor"){
		TB[[x]] = factor(TB[[x]], levels= levels(data[[x]]))
	}
	if( class(data[[Col]]) == "factor"){
		TB[[Col]] = factor(TB[[Col]], levels= levels(data[[Col]]))
	}
	if( Vari_level != "FALSE"){
		TB$Variable = factor(TB$Variable, levels= Vari_level)
	}
	if( Frow_level != "FALSE"){
		TB[[Facet_row]] = factor(TB[[Facet_row]], levels= Frow_level)
	}

	# fill coclor
	if(fill!=FALSE){
		P <- ggplot(data= TB, aes(x= .data[[x]], y = Mean, fill=.data[[fill]]))
	}else{
		P <- ggplot(data= TB, aes(x= .data[[x]], y = Mean, fill=Variable))
	}
	#print(class(TB[[P$labels$fill]]))
	#print("here")
	if(class(TB[[P$labels$fill]])!='numeric'){
		Fac_N = length(unique(TB[[P$labels$fill]]))
		#print(Fac_N)
		Max_c_n = brewer.pal.info[Plate,'maxcolors']
		print(round(Fac_N/Max_c_n)+1)

		P <- P + scale_fill_manual( values =  rep(brewer.pal(n = Max_c_n,name =Plate),round(Fac_N/Max_c_n)+1))
	}else{
		colorRampPalette(rev(brewer.pal(n = 11,name = "RdYlBu"))) -> cc
		P <- P + scale_fill_gradientn(colors=cc(20))
	}

	#TB[[Col]] = factor(TB[[Col]])
	P <- P + geom_bar( stat='identity', position = Pos, alpha=BarAl, width= BarW)+
	theme_bw()

	if (Var == "SD"){
		P <- P +		geom_errorbar(aes( ymax= Mean+Sd, ymin=Mean-Sd), position = position_dodge(.9), width = ErbW)
	}

	if (Var == "SEM"){
		P <- P +		geom_errorbar(aes(ymax= Mean+Sem, ymin=Mean-Sem), position = position_dodge(.9), width = ErbW)

	}
	P <- P + labs(x=x)

	P + theme(strip.background = element_rect(fill = 'white'))

	# Facte
	print(Facet_row)
	print(Col)
	if(Col!=FALSE){
		if(Facet!=FALSE){
			if(Facet=="wrap"){
				if (Facet_row!=FALSE){
					P <- P + facet_wrap( TB[[Facet_row]] ~ .data[[Col]] , scales=scales)
				}else{
					P <- P + facet_wrap(.data[[Col]]~., scales=scales)
			}
		}
			if(Facet=="grid"){
				if (Facet_row!=FALSE){
					P <- P + facet_grid( TB[[Facet_row]] ~ .data[[Col]] , scales=scales, space = space)
				}else{
					P <- P + facet_grid(.data[[Col]]~., scales=scales, space= space)
			}
		}
		}

	}
	return( P)
}

Kaboom_test <- function(){
	print("test")
}
