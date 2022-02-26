TB_summary <- function(Data, Group){
	List <- unique(Data[[Group]])
	TB = data.frame()
	for(i in List){
		TMP = Data[Data[Group]==i,]
		TMP = TMP[-which(colnames(TMP)==Group)]
		Mean = apply(TMP, 2, mean, na.rm = TRUE)
		SD = apply(TMP, 2, sd, na.rm = TRUE)
		SEM = SD/sqrt(nrow(TMP))
		tmp = data.frame(Group=i, Mean=Mean, Sd=SD, Sem=SEM)
		tmp$Variable = row.names(tmp)
		colnames(tmp)[1] = Group
		TB = rbind(TB, tmp)
	}
	return(TB)
}

Kaboom_bar <- function(Data = Data, Group = Group,
											 Group2= FALSE, Var="SD",
											 Pos = "dodge", BarW = .9, BarAl = .6, ErbW = .3,
										   Plate = "Set1"){

	if(Group2==FALSE){
		TB = TB_summary(Data, Group)
	}else{
		List <- unique(Data[[Group]])
		TB = data.frame()
		for(i in List){
			TMP = Data[Data[Group]==i,]
			TMP = TMP[-which(colnames(TMP)==Group)]
			TMP = TB_summary(TMP, Group2)
			TMP[Group]= i
			TB = rbind(TB, TMP)
		}
	}
	print(head(TB))

	if(Group2!=FALSE){
		P <- ggplot(data= TB, aes(x= .data[[Group]], y = Mean, fill=.data[[Group2]]))
	}else{
		P <- ggplot(data= TB, aes(x= .data[[Group]], y = Mean, fill=Variable))
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

	#TB[[Group2]] = factor(TB[[Group2]])
	P <- P + geom_bar( stat='identity', position = Pos, alpha=BarAl, width= BarW)+
	theme_bw()

	if (Var == "SD"){
		P <- P +		geom_errorbar(aes( ymax= Mean+Sd, ymin=Mean-Sd), position = position_dodge(.9), width = ErbW)
	}

	if (Var == "SEM"){
		P <- P +		geom_errorbar(aes(ymax= Mean+Sem, ymin=Mean-Sem), position = position_dodge(.9), width = ErbW)

	}
	P <- P + labs(x=Group)
	return( P)
}

Kaboom_test <- function(){
	print("test")
}
