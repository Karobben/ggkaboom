Kaboom_bar <- function(Data = Data, Group = Group, Var="SD",
											 Pos = "dodge", BarW = .9, BarAl = .6, ErbW = .3,
										   Plate = "Set1"){
	List <- unique(Data[[Group]])
	TB = data.frame()

	for(i in List){
		TMP = Data[Data[Group]==i,]
		TMP = TMP[-which(colnames(TMP)==Group)]
		Mean = apply(TMP, 2, mean, na.rm = TRUE)
		SD = apply(TMP, 2, sd, na.rm = TRUE)
		SEM = SD/sqrt(nrow(TMP))
		tmp = data.frame(Species=i, Mean=Mean, Sd=SD, Sem=SEM)
		tmp$Variable = row.names(tmp)
		TB = rbind(TB, tmp)
	}

	P <- ggplot() + geom_bar( data= TB, aes(x= Species, y = Mean, fill=Variable),
	stat='identity', position = Pos, alpha=BarAl, width= BarW)+
	theme_bw()+ scale_fill_brewer(palette = Plate)

	if (Var == "SD"){
		P <- P +		geom_errorbar( data= TB, aes(x= Species, y = Mean,
					ymax= Mean+Sd, ymin=Mean-Sd, fill=Variable), position = position_dodge(.9), width = ErbW)
	}

	if (Var == "SEM"){
		P <- P +		geom_errorbar( data= TB, aes(x= Species, y = Mean,
					ymax= Mean+Sem, ymin=Mean-Sem, fill=Variable), position = position_dodge(.9), width = ErbW)

	}

	return(P)
}

Kaboom_test <- function(){
	print("test")
}
