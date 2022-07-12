P2S <- function(P){
    S = rep("ns", length(P))
    S[P <= 0.05]= "*"
    S[P <= 0.01]= "**"
    S[P <= 0.001]= "***"
    S[P <= 0.0001]= "****"
    return(S)
}

TB_summary <- function(data, x, P_test=FALSE, Show_ns=FALSE){
    List <- unique(data[[x]])
    TB = data.frame()
    for(i in List){
        TMP = data[data[x]==i,]
        TMP = TMP[-which(colnames(TMP)==x)]
        Mean = apply(TMP, 2, mean, na.rm = TRUE)
        SD = apply(TMP, 2, sd, na.rm = TRUE)
        SEM = SD/sqrt(nrow(TMP))
        N = nrow(TMP)
        tmp = data.frame(x=i, Mean=Mean, Sd=SD, Sem=SEM, N=N)
        tmp$Variable = row.names(tmp)
        colnames(tmp)[1] = x
        TB = rbind(TB, tmp)
    }
    if(length(List)==2){
        if(P_test=='ttest'){
            P = t.test(data[-which(colnames(data)==x)][data[[x]]==List[1],],
            data[-which(colnames(data)==x)][data[[x]]==List[2],])$p.value
        }
        if(P_test=="wilcox"){
            P = wilcox.test(data[-which(colnames(data)==x)][data[[x]]==List[1],],
            data[-which(colnames(data)==x)][data[[x]]==List[2],])$p.value
        }
        TB$pval = P
        TB$Stars = P2S(P)
        TB$Stars[1] = ""
    }
    if(P_test=="DunTest"){
        if(sum(data[[-which(colnames(data)==x)]])!= 0){
            Dun_TB <- DunnettTest(x=data[[-which(colnames(data)==x)]], g=data[[x]])
            TB$pval = c("",as.data.frame(Dun_TB[[1]])$pval)
            TB$Stars = c("",P2S(as.data.frame(Dun_TB[[1]])$pval))
        }else{
            TB$pval = ""
            TB$Stars = ""
        }
    }
    if(P_test!=FALSE){
        TB$Stars[TB$Stars=='ns'] = ""
    }
    return(TB)
}


TB_anova <- function(data, x, P_test=FALSE, Show_ns=FALSE){
    List <- unique(data[[x]])
    TB = data.frame()
    for(i in List){
        TMP = data[data[x]==i,]
        TMP = TMP[-which(colnames(TMP)==x)]
        Mean = apply(TMP, 2, mean, na.rm = TRUE)
        SD = apply(TMP, 2, sd, na.rm = TRUE)
        SEM = SD/sqrt(nrow(TMP))
        N = nrow(TMP)
        tmp = data.frame(x=i, Mean=Mean, Sd=SD, Sem=SEM, N=N)
        tmp$Variable = row.names(tmp)
        colnames(tmp)[1] = x
        TB = rbind(TB, tmp)
    }
    if(length(List)==2){
        if(P_test=='ttest'){
            P = t.test(data[-which(colnames(data)==x)][data[[x]]==List[1],],
            data[-which(colnames(data)==x)][data[[x]]==List[2],])$p.value
        }
        if(P_test=="wilcox"){
            P = wilcox.test(data[-which(colnames(data)==x)][data[[x]]==List[1],],
            data[-which(colnames(data)==x)][data[[x]]==List[2],])$p.value
        }
        TB$pval = P
        TB$Stars = P2S(P)
        TB$Stars[1] = ""
    }
    if(P_test=="DunTest"){
        if(sum(data[[-which(colnames(data)==x)]])!= 0){
            Dun_TB <- DunnettTest(x=data[[-which(colnames(data)==x)]], g=data[[x]])
            TB$pval = c("",as.data.frame(Dun_TB[[1]])$pval)
            TB$Stars = c("",P2S(as.data.frame(Dun_TB[[1]])$pval))
        }else{
            TB$pval = ""
            TB$Stars = ""
        }
    }
    if(P_test!=FALSE){
        TB$Stars[TB$Stars=='ns'] = ""
    }
    return(TB)
}


Kaboom_bar <- function(data, x,
        Col= FALSE, Var="SD", fill = FALSE,
        Pos = "dodge", BarW = .9, BarAl = .6, ErbW = .3,
        Plate = "Set1",
        Facet = "wrap", Facet_row = FALSE, scales = "fixed",
        space="fixed",
        Vari_level= FALSE, Frow_level = FALSE,
        Show_N_Group = TRUE, Show_N_x = FALSE, Show_ns = FALSE,
        P_test = FALSE
        ){
    if(Col==FALSE){
    	TB = TB_summary(data, x)
    }else{
        List <- unique(data[[x]])
        TB = data.frame()
        for(i in List){
            TMP = data[data[x]==i,]
            TMP = TMP[-which(colnames(TMP)==x)]
            TMP = TB_summary(TMP, Col, P_test= P_test)
            TMP[x]= i
            TB = rbind(TB, TMP)
        }
        }
    print(head(TB))


	# Factor inheritance
	if( class(data[[x]]) == "factor"){
		TB[[x]] = factor(TB[[x]], levels= levels(data[[x]]))
	}
	if(Col!=FALSE){
		if( class(data[[Col]]) == "factor"){
			TB[[Col]] = factor(TB[[Col]], levels= levels(data[[Col]]))
		}
	}
	if( Vari_level != FALSE){
		TB$Variable = factor(TB$Variable, levels= Vari_level)
	}
	if( Frow_level != FALSE){
		TB[[Facet_row]] = factor(TB[[Facet_row]], levels= Frow_level)
	}

    # If show N By x
    if(Show_N_x == TRUE){
        TB[x]<- factor(paste(TB[[x]], TB[['N']], sep="\nn="), levels = unique(paste(TB[[x]], TB[['N']], sep="\nn=")))
    }
    # If show N By group
    if(Show_N_Group == TRUE){
        TB[Col]<- factor(paste(TB[[Col]], TB[['N']], sep="\nn="), levels = unique(paste(TB[[Col]], TB[['N']], sep="\nn=")))
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
		P <- P +  geom_errorbar(aes( ymax= Mean+Sd, ymin=Mean-Sd), position = position_dodge(.9), width = ErbW)
	}

	if (Var == "SEM"){
		P <- P +  geom_errorbar(aes(ymax= Mean+Sem, ymin=Mean-Sem), position = position_dodge(.9), width = ErbW)

	}
	P <- P + labs(x=x)
    if(P_test!=FALSE){
        if(Show_ns==FALSE){
            VJUST = 0.4
        }else{
            VJUST = - 0.2
        }
        if(Var == "SEM"){
            P <- P + geom_text(aes(y = Mean+Sem, label=.data[['Stars']]), position = position_dodge(.9), vjust= VJUST)
        }else if(Var == "SD"){
            P <- P + geom_text(aes(y = Mean+Sd, label=.data[['Stars']]), position = position_dodge(.9), vjust= VJUST)

        }else{
            P <- P + geom_text(aes(y = Mean, label=.data[['Stars']]), position = position_dodge(.9), vjust= VJUST)
        }

    }
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
