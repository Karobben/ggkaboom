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
        if(P_test!=FALSE){
            if(P_test=='ttest'){
                P = t.test(data[-which(colnames(data)==x)][data[[x]]==List[1],],
                data[-which(colnames(data)==x)][data[[x]]==List[2],])$p.value
                TB$pval = P
                TB$Stars = P2S(P)
                TB$Stars[1] = ""
                if(Show_ns==FALSE){
                    TB$Stars[TB$Stars=='ns'] = ""
                }
                print("\n\n\n\n\ngood\n\n\n\n\n")
                print(P)
            }
            if(P_test=="wilcox"){
                P = wilcox.test(data[-which(colnames(data)==x)][data[[x]]==List[1],],
                data[-which(colnames(data)==x)][data[[x]]==List[2],])$p.value
                TB$pval = P
                TB$Stars = P2S(P)
                TB$Stars[1] = ""
                if(Show_ns==FALSE){
                    TB$Stars[TB$Stars=='ns'] = ""
                }

            }
        }
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
        if(Show_ns==FALSE){
            TB$Stars[TB$Stars=='ns'] = ""
        }
    }
    return(TB)
}

TB_anova <- function(data, x, Show_ns=FALSE){
    print(head(data))
    Tuk_TB <- data.frame(TukeyHSD(aov( data[[which(colnames(data)!=x)]] ~data[[x]], data=data))[[1]])
    Tuk_TB$Stars <- P2S(Tuk_TB$p.adj)
    if(Show_ns==FALSE){
        Tuk_TB$Stars[Tuk_TB$Stars=='ns'] = ""
    }
    Tuk_TB <- cbind(Tuk_TB, data.frame(str_split_fixed(row.names(Tuk_TB), "-", 2)))
    return(Tuk_TB)
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
        Ava_TB = data.frame()
        for(i in List){
            TMP = data[data[x]==i,]
            TMP = TMP[-which(colnames(TMP)==x)]
            if(P_test=="Tukey"){
                tmp <- TB_anova(TMP, Col)
                tmp[x]=i
                Ava_TB <- rbind(Ava_TB, tmp)
            }
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
        P <- ggplot() + geom_bar(data= TB, aes(x= TB[[x]], y = Mean, fill=TB[[fill]]), stat='identity', position = Pos, alpha=BarAl, width= BarW)+
            theme_bw()
    }else{
        P <-  ggplot() + geom_bar(data= TB, aes(x= TB[[x]], y = Mean, fill=Variable), stat='identity', position = Pos, alpha=BarAl, width= BarW)+
            theme_bw()
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
    # Add error bars
    if (Var == "SD"){
        P <- P +  geom_errorbar(data= TB, aes(x= .data[[x]], y = Mean, fill=.data[[fill]], ymax= Mean+Sd, ymin=Mean-Sd), position = position_dodge(.9), width = ErbW)
    }

    if (Var == "SEM"){
        P <- P +  geom_errorbar(data= TB, aes(x= TB[[x]], y = Mean, fill=TB[[fill]], ymax= Mean+Sem, ymin=Mean-Sem), position = position_dodge(.9), width = ErbW)

    }
    P <- P + labs(x=x)

    if(P_test=="Tukey"){
        Ava_TB <- Ava_TB[!Ava_TB$Stars =="",]
        Ra_ = BarW/length(unique(TB[[fill]]))
        if( class(data[[x]]) == "factor"){
            Ava_TB[[x]] <- factor(Ava_TB[[x]], levels = levels(data[[x]]))
            X_tmp = Ava_TB[[x]]
            if (is.na(sum(as.numeric(as.character(X_tmp))))){
                X_tmp = as.numeric(X_tmp)
            }else{
                X_tmp = as.numeric(as.character(X_tmp))
            }
        }else{
                X_tmp = as.numeric(as.character(Ava_TB[[x]]))
            }
        if (is.na(sum(as.numeric(as.character(Ava_TB$X1))))){
            Ava_TB$X1 <- factor(Ava_TB$X1, levels = levels(data[[Col]]))
            Ava_TB$X2 <- factor(Ava_TB$X2, levels = levels(data[[Col]]))
            Ava_TB['x1'] = as.numeric(Ava_TB$X1)*Ra_ + X_tmp-((BarW+Ra_)/2)
            Ava_TB['x2'] = as.numeric(Ava_TB$X2)*Ra_ + X_tmp-((BarW+Ra_)/2)
        }else{
            Ava_TB['x1'] = as.numeric(as.character(Ava_TB$X1))*Ra_ + X_tmp-((BarW+Ra_)/2)
            Ava_TB['x2'] = as.numeric(as.character(Ava_TB$X2))*Ra_ + X_tmp-((BarW+Ra_)/2)
        }
        Ava_TB$y = 0
        if(Var =="SD"){
            TB_l = c("Mean", "Sd")
        }else if(Var =="SEM"){
            TB_l = c("Mean", "Sem")
        }else{
            TB_l = c("Mean")
        }
        V_just = max(apply(TB[TB_l], 1, sum)) *.02
        for(id in unique(Ava_TB[[x]])){
            Ava_TB$y[Ava_TB[[x]]==id] = V_just + max(apply(as.data.frame(TB[TB_l][TB[x]==id,]), 1, sum))
            if(nrow(Ava_TB[Ava_TB[[x]]==id,])>1){
                for(i in c(2:nrow(Ava_TB[Ava_TB[[x]]==id,]))){
                    Ava_TB$y[Ava_TB[[x]]==id][i] = Ava_TB$y[Ava_TB[[x]]==id][i-1]+ V_just
                }
            }
        }

        P <- P + geom_segment(data = Ava_TB, aes(x=x1, xend=x2, y = y, yend=y)) +
          geom_text(data=Ava_TB, aes(x=x1,y=y, label = Stars))

    }else{
        if(P_test!=FALSE){
            if(Show_ns==FALSE){
                VJUST = 0.4
            }else{
                VJUST = - 0.2
            }
            if(Var == "SEM"){
                P <- P + geom_text(data= TB, aes(x= TB[[x]], y = Mean+Sem, fill= .data[[fill]], label=.data[['Stars']]), position = position_dodge(.9), vjust= VJUST)
            }else if(Var == "SD"){
                P <- P + geom_text(data= TB, aes(x= TB[[x]], y = Mean+Sd, fill= .data[[fill]], label=.data[['Stars']]), position = position_dodge(.9), vjust= VJUST)

            }else{
                P <- P + geom_text(data= TB, aes(x= TB[[x]], y = Mean, fill= .data[[fill]], label=.data[['Stars']]), position = position_dodge(.9), vjust= VJUST)
            }

        }
        P + theme(strip.background = element_rect(fill = 'white'))
    }

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
	return(list(P,TB, Ava_TB))
}

Kaboom_test <- function(){
    print("test")
}
