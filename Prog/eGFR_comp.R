egdat_Scr <- seq(0.5,3.0,0.1)
egdat_age <- seq(20,90,1)


#https://www.kidney.org/professionals/kdoqi/gfr_calculator

eGFR <- function(Scr,age,sex,race=0){
	eGFR = 194 * Scr^(-1.094) * age^(-0.287) * 	0.85^sex
	return(eGFR)
}

# CKD-EPI Creatinine Equation (2009)

eGFR_2009 <- function(Scr,age,sex,race){
	
	if(sex){
		kappa <- 0.7
		alpha <- -0.329
		} else{
			kappa <- 0.9
			alpha <- -0.411
			}
	
	GFR = 141 * 
	 min(Scr/kappa, 1)^alpha * 
	 max(Scr/kappa, 1)^(-1.209) * 
	 0.993^age *
	 1.018^sex *
	 1.159^race
	
	return(GFR)
}

return.mat <- function(x,y,fml,sex,race){
	if(quote(fml)=='eGFR'){race=0}
	mat <- matrix(rep(NA,length(x)*length(y)),ncol=length(y))
	for(i in 1:length(x)){
		for(j in 1:length(y)){
			mat[i,j] <- round( fml(x[i],y[j],sex,race) ,2)
		}
		
	}
	rownames(mat) <- x
	colnames(mat) <- y
	return(mat)
}

write.csv(
 round(return.mat(egdat_Scr,egdat_age,eGFR,1,0)/return.mat(egdat_Scr,egdat_age,eGFR_2009,1,0),2),
 file = sprintf('%s/%s.csv',output.path,'eGFR_eGFR2009_ratio_in_Female')
 )
write.csv(
 round(return.mat(egdat_Scr,egdat_age,eGFR,0,0)/return.mat(egdat_Scr,egdat_age,eGFR_2009,0,0),2),
 file = sprintf('%s/%s.csv',output.path,'eGFR_eGFR2009_ratio_in_Male')

)
