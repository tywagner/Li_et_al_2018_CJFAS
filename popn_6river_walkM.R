## Citation ##
# Li, Y., T. Wagner, Y. Jiao, R. Lorants, and C.A Murphy. in press. Evaluating spatial and temporal variability 
# in growth and mortality for recreational fisheries with limited catch data. Canadian Journal of Fisheries 
# and Aquatic Sciences.


# library(rjags)
library(R2jags)
# library(jagsUI)

n.river=6
n.size=7
####read in data
river.para=as.matrix(read.table('river_specific_para_input.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:15)))

##cpue data 1990-2013
dd.cpue.1=read.table('smb_cpue_river_1.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:6)) #replace 0 with NA to avoid JAGS error-node inconsis ent with paraents
dd.cpue.2=read.table('smb_cpue_river_2.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:6))
dd.cpue.3=read.table('smb_cpue_river_3.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:6))
dd.cpue.4=read.table('smb_cpue_river_4.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:6))
dd.cpue.5=read.table('smb_cpue_river_5.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:6))
dd.cpue.6=read.table('smb_cpue_river_6.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:6))
dd.cpue=cbind(dd.cpue.1[,1:n.size],dd.cpue.2[,1:n.size],dd.cpue.3[,1:n.size],dd.cpue.4[,1:n.size],dd.cpue.5[,1:n.size],dd.cpue.6)
name.cpue=rep(NA,n.river*n.size)
for(r in 1:n.river){
	for(i in 1:n.size){
		name.cpue[(r-1)*n.size+i]=paste('r',r,'_s',i,sep='')
	}	
}
colnames(dd.cpue)=c(name.cpue,'yr')

##yoy data 1987-2010,use yoy data 1991-2010 because set 1990 as initial, age 0 as recurit
dd.yoy=read.table('yoy_cpe_year_river.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:7))
dd.yoy=dd.yoy[5:nrow(dd.yoy),]

###fixed life history and popn parameters
sim.yr=1990:2013 #set 1990 as initial
l.low=c(25,75,125,175,225,300,375)
l.up=c(75,125,175,225,300,375,550)
l.mid=(l.low+l.up)/2
N.ini.size.dist=river.para[,c('s1','s2','s3','s4','s5','s6','s7')] #set the size dist based on catch in CPE data in 1990
p.rec=river.para[,c('r1','r2','r3','r4','r5','r6','r7')] #recruit size distribution based on length-age data for age 0
n.prior.lo=river.para[,'n1'] #x103
n.prior.up=river.para[,'n2'] #x103
avg.rec.lo=river.para[,'ar1'] #x103
avg.rec.up=river.para[,'ar2'] #x103

n.cpue=nrow(dd.cpue)
n.yoy=nrow(dd.yoy)

########para name to track
nin.name=rep(NA,n.river)
for(r in 1:n.river){
	nin.name[r]=paste('Nini[',r,']',sep='')
}

Rybar.name=rep(NA,n.river)
for(r in 1:n.river){
	Rybar.name[r]=paste('Rybar[',r,']',sep='')
}

sigmaCPUE.name=rep(NA,n.river)
for(r in 1:n.river){
	sigmaCPUE.name[r]=paste('sigmaCPUE[',r,']',sep='')
}

sigmaYOY.name=rep(NA,n.river)
for(r in 1:n.river){
	sigmaYOY.name[r]=paste('sigmaYOY[',r,']',sep='')
}

qYOY.name=rep(NA,n.river)
for(r in 1:n.river){
	qYOY.name[r]=paste('qYOY[',r,']',sep='')
}

qJUV.name=rep(NA,n.river)
for(r in 1:n.river){
	qJUV.name[r]=paste('q[',r,',',4,']',sep='')
}

qYOUNG.name=rep(NA,n.river)
for(r in 1:n.river){
	qYOUNG.name[r]=paste('q[',r,',',5,']',sep='')
}

qADU.name=rep(NA,n.river)
for(r in 1:n.river){
	qADU.name[r]=paste('qADU[',r,']',sep='')
}

qr.name=rep(NA,n.river)
for(r in 1:n.river){
	qr.name[r]=paste('qr[',r,']',sep='')
}

mYOY.name=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		mYOY.name[(r-1)*(length(sim.yr)-1)+y]=paste('mYOY[',y,',',r,']',sep='') #yoy mortality 1990-2012
	}
}

mJUV.name=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		mJUV.name[(r-1)*(length(sim.yr)-1)+y]=paste('mJUV[',y,',',r,']',sep='') #yoy mortality 1990-2012
	}
}

mYOUNG.name=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		mYOUNG.name[(r-1)*(length(sim.yr)-1)+y]=paste('mYOUNG[',y,',',r,']',sep='') #young adult mortality 1990-2012
	}
}

zADU.name=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		zADU.name[(r-1)*(length(sim.yr)-1)+y]=paste('zADU[',y,',',r,']',sep='') #adult mortality 1990-2012
	}
}

sigmaMyoyYr.name=rep(NA,n.river)
for(r in 1:n.river){
	sigmaMyoyYr.name[r]=paste('sigmaMyoyYr[',r,']',sep='')
}

sigmaMjuvYr.name=rep(NA,n.river)
for(r in 1:n.river){
	sigmaMjuvYr.name[r]=paste('sigmaMjuvYr[',r,']',sep='')
}

sigmaMyoungYr.name=rep(NA,n.river)
for(r in 1:n.river){
	sigmaMyoungYr.name[r]=paste('sigmaMyoungYr[',r,']',sep='')
}

sigmaZaduYr.name=rep(NA,n.river)
for(r in 1:n.river){
	sigmaZaduYr.name[r]=paste('sigmaZaduYr[',r,']',sep='')
}

sigmaN.name=rep(NA,n.river)
for(r in 1:n.river){
    sigmaN.name[r]=paste('sigmaN[',r,']',sep='')
}

sigmaR.name=rep(NA,n.river)
for(r in 1:n.river){
    sigmaR.name[r]=paste('sigmaR[',r,']',sep='')
}

muCPUE.name=rep(NA,n.river*n.size*n.cpue)
for(r in 1:(n.river*n.size)){
	for(y in 1:n.cpue){
		muCPUE.name[(r-1)*n.cpue+y]=paste('muCPUE[',y,',',r,']',sep='')
	}
}

muYOY.name=rep(NA,n.river*n.yoy)
for(r in 1:n.river){
	for(y in 1:n.yoy){
		muYOY.name[(r-1)*n.yoy+y]=paste('muYOY[',y,',',r,']',sep='')
	}
}

Ry.name=rep(NA,n.river*(n.cpue-1))
for(r in 1:n.river){
	for(y in 1:(n.cpue-1)){
		Ry.name[(r-1)*(n.cpue-1)+y]=paste('Ry[',y+1,',',r,']',sep='')
	}
}

#####calculate growth transition using par estimates from VB model
vb=read.table('estimate_par_VB_6river_walkG.txt',header=T,sep='\t',row.names=NULL)
extreme=apply(as.matrix(vb[,which(colnames(vb)==paste('Linf',1,'_',sim.yr[1],sep='')):which(colnames(vb)==paste('Linf',n.river,'_',sim.yr[length(sim.yr)-1],sep=''))]),2,function(x){which(x>5000)}) #delete para set containing extreme value
extreme=unique(unlist(extreme))
if(length(extreme)>0){
	vb=vb[-extreme,]
}

extreme=apply(as.matrix(vb[,which(colnames(vb)==paste('K',1,'_',sim.yr[1],sep='')):which(colnames(vb)==paste('K',n.river,'_',sim.yr[length(sim.yr)-1],sep=''))]),2,function(x){which(x>1)}) #delete para set containing extreme value
extreme=unique(unlist(extreme))
if(length(extreme)>0){
	vb=vb[-extreme,]
}

trans.1=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 1
colnames(trans.1)=name.cpue
trans.2=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 2
colnames(trans.2)=name.cpue
trans.3=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 3
colnames(trans.3)=name.cpue
trans.4=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 4
colnames(trans.4)=name.cpue
trans.5=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 5
colnames(trans.5)=name.cpue
trans.6=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 6
colnames(trans.6)=name.cpue
trans.7=matrix(NA,nrow=length(sim.yr)-1,ncol=n.size*n.river) #prob grow to size 7
colnames(trans.7)=name.cpue

for(y in 1:(length(sim.yr)-1)){
	trans=rep(NA,n.size) #initialize trans matrix, delete it later
	for(r in 1:n.river){
		dl=matrix(NA,nrow=nrow(vb),ncol=n.size) #each col is dl dist for size j (col j)
		for(j in 1:n.size){
			dl[,j]=(vb[,paste('Linf',r,'_',sim.yr[y],sep='')]-l.mid[j])*(1-exp(-vb[,paste('K',r,'_',sim.yr[y],sep='')]))
		}

		trans.tem=matrix(0,nrow=n.size,ncol=n.size) #each col is prob of size j (col j) growing to the rest of sizes i (row i)
		#approch 1: using Monte Carlo
		for(j in 1:n.size){
			for(i in j:n.size){
				trans.tem[i,j]=round(length(which((dl[,j]+l.mid[j])>l.low[i] & (dl[,j]+l.mid[j])<l.up[i]))/nrow(dl),4) #no need to standardize using MC
			}
		}
		trans=cbind(trans,trans.tem)
	}
	trans=trans[,-1] #delete the 1st initalizing colomn
	trans.1[y,]=trans[1,]
	trans.2[y,]=trans[2,]
	trans.3[y,]=trans[3,]
	trans.4[y,]=trans[4,]
	trans.5[y,]=trans[5,]
	trans.6[y,]=trans[6,]
	trans.7[y,]=trans[7,]
}

######jags model
model=function(){
	#survival
	for(r in 1:n.river){
		for(y in 1:(n.cpue-2)){
			mYOY[y+1,r]~dlnorm(log(mYOY[y,r]),tau.mYOY.yr[r]) %_% T(0.01,2)
			mJUV[y+1,r]~dlnorm(log(mJUV[y,r]),tau.mJUV.yr[r]) %_% T(0.01,2)
			mYOUNG[y+1,r]~dlnorm(log(mYOUNG[y,r]),tau.mYOUNG.yr[r]) %_% T(0.01,2)
			zADU[y+1,r]~dlnorm(log(zADU[y,r]),tau.zADU.yr[r]) %_% T(0.01,3)		
		}
	}
	
	for(r in 1:n.river){
		tau.mYOY.yr[r]=pow(sigmaMyoyYr[r],-2)
		tau.mJUV.yr[r]=pow(sigmaMjuvYr[r],-2)
		tau.mYOUNG.yr[r]=pow(sigmaMyoungYr[r],-2)
		tau.zADU.yr[r]=pow(sigmaZaduYr[r],-2)
		sigmaMyoyYr[r]~dlnorm(muSigmaMyoyYr,tau.sigmaMyoyYr) %_% T(0.001,1)
		sigmaMjuvYr[r]~dlnorm(muSigmaMjuvYr,tau.sigmaMjuvYr) %_% T(0.001,1)
		sigmaMyoungYr[r]~dlnorm(muSigmaMyoungYr,tau.sigmaMyoungYr) %_% T(0.001,1)
		sigmaZaduYr[r]~dlnorm(muSigmaZaduYr,tau.sigmaZaduYr) %_% T(0.001,1)
	}
	tau.sigmaMyoyYr=pow(sigmaSigmaMyoyYr,-2)
	tau.sigmaMjuvYr=pow(sigmaSigmaMjuvYr,-2)
	tau.sigmaMyoungYr=pow(sigmaSigmaMyoungYr,-2)
	tau.sigmaZaduYr=pow(sigmaSigmaZaduYr,-2)
	
	for(r in 1:n.river){
		mYOY[1,r]~dlnorm(log(mYOYbar),tau.mYOY) %_% T(0.01,2)
		mJUV[1,r]~dlnorm(log(mJUVbar),tau.mJUV) %_% T(0.01,2)
		mYOUNG[1,r]~dlnorm(log(mYOUNGbar),tau.mYOUNG) %_% T(0.01,2)
		zADU[1,r]~dlnorm(log(zADUbar),tau.zADU) %_% T(0.01,3)
	}
	tau.mYOY=pow(sigmaMyoy,-2)
	tau.mJUV=pow(sigmaMjuv,-2)
	tau.mYOUNG=pow(sigmaMyoung,-2)
	tau.zADU=pow(sigmaZadu,-2)

	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			for(i in 1:3){
				z[y,(r-1)*n.size+i]=mYOY[y,r]
			}
		}
	}

	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			z[y,(r-1)*n.size+4]=mJUV[y,r]
			}
		}

	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			z[y,(r-1)*n.size+5]=mYOUNG[y,r]
			}
		}
		
	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			for(i in 6:n.size){
				z[y,(r-1)*n.size+i]=zADU[y,r]
			}
		}
	}
				
	for(y in 1:(n.cpue-1)){
		for(i in 1:(n.size*n.river)){
			surv[y,i]=exp(-z[y,i])
		}
	}
		
	#recruit, age-0 recuitment, so no one-year delay, ssn[y] gives recruit[y]
	for(r in 1:n.river){
		Ry[1,r]=0
	}
	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			Ry[y+1,r]~dlnorm(log(Rybar[r]*1000),tau.R[r])
		}
		tau.R[r]=pow(sigmaR[r],-2)
	}
	
	for(r in 1:n.river){
		sigmaR[r]~dlnorm(muSigmaR,tau.sigmaR) %_% T(0.001,10)
	}
	tau.sigmaR=pow(sigmaSigmaR,-2)

	for(r in 1:n.river){
		for(y in 1:n.cpue){
			R[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]=round(Ry[y,r]*p.rec[r,])
		}
	}
	
	#popn
	for(r in 1:n.river){
		N[1,((r-1)*n.size+1):((r-1)*n.size+n.size)]=round(Nini[r]*N.ini.size.dist[r,]*1000)
	}

	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			N[y+1,(r-1)*n.size+1]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.1[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+1])*exp(errorN[y+1,(r-1)*n.size+1]))
			N[y+1,(r-1)*n.size+2]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.2[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+2])*exp(errorN[y+1,(r-1)*n.size+2]))
			N[y+1,(r-1)*n.size+3]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.3[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+3])*exp(errorN[y+1,(r-1)*n.size+3]))
			N[y+1,(r-1)*n.size+4]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.4[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+4])*exp(errorN[y+1,(r-1)*n.size+4]))
			N[y+1,(r-1)*n.size+5]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.5[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+5])*exp(errorN[y+1,(r-1)*n.size+5]))
			N[y+1,(r-1)*n.size+6]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.6[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+6])*exp(errorN[y+1,(r-1)*n.size+6]))
			N[y+1,(r-1)*n.size+7]=round((sum(N[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*surv[y,((r-1)*n.size+1):((r-1)*n.size+n.size)]*trans.7[y,((r-1)*n.size+1):((r-1)*n.size+n.size)])+R[y+1,(r-1)*n.size+7])*exp(errorN[y+1,(r-1)*n.size+7]))
		}
	}

	for(r in 1:n.river){
		for(i in 1:n.size){
			errorN[1,(r-1)*n.size+i]=0
		}
	}
	
	for(r in 1:n.river){
		for(y in 1:(n.cpue-1)){
			for(i in 1:n.size){
				errorN[y+1,(r-1)*n.size+i]~dnorm(0,tau.N[r])
			}			
		}
		tau.N[r]=pow(sigmaN[r],-2)
	}

	for(r in 1:n.river){
		sigmaN[r]~dlnorm(muSigmaN,tau.sigmaN) %_% T(0.001,10)
	}
	tau.sigmaN=pow(sigmaSigmaN,-2)
		
	#cpue
	for(r in 1:n.river){
		for(i in 1:3){
			q[r,i]=qYOY[r]
		}
	}
	for(r in 1:n.river){
		for(i in 6:n.size){
			q[r,i]=qADU[r]
		}
	}
	for(r in 1:n.river){
		for(y in 1:n.cpue){
			for(i in 1:n.size){
				dd.cpue[y,(r-1)*n.size+i]~dlnorm(muCPUE[y,(r-1)*n.size+i],tau.cpue[r])
				muCPUE[y,(r-1)*n.size+i]=log(N[y,(r-1)*n.size+i]*q[r,i]*0.0001+0.00001) #add a small constant if needed to avoid error during intial:inconsisten node with parents
			}
		}
		tau.cpue[r]=pow(sigmaCPUE[r],-2)
	}
	
	#yoy,start in 1991, yoy[y]->Ry[y+1] because Ry start in 1990 Ry[1990]=0
	for(r in 1:n.river){
		for(y in 1:n.yoy){
			dd.yoy[y,r]~dlnorm(muYOY[y,r],tau.yoy[r])
			muYOY[y,r]=log(Ry[y+1,r]*qr[r]*0.0001+0.00001) #add a small constant if needed to avoid error during intial:inconsisten node with parents
		}
		tau.yoy[r]=pow(sigmaYOY[r],-2)
	}
		
	#priors
	mYOYbar~dunif(0.2,0.8)
	mJUVbar~dunif(0.2,0.8)
	mYOUNGbar~dunif(0.2,0.8)
	zADUbar~dunif(0.2,3)
	sigmaMyoy~dunif(0.001,1)
	sigmaMjuv~dunif(0.001,1)
	sigmaMyoung~dunif(0.001,1)
	sigmaZadu~dunif(0.001,1)
	
	muSigmaMyoyYr~dunif(0.001,1)
	muSigmaMjuvYr~dunif(0.001,1)
	muSigmaMyoungYr~dunif(0.001,1)
	muSigmaZaduYr~dunif(0.001,1)
	sigmaSigmaMyoyYr~dunif(0.001,1)
	sigmaSigmaMjuvYr~dunif(0.001,1)
	sigmaSigmaMyoungYr~dunif(0.001,1)
	sigmaSigmaZaduYr~dunif(0.001,1)
			
	for(r in 1:n.river){
		Rybar[r]~dunif(avg.rec.lo[r],avg.rec.up[r]) #x103
	}
	
	for(r in 1:n.river){
		Nini[r]~dunif(n.prior.lo[r],n.prior.up[r]) #x103
	}
	
	muSigmaN~dunif(0.001,10)
    muSigmaR~dunif(0.001,10)		
	sigmaSigmaN~dunif(0.001,10)
	sigmaSigmaR~dunif(0.001,10)

	for(r in 1:n.river){
		qYOY[r]~dunif(0.001,1) #x10-4
		q[r,4]~dunif(0.001,1) #x10-4
		q[r,5]~dunif(0.001,1) #x10-4
		qADU[r]~dunif(0.001,1) #x10-4
		sigmaCPUE[r]~dunif(0.001,10)
	}
	
	for(r in 1:n.river){
		qr[r]~dunif(0.001,1) #x10-4
		sigmaYOY[r]~dunif(0.001,10)	
	}
	
}

datafit=list('dd.cpue'=dd.cpue,'dd.yoy'=dd.yoy,'n.size'=n.size,'N.ini.size.dist'=N.ini.size.dist,'p.rec'=p.rec,'n.cpue'=n.cpue,'n.yoy'=n.yoy,'n.prior.lo'=n.prior.lo,'n.prior.up'=n.prior.up,'avg.rec.lo'=avg.rec.lo,'avg.rec.up'=avg.rec.up,'trans.1'=trans.1,'trans.2'=trans.2,'trans.3'=trans.3,'trans.4'=trans.4,'trans.5'=trans.5,'trans.6'=trans.6,'trans.7'=trans.7,'n.river'=n.river)
para=c('mYOYbar','mJUVbar','mYOUNGbar','zADUbar','sigmaMyoy','sigmaMjuv','sigmaMyoung','sigmaZadu','muSigmaR','muSigmaN','sigmaSigmaR','sigmaSigmaN','muSigmaMyoyYr','muSigmaMjuvYr','muSigmaMyoungYr','muSigmaZaduYr','sigmaSigmaMyoyYr','sigmaSigmaMjuvYr','sigmaSigmaMyoungYr','sigmaSigmaZaduYr',nin.name,Rybar.name,sigmaCPUE.name,sigmaYOY.name,sigmaMyoyYr.name,sigmaMjuvYr.name,sigmaMyoungYr.name,sigmaZaduYr.name,sigmaN.name,sigmaR.name,qYOY.name,qJUV.name,qYOUNG.name,qADU.name,qr.name,mYOY.name,mJUV.name,mYOUNG.name,zADU.name,muCPUE.name,muYOY.name,Ry.name)

# initial=function(){
# list('Linf'=runif(1,538,800),'k'=runif(1,0,1),'t0'=runif(1,-2,0),'sigma.age'=runif(1,0,10))
# }

# initial=list(list('Linf'=600,'k'=0.1,'t0'=-1.4,'sigma.age'=38),list('Linf'=630,'k'=0.2,'t0'=-1.5,'sigma.age'=40),list('Linf'=700,'k'=0.15,'t0'=-1.6,'sigma.age'=35))


fit=jags(data=datafit,inits=NULL,parameters.to.save=para,model.file=model,n.chains=3,n.iter=500000,n.burnin=470000,n.thin=10,DIC=T)

# #display output
# plot(fit)
# print(fit,intervals=c(0.025,0.5,0.975))
# traceplot(fit)

# #use plots in coda
# fit.mcmc=as.mcmc(fit)
# xyplot(fit.mcmc)
# densityplot(fit.mcmc)

###########extract para values after burn-in and thin
nin.name.2=rep(NA,n.river)
for(r in 1:n.river){
	nin.name.2[r]=paste('Nini',r,sep='')
}

Rybar.name.2=rep(NA,n.river)
for(r in 1:n.river){
	Rybar.name.2[r]=paste('Rybar',r,sep='')
}

sigmaCPUE.name.2=rep(NA,n.river)
for(r in 1:n.river){
	sigmaCPUE.name.2[r]=paste('sigmaCPUE',r,sep='')
}

sigmaYOY.name.2=rep(NA,n.river)
for(r in 1:n.river){
	sigmaYOY.name.2[r]=paste('sigmaYOY',r,sep='')
}

qYOY.name.2=rep(NA,n.river)
for(r in 1:n.river){
	qYOY.name.2[r]=paste('qYOY',r,sep='')
}

qJUV.name.2=rep(NA,n.river)
for(r in 1:n.river){
	qJUV.name.2[r]=paste('qJUV',r,sep='')
}

qYOUNG.name.2=rep(NA,n.river)
for(r in 1:n.river){
	qYOUNG.name.2[r]=paste('qYOUNG',r,sep='')
}

qADU.name.2=rep(NA,n.river)
for(r in 1:n.river){
	qADU.name.2[r]=paste('qADU',r,sep='')
}

qr.name.2=rep(NA,n.river)
for(r in 1:n.river){
	qr.name.2[r]=paste('qr',r,sep='')
}

mYOY.name.2=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		mYOY.name.2[(r-1)*(length(sim.yr)-1)+y]=paste('mYOY',r,'_',sim.yr[y],sep='') #yoy mortality 1990-2012
	}
}

mJUV.name.2=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		mJUV.name.2[(r-1)*(length(sim.yr)-1)+y]=paste('mJUV',r,'_',sim.yr[y],sep='') #yoy mortality 1990-2012
	}
}

mYOUNG.name.2=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		mYOUNG.name.2[(r-1)*(length(sim.yr)-1)+y]=paste('mYOUNG',r,'_',sim.yr[y],sep='') #young adult mortality 1990-2012
	}
}

zADU.name.2=rep(NA,(length(sim.yr)-1)*n.river)
for(r in 1:n.river){
	for(y in 1:(length(sim.yr)-1)){
		zADU.name.2[(r-1)*(length(sim.yr)-1)+y]=paste('zADU',r,'_',sim.yr[y],sep='') #adult mortality 1990-2012
	}
}

sigmaMyoyYr.name.2=rep(NA,n.river)
for(r in 1:n.river){
	sigmaMyoyYr.name.2[r]=paste('sigmaMyoyYr',r,sep='')
}

sigmaMjuvYr.name.2=rep(NA,n.river)
for(r in 1:n.river){
	sigmaMjuvYr.name.2[r]=paste('sigmaMjuvYr',r,sep='')
}

sigmaMyoungYr.name.2=rep(NA,n.river)
for(r in 1:n.river){
	sigmaMyoungYr.name.2[r]=paste('sigmaMyoungYr',r,sep='')
}

sigmaZaduYr.name.2=rep(NA,n.river)
for(r in 1:n.river){
	sigmaZaduYr.name.2[r]=paste('sigmaZaduYr',r,sep='')
}

sigmaN.name.2=rep(NA,n.river)
for(r in 1:n.river){
    sigmaN.name.2[r]=paste('sigmaN',r,sep='')
}

sigmaR.name.2=rep(NA,n.river)
for(r in 1:n.river){
    sigmaR.name.2[r]=paste('sigmaR',r,sep='')
}

muCPUE.name.2=rep(NA,n.river*n.size*n.cpue)
for(r in 1:n.river){
	for(i in 1:n.size){
		for(y in 1:n.cpue){
			muCPUE.name.2[((r-1)*n.size+i-1)*n.cpue+y]=paste('muCPUE',r,'s',i,'_',sim.yr[y],sep='')
		}
	}
}

muYOY.name.2=rep(NA,n.river*n.yoy)
for(r in 1:n.river){
	for(y in 1:n.yoy){
		muYOY.name.2[(r-1)*n.yoy+y]=paste('muYOY',r,'_',dd.yoy$yr[y],sep='')
	}
}

Ry.name.2=rep(NA,n.river*(n.cpue-1))
for(r in 1:n.river){
	for(y in 1:(n.cpue-1)){
		Ry.name.2[(r-1)*(n.cpue-1)+y]=paste('Ry',r,'_',sim.yr[y+1],sep='')
	}
}

output.array=fit$BUGSoutput$sims.array #[,3,1929]
output=rbind(output.array[,1,],output.array[,2,],output.array[,3,]) #convert to matrix; the matrix directly from fit output (fit$BUGSoutput$sims.matrix) does not sort by chain

output=output[,c(para,'deviance')] #re-organize the parameter order in the matrix
colnames(output)=c(para[1:20],nin.name.2,Rybar.name.2,sigmaCPUE.name.2,sigmaYOY.name.2,sigmaMyoyYr.name.2,sigmaMjuvYr.name.2,sigmaMyoungYr.name.2,sigmaZaduYr.name.2,sigmaN.name.2,sigmaR.name.2,qYOY.name.2,qJUV.name.2,qYOUNG.name.2,qADU.name.2,qr.name.2,mYOY.name.2,mJUV.name.2,mYOUNG.name.2,zADU.name.2,muCPUE.name.2,muYOY.name.2,Ry.name.2,'deviance')

write.table(output,'estimate_par_popn_6river_walkM.txt',row.names=F,sep='\t',quote=F) #switch between rivers

###############summary HPD values for para
# output=read.table('estimate_par_popn_6river_walkM.txt',header=T,row.names=NULL,sep='\t') #switch between rivers

####################HPD
para.est=matrix(NA,nrow=ncol(output)+2,ncol=3)
rownames(para.est)=c(colnames(output),'pD','DIC')
colnames(para.est)=c('HPD','lower','upper')

for(i in 1:ncol(output)){
den=density(output[,i])	
para.est[i,'HPD']=round(den$x[which.max(den$y)],5)
para.est[i,'upper']=round(quantile(output[,i],probs=0.975),5)
para.est[i,'lower']=round(quantile(output[,i],probs=0.025),5)
}

para.est['pD',1]=round(fit$BUGSoutput$pD,3)
para.est['DIC',1]=round(fit$BUGSoutput$DIC,3)
write.table(para.est,'summary_par_popn_6river_walkM_HPD.txt',row.names=T,sep='\t',quote=F)

####################mean
para.est=matrix(NA,nrow=ncol(output)+2,ncol=3)
rownames(para.est)=c(colnames(output),'pD','DIC')
colnames(para.est)=c('HPD','lower','upper')

for(i in 1:ncol(output)){
den=density(output[,i])	
para.est[i,'HPD']=round(mean(output[,i]),5)
para.est[i,'upper']=round(quantile(output[,i],probs=0.975),5)
para.est[i,'lower']=round(quantile(output[,i],probs=0.025),5)
}

para.est['pD',1]=round(fit$BUGSoutput$pD,3)
para.est['DIC',1]=round(fit$BUGSoutput$DIC,3)
write.table(para.est,'summary_par_popn_6river_walkM_mean.txt',row.names=T,sep='\t',quote=F)

##################trace plot and density plot
# output=read.table('estimate_par_jags_1.txt',header=T,sep='\t',row.names=NULL,as.is=c(1:43)) #switch between rivers
n.keep=nrow(output)/3 #num of posterior samples kept at each chain after burn-in and thin, 3 chains
# n.keep=fit$BUGSoutput$n.keep #num of posterior samples kept at each chain after burn-in and thin, 3 chains

n.pannel=50 #num of pannels per plot
n.plot=ceiling(ncol(output)/n.pannel)

#trace plot
for(hh in 1:n.plot){
	pdf(paste('trace_popn_6river_walkM_',hh,'.pdf',sep=''),width=8,height=12)
	
	if(hh<n.plot){
		n.fig=n.pannel #number of pannels per plot
		layout(matrix(1:n.fig,nrow=10,ncol=5,byrow=T))
		par(mar=c(2,4,1,1),oma=c(2,2,1,1))	
	}
	
	if(hh==n.plot){
		n.fig=ncol(output)-(n.plot-1)*n.pannel
		layout(matrix(c(1:n.fig,rep(0,(n.pannel-n.fig))),nrow=10,ncol=5,byrow=T))
		par(mar=c(2,4,1,1),oma=c(2,2,1,1))
	}
	
	for(pp in 1:n.fig){
		y.min=min(output[,(hh-1)*n.pannel+pp],na.rm=T)
		y.max=max(output[,(hh-1)*n.pannel+pp],na.rm=T)
		plot(x=1:n.keep,y=output[1:n.keep,(hh-1)*n.pannel+pp],type='l',lwd=1,col='red',ylim=c(y.min,y.max),xlab='',ylab=colnames(output)[(hh-1)*n.pannel+pp],main=NULL)
		lines(x=1:n.keep,y=output[(n.keep+1):(n.keep*2),(hh-1)*n.pannel+pp],type='l',lwd=1,col='green')
		lines(x=1:n.keep,y=output[(2*n.keep+1):(n.keep*3),(hh-1)*n.pannel+pp],type='l',lwd=1,col='blue') #3 chains in total
	}
	mtext('Iteration',1,outer=T)

dev.off()
}

#density plot
for(hh in 1:n.plot){
	pdf(paste('density_popn_6river_walkM_',hh,'.pdf',sep=''),width=8,height=12)
	
	if(hh<n.plot){
		n.fig=n.pannel #number of pannels per plot
		layout(matrix(1:n.fig,nrow=10,ncol=5,byrow=T))
		par(mar=c(4,2,1,1),oma=c(2,2,1,1))	
	}
	
	if(hh==n.plot){
		n.fig=ncol(output)-(n.plot-1)*n.pannel
		layout(matrix(c(1:n.fig,rep(0,(n.pannel-n.fig))),nrow=10,ncol=5,byrow=T))
		par(mar=c(4,2,1,1),oma=c(2,2,1,1))
	}
	
	for(pp in 1:n.fig){
		den.1=density(output[1:n.keep,(hh-1)*n.pannel+pp])
		den.2=density(output[(n.keep+1):(n.keep*2),(hh-1)*n.pannel+pp])
		den.3=density(output[(2*n.keep+1):(n.keep*3),(hh-1)*n.pannel+pp])
		
		x.min=min(output[,(hh-1)*n.pannel+pp],na.rm=T)
		x.max=max(output[,(hh-1)*n.pannel+pp],na.rm=T)
		y.max=max(max(den.1$y),max(den.2$y),max(den.3$y))
		
		plot(den.1,type='l',lwd=1,col='red',xlim=c(x.min,x.max),ylim=c(0,y.max),ylab='',xlab=colnames(output)[(hh-1)*n.pannel+pp],main='')
		lines(den.2,type='l',lwd=1,col='green')
		lines(den.3,type='l',lwd=1,col='blue') #3 chains in total
	}
	mtext('Density',2,outer=T)

dev.off()
}







