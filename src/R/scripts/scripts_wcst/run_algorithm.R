# Sequential learning models for the Wisconsin Card Sort Task. 
# Programmed by Anthony Bishara. 
# More details can be found in: Bishara, A. J., Kruschke, J.
# K., Stout, J. C., Bechara, A., McCabe, D. P., & Busemeyer, J. R. (2010).
# Sequential learning models for the Wisconsin Card Sort Task: Assessing
# processes in substance dependent individuals. Journal of Mathematical
# Psychology, 54, 5-13.
# Version 18b, December 2019
# (added error message for impossible punishment or reinforcement,
# eliminated error messages for lacking subject labels in column 257,
# removed output columns that were empty,
# improved comments and some formatting)


# See Intructions.txt for detailed instructions on using this code



# rm(list=ls(all=TRUE))  					#clears memory


# MODIFY THIS SECTION ---------------------------------------------------------

datname = "onesubjdata.txt"

# Number of starting parameters to iterate through. Default=100.
# Lower values are faster, but can lead to inaccurate parameter estimates.
maxiter = 100		

# Vector of nested models to run; Default=5, the best fitting model in the
# article; use "modelstorun=1:24" to run all of them (much slower)
modelstorun = 5   

# Lower boundaries for parameters r, p, d, i (i was called "f" in the article)
# then upper boundaries for parameters r, p, d, i
parbounds = c(0, 0, .01, .01,
              1, 1, 5, 5)  
#  -----------------------------------------------------------------------------

# If fOptions package isn't installed, install it now. Internet access required.
if(sum(rownames(installed.packages()) == "fOptions")==0)
  install.packages("fOptions")
library(fOptions)  						#load fOptions into memory.
#Warning messages about version numbers are usually benign.

rawdatamat = read.table(datname, encoding = "UTF-8")  #Read data file.
# If you receive an error after this line, then the datafile isn't being read in
# properly. One possible problem is that the datafile doesn't have UTF-8
# Encoding. One solution is to use the Notepad program to resave the datafile.
# At the bottom of the Save dialog window, next to "Encoding:", choose "UTF-8"
# before saving the datafile. If that does not fix the problem, try opening the
# file in MS Word (using windows default encoding), delete the unusual
# characters, and then resave it (again using windows default encoding).

# Use subject labels if available in the datafile
if (ncol(rawdatamat) >= 257) {
  subjlabels = rawdatamat[, 257]
} else {
  subjlabels = 1:nrow(rawdatamat)
}

lb = parbounds[1:4]	  # lower parameter bounds
ub = parbounds[5:8]   # upper parameter bounds

itercolumns = maxiter
subjectsmodeled = 1:length(rawdatamat[, 1])
savelabel = paste("Code18b_", strtrim(datname,6), "_s", min(subjectsmodeled),
                  "-", max(subjectsmodeled), "_iter", maxiter,sep="")

# Define 128 match matrices (3 dimensions x 4 decks x 128 trials)
matchstack = array(rep(0, (3 * 4 * 128)), dim = c(3, 4, 128)) 
correctdeckmat = read.table("correctdeck.txt", header = 1)
for (i in 1:4) 
  matchstack[, i, ] = array(as.numeric(correctdeckmat[, ] == i), dim=c(3, 128))

# Functions to transform parameters to have infinite boundaries (stretch) and
# back to original boundaries (contract)
stretchpars = function(opars)
  - log((ub - lb) / (opars - lb) - 1)	
contractpars = function(spars)
  (ub - lb) / (exp(-spars) + 1) + lb	

# Function to transform a vector of 3 pars that would sum to 1 into 2 pars that
# range from 0-1. This is needed for optimization with range 0-1 can work.  
# e.g. (.6,.1,.3) transforms to (.6,.25)
scale3to2 = function(fpars)
  c(fpars[1], fpars[2] / (1 - fpars[1]))

# Function to transform a vector of 2 pars back to 3 pars
scale2to3 = function(fpars)
  c(fpars[1],
    (1 - fpars[1]) * fpars[2],
    1 - fpars[1] + (fpars[1] - 1) * fpars[2])

# Function that takes a vector, raises to a power, then rescales to sum to 1
powerrize=function(pow,tempvec) (tempvec^pow)/sum(tempvec^pow)

# Function to predict probabilitys, assuming constant attention weights
cattpredpfun=function(fpars, flength) {
  tempmat=matrix(-1,ncol=flength,nrow=4)
  for (ftrial in 1:flength) tempmat[,ftrial]=t(fpars%*%matchstack[,,ftrial])
  tempmat
}

# An overarching model function calculating the predicted probabilities.
# It takes parameters r, p, d, i ("i" might be called "g" or "f" in paper).
# freeletters: vector of characters in any order for the free parameter letters
# fixedvals: ordered vector numbers that only matters for nonfree parameters
# pequalsr: boolean for whether constraint p=r is true
vattpredpfun9=function(fpars,freeletters,fixedvals,pequalsr,fchoices,tempreinf) { 
  if(sum(freeletters=="r")>0) r=fpars[1] else r=fixedvals[1]
  if(sum(freeletters=="p")>0) p=fpars[2] else p=fixedvals[2] 
  if(sum(freeletters=="d")>0) d=fpars[3] else d=fixedvals[3]
  if(sum(freeletters=="i")>0) i=fpars[4] else i=fixedvals[4]
  if(pequalsr) p=r
  
  flength = length(fchoices)
  curatt = rep((1 / 3), 3)
  subjattmat = matrix(nrow = 3, ncol = flength)
  predpmat = matrix(-1, nrow = 4, ncol = flength)
  subjattmat[, 1] = curatt
  for (ftrial in 1:(flength - 1)) {
    attmatchchoice = correctdeckmat[, ftrial] == fchoices[, ftrial]
    doubleatt = curatt * .9999997 + .0000001 #rescale to prevent rounding errors
    if (tempreinf[, ftrial] == 1) {
      attsignal = powerrize(i, (attmatchchoice * doubleatt))
      curatt = (1 - r) * curatt + r * attsignal
    } else {
      attsignal = powerrize(i, ((1 - attmatchchoice) * doubleatt))
      curatt = (1 - p) * curatt + p * attsignal
    }
    predpmat[,ftrial+1] = t(powerrize(d,curatt)%*%matchstack[,,ftrial+1])
  }
  return(predpmat)
}

# Function to generate G^2 for constant attention model
# This takes the 2 parameter pars vector (because only two are free)
cattG2fun = function(pars2, fchoices) { 
  fpars=scale2to3(pars2)
  flength=length(fchoices)
  fp.mat=matrix(c(fchoices==1,fchoices==2, fchoices==3,fchoices==4),
                nrow=4,ncol=flength,byrow=TRUE)*(cattpredpfun(fpars,flength))
  fp.mat=colSums(fp.mat)
  # remove trial 1 and rescale:
  fp.mat=fp.mat[2:flength]*.9998+.0001		
  return(-2*sum(log(fp.mat)))
}

# Function to generate G^2 for all other models
vattG2overarchfun = function(fpars, fparbounds, freeletters, fixedvals, 
                             pequalsr, fchoices, tempreinf, predpfun) {
  origpars=contractpars(fpars)
  flength=length(fchoices)
  fp.mat=matrix(c(fchoices==1,fchoices==2,fchoices==3,fchoices==4),
                nrow=4,ncol=flength,
                byrow=TRUE)*(predpfun(origpars,freeletters,fixedvals,pequalsr,
                                      fchoices,tempreinf))
  fp.mat=colSums(fp.mat)
  fp.mat=fp.mat[2:flength]*.9998+.0001		#removes trial 1 and rescales
  return(-2*sum(log(fp.mat)))
}


# PROGRAM BODY ----------------------------------------------------------
# Define a potential error message
imposs.message = paste(": Impossible feedback on at least one trial.",
                       "If this error occurs in most participants, then the",
                       "`correct deck matrix.txt` file probably needs to be",
                       "adjusted for your version of the task. If this error",
                       "is rare, then it is probably due to typo(s) in the",
                       "data file at this particular row.")

all.errors.message = paste("ERROR: No participants could be modeled. The `correct deck matrix.txt` file probably needs adjusted for your version of the task. Numbers in that file should refer to which pile (out of 4) the card would be sorted to if matched on that dimension (color, form, number).  It is also possible that the data file is in the wrong format.  See Instructions.txt for details.")

numsubj = nrow(rawdatamat)
# deckbase: a baseline model that predicts .25 prob. of choosing each deck
deckbaseG2 = rep(-1, numsubj)
deckbaseBIC = rep(-1, numsubj)
# catt33: a smarter baseline model with constant 1/3 attention to each dimension
catt33G2 = rep(-1, numsubj)
lengthvec = 128 - rowSums(rawdatamat[, 1:128] == 0) #number of trials

# setting up a matrix that lists the free pars of each of 24 models
freeparmat=matrix(nrow=24,ncol=4,dimnames=list(NULL,c("r","p","d","i")))  
# parameter constraint values of 24 models
fixedvalmat=matrix(-1,nrow=24,ncol=4,dimnames=list(NULL,c("r","p","d","i"))) 
pequalsrmat=matrix(0,nrow=24,ncol=1)
for (ploop in 0:1) for (dloop in 0:1) for (iloop in 0:2) for (rloop in 0:1) {
  rowloop=ploop*12 + dloop*6 + iloop*2 + rloop +1
  freeparmat[rowloop,1] = (if(rloop==0) "r" else "")
  freeparmat[rowloop,2] = (if(ploop==0) "p" else "")
  freeparmat[rowloop,3] = (if(dloop==0) "d" else "")
  freeparmat[rowloop,4] = (if(iloop==0) "i" else "")
  if(dloop==1) fixedvalmat[rowloop,3]=1-1e-8
  if(iloop==1) fixedvalmat[rowloop,4]=.0001
  if(iloop==2) fixedvalmat[rowloop,4]=1
  if(rloop==1) fixedvalmat[rowloop,1]=1
  if(ploop==1) pequalsrmat[rowloop,1]=1
}

parnames = apply(freeparmat,1,paste,collapse="")
modnames = parnames
modnames[freeparmat[,"i"]==""] = paste0(modnames[freeparmat[,"i"]==""], round(fixedvalmat[freeparmat[,"i"]=="","i"],1))

G2stack=array(rep(NA,(numsubj*itercolumns*24)),dim=c(numsubj,itercolumns,24)) 
BICstack=array(rep(NA,(numsubj*itercolumns*24)),dim=c(numsubj,itercolumns,24)) 
parstack=array(rep(NA,(numsubj*4*24)),dim=c(numsubj,4,24)) 

dimnames(G2stack)=list(paste0("s",subjlabels),paste0("i",1:itercolumns),paste0("m",1:24,"_",modnames))
dimnames(BICstack)=list(paste0("s", subjlabels),paste0("i",1:itercolumns),paste0("m",1:24,"_",modnames))
dimnames(parstack)=list(paste0("s",subjlabels),c("r","p","d","i"),paste0("m",1:24,"_",modnames))


presobelmat=sobelmat=runif.sobol(maxiter,4,1)
for (i in 1:4) sobelmat[,i]=presobelmat[,i]*(parbounds[i+4]-parbounds[i])+parbounds[i]
subj.errors = rep(F,length(subjectsmodeled))


#########


starttime=Sys.time()
# Loop across subjects:
for (cursubj in subjectsmodeled) {	
  curlength = lengthvec[cursubj]
  curchoices = data.frame(rawdatamat[cursubj, 1:curlength])
  curreinf = data.frame(rawdatamat[cursubj, 129:(128 + curlength)])
  
  # Check/warn for impossible reinforcing/punishing feedback problems
  reinf.ind = which(as.logical(curreinf)) # reinforced trial indices
  pun.ind = which(!as.logical(curreinf)) # punished trial indices
  any.reinf.imposs = !all(
    curchoices[reinf.ind] == correctdeckmat[1, reinf.ind] |
    curchoices[reinf.ind] == correctdeckmat[2, reinf.ind] |
    curchoices[reinf.ind] == correctdeckmat[3, reinf.ind]
  )
  any.pun.imposs = any(
    curchoices[pun.ind] == correctdeckmat[1,pun.ind] &
    curchoices[pun.ind] == correctdeckmat[2,pun.ind] &
    curchoices[pun.ind] == correctdeckmat[3,pun.ind]
  )
  
  # -----------------------------------
  
  if(any.reinf.imposs|any.pun.imposs) {
    message(paste("ERROR IN PARTICIPANT ROW", cursubj, imposs.message))
    subj.errors[cursubj] = TRUE
    next()
    }
  
  deckobsp = c(mean(curchoices==1), mean(curchoices==2),
             mean(curchoices==3), mean(curchoices==4))
  deckobsf = deckobsp*curlength
  deckbaseG2[cursubj] = -2*sum(deckobsf*log(deckobsp))
  catt33G2[cursubj] = cattG2fun(scale3to2(rep((1/3),3)), curchoices)
  
  for (curmod in modelstorun)	{ 
    #loop across models (different parameter constraints) 
    #for (curiter in 1:itercolumns)
    curiter = 0
    contiter = TRUE
    while(contiter)	{	#loop across different iterations (i.e., starting parameters)
      curiter = curiter + 1
      pars4init = sobelmat[curiter, ]
      spars4init = stretchpars(pars4init)
      tempmod = optim(
        spars4init,
        vattG2overarchfun,
        fparbounds = parbounds,
        freeletters = freeparmat[curmod, ],
        fixedvals = fixedvalmat[curmod, ],
        pequalsr = pequalsrmat[curmod, ],
        fchoices = curchoices,
        tempreinf = curreinf,
        predpfun = vattpredpfun9,
        method = "Nelder-Mead"
      )
      
      G2stack[cursubj,curiter,curmod]=tempmod$value
      roundpars=round(contractpars(tempmod$par),3)					
      print(noquote(c("subj#=",cursubj," iter=",curiter," model=",
                      modnames[curmod], "  -2LL=",round(tempmod$value,3) )))
      print(noquote(c("r=",roundpars[1],"  p=",roundpars[2],"  d=",roundpars[3],
                      "   i=",roundpars[4])))
      print(noquote(""))
      flush.console()
      
      if(curiter==1) {
        parstack[cursubj,,curmod]=contractpars(tempmod$par) 
        } else {
        if(tempmod$value<min(G2stack[cursubj,1:curiter-1,curmod])) 
          parstack[cursubj,,curmod]=contractpars(tempmod$par)
      }
      BICstack[cursubj,curiter,curmod]=G2stack[cursubj,curiter,curmod]+sum(freeparmat[curmod,]!="")*log(curlength-1)
      if(curiter>=maxiter) contiter=FALSE
    }
  }
  deckbaseBIC[cursubj]=deckbaseG2[cursubj]+3*log(curlength-1)
  
  
}
timeelapsed=Sys.time()-starttime
timeelapsed

if(!all(subj.errors)) {
  G2finalmat=array(rep(NA,(length(subjectsmodeled)*24)),dim=c(length(subjectsmodeled),24))
  BICfinalmat=array(rep(NA,(length(subjectsmodeled)*24)),dim=c(length(subjectsmodeled),24))
  dimnames(G2finalmat)=list(paste0("s",subjlabels[subjectsmodeled]),paste0("G2m",1:24,"_",modnames)) 
  dimnames(BICfinalmat)=list(paste0("s",subjlabels[subjectsmodeled]),paste0("BICm",1:24,"_",modnames)) 

  parfinalmat=NULL
  parlabels=NULL
  for (i in modelstorun) {
    G2finalmat[,i]=apply((G2stack[subjectsmodeled,,i]),1,min,na.rm=1)
    BICfinalmat[,i]=apply((BICstack[subjectsmodeled,,i]),1,min,na.rm=1)
    parfinalmat=cbind(parfinalmat,parstack[subjectsmodeled,,i])
    parlabels=c(parlabels,paste0("m",i,"_",modnames[i],"_",freeparmat[1,]))
  }
  G2BICmat=cbind(catt33G2[subjectsmodeled],G2finalmat,BICfinalmat)
  colnames(G2BICmat)[1]="catt33G2"

  G2BICmat=cbind(subjlabels[subjectsmodeled],G2BICmat)

  colnames(G2BICmat)[1]=c("subjnum")
  colnames(parfinalmat)=parlabels
  all.to.write = cbind(G2BICmat,parfinalmat)
  #Clear empty columns
  wipe.cols = rep(F, ncol(all.to.write))
  for (i in 1:ncol(all.to.write) ) {
    if(all(is.na(all.to.write[ ,i]))) wipe.cols[i] = TRUE
  }
  all.to.write = all.to.write[ , !wipe.cols]
  write.table(all.to.write, paste(savelabel,".txt",sep=""), 
              row.names=F)
} else {
  message(all.errors.message)
}

save.image(paste(savelabel,".Rdata",sep=""))
