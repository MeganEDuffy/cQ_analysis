####################################################################
# Event concentration discharge analysis including hysteresis
# R Code
# written by Qing Zhan, Rémi Dupas, Camille Minaudo and Andreas Musolff
# version 1.1, 2021-07-23
####################################################################
### description
# This code uses joint time series of concentration and discharge to 
# (1) separate discharge events and store them in a data frame (events_h) and
# (2) analyse C-Q relationships including hysteresis, derive metrics describing these 
#     and store them in a data frame (eve.des)

# Note that input data are needed as a data frame and not as a time series object.
#   in the given example the data frame is called "dat", time is column "datum", 
#   concentration column is named "C", discharge "Q.smooth"

### package needed for model fitting
library("minpack.lm")


####################################################################
### Part (1) event detection and separation
####################################################################
###detection storm events from 15min discharge in mm/d
# parameters of the event separation needs to be adjusted
# also other event detenction and separation algorithms may be used

dat<-unique(dat)

nb<-0 #event identification number
events_h<-c() #final table with event extracted + identification number
event<-c() #table with event extracted (only in the loop)
step<-5 # depending on temporal resolution, small catchments 4, large catchments 50
i<-step+1

start<-0 #switches to 1 when a potential storm events has started

while (i<length(dat[,1])){ 
  event<-c()
  if ((dat$Q.smooth[i]-dat$Q.smooth[i-step])/(dat$Q.smooth[i-step])>0.1 & (dat$Q.smooth[i+step]-dat$Q.smooth[i])/(dat$Q.smooth[i])>0.1 & start==0){
    #event starts when +10% increase in 1h (step x 15 min) #this parameter must be adjusted 0.1 for flashy, 0.05 for larger catchments
    start<-1
    event<-dat[c((i-step):i),] 
  }
  i=i+1
  
  while (start==1){
    while ((dat$Q.smooth[i]-dat$Q.smooth[i-step])>0 | (max(event$Q.smooth)-dat$Q.smooth[i])/(max(event$Q.smooth)-event$Q.smooth[1]+0.0001)<0.5){
      # we add lines as long as discharge keeps on increasing or has dropped <20-50% max discharge #this parameter must be adjusted
      event<-rbind(event, dat[i,])
      i<-i+1
    }
    
    while(((dat$Q.smooth[i]-dat$Q.smooth[i-step])/(dat$Q.smooth[i-step]) < -0.005 )
          # the event stops when discharge has almost stabilized	(0.5% variation in discharge in 15min * step) #this parameter must be adjusted, most sensitive for event ending
          
    ){
      event<-rbind(event, dat[i,])
      i<-i+1
    }
    start=2
  }
  if (start==2){
    if (length(event$Q.smooth)>15 & (max(event$Q.smooth, na.rm=T)-event$Q.smooth[1])>0.8){ # the potential event is finally selected if discharge has increased enough (most sensitive for event count) and if long enough these parameters must be adjusted
      nb=nb+1
      events_h<-rbind(events_h, cbind(event, rep(nb, dim(event)[1])))
    }
  }
  start<-0
}

# rename column defining the event number
colnames(events_h)[6]<-"nb" #column is "rep(nb, dim(event)[1])" to be renamed

save(events_h,file="events_h.rdata")

####################################################################
### Part (2) C-Q modeling of individual event
####################################################################
# uses the events_h data frame created above
# script runs for one constituents only (multiple constituents needs multiple runs)

load(file="events_h.rdata")

events_h<-unique(events_h)
events_h_C<-na.omit(events_h[,c(1:2,5,6)]) #select constituent here EC - 3, NO3 - 4, SAC - 5 - depending on the given data structure
# the resulting data frame stores columns [1] - datum (timestamp), [2] - Q.smooth (discharge), [3] - C (concentration), [4] - nb (event number)

colnames(events_h_C)[3]="C"

### Part (2a) creating a list (event.list) where C and Q data is stored for each event and models C-Q with hysteresis

# creating the vectors to store results
event <- c()
dQ <- c()
dt <- c()
dQdt <- c()
Q <- c()
C <- c()
modelallnls <- c()

formu <- (C~(a*(Q^b)+(c*dQdt))) # the eqation of C-Q relationship including hysteresis

scale_range <- function(x){x/((max(x))-(min(x)))} # function to normalize dQdt with its range

event.list<-unique(events_h_C$nb)

for (i in event.list) {
  event[[i]] <- events_h_C[which(events_h_C$nb==i),]
  dQ[[i]] <- diff(event[[i]]$Q.smooth)
  dt[[i]] <- as.numeric(diff(event[[i]]$datum))
  dQdt[[i]] <- scale_range(dQ[[i]]/dt[[i]])
  C[[i]] <- event[[i]]$C[-1]            
  Q[[i]] <- event[[i]]$Q.smooth[-1]
  modelallnls[[i]]  <- nlsLM(formu, 
                               data = list(C=C[[i]],Q=Q[[i]],dQdt=dQdt[[i]]),
                               start = list(a=.1, b=.1, c=.1))
 
  
}


### Part (2b) looping through the list (event.list) to derive CQ metrics

eve.des <- c()

event.list<-unique(events_h_C$nb)

# deriving begin and end of the events
for (i in event.list) {
  eve.des$Begin[i] <- as.character(events_h_C$datum[which(events_h_C$nb==i)][1])
  eve.des$End[i] <- as.character(events_h_C$datum[which(events_h_C$nb==i)][length(which(events_h_C$nb==i))])
}

# deriving Q min, max, mean and C starting concentrations, min and max
for (i in event.list) {
  eve.des$Qmean[i] <- mean(events_h_C$Q.smooth[which(events_h_C$nb==i)])
  eve.des$Qmax[i] <- max(events_h_C$Q.smooth[which(events_h_C$nb==i)])
  eve.des$Qmin[i] <- min(events_h_C$Q.smooth[which(events_h_C$nb==i)])
  eve.des$C0[i] <- events_h_C$C[which(events_h_C$nb==i)][1]
  eve.des$Cmax[i] <- max(events_h_C$C[which(events_h_C$nb==i)])
  eve.des$Cmin[i] <- min(events_h_C$C[which(events_h_C$nb==i)])
}

# function needed to judge "even" or "odd" numbers in hysteresis index
parity <- function(x){
  ifelse(x%%2 == 0, "even", "odd")
}

# deriving the hysteresis index after Lloyd, adapted by Vaughan et al. (2017)
for (i in event.list) { 
  events_h.tem <- events_h_C[which(events_h_C$nb==i),]
  Qmax<-max(events_h.tem$Q.smooth)
  Qmin<-min(events_h.tem$Q.smooth)
  Qnorm.tem<-(events_h.tem$Q.smooth-Qmin)/(Qmax-Qmin)
  Cmax<-max(events_h.tem$C)
  Cmin<-min(events_h.tem$C)
  Cnorm.tem<-(events_h.tem$C-Cmin)/(Cmax-Cmin)
  CQ.tem<-data.frame(Qnorm.tem,Cnorm.tem)
  if (parity(which.max(CQ.tem$Qnorm.tem))=="odd"){
    CQ.tem.RL <- CQ.tem[1:which.max(CQ.tem$Qnorm.tem),]
  }else {
    CQ.tem.RL <- CQ.tem[2:which.max(CQ.tem$Qnorm.tem),]
  }
  if (parity(length(CQ.tem$Qnorm.tem)-which.max(CQ.tem$Qnorm.tem))=="even"){
    CQ.tem.FL <- CQ.tem[which.max(CQ.tem$Qnorm.tem):length(CQ.tem$Qnorm.tem),]
  }else {
    CQ.tem.FL <- CQ.tem[which.max((CQ.tem$Qnorm.tem)+1):length(CQ.tem$Qnorm.tem),]}
  if ((length(CQ.tem.RL$Qnorm.tem)>3) & (length(CQ.tem.FL$Qnorm.tem)>3)) {
      C.RL.new<-approx(CQ.tem.RL$Qnorm.tem,CQ.tem.RL$Cnorm.tem, xout=(seq(0,1, by=1/50)))
  C.FL.new<-approx(CQ.tem.FL$Qnorm.tem,CQ.tem.FL$Cnorm.tem, xout=(seq(0,1, by=1/50)))
  C.Diff.new<-na.omit(data.frame(C.RL.new$y,C.FL.new$y))
  eve.des$HILLoyd[i]=mean(C.Diff.new$C.RL.new.y-C.Diff.new$C.FL.new.y)
  }else{
    eve.des$HILLoyd[i]=NA
  }
}

# deriving the parameters and R2 of simple power law C-Q without hysteresis
for (i in event.list) {
  events_h.tem <- events_h_C[which(events_h_C$nb==i),]
  model <- lm(log(C)~log(Q.smooth),data = events_h.tem)
  eve.des$slopeCQ[i] <-model$coefficients[2] #slope b of C-Q
  eve.des$r.sq.CQ[i] <-summary(model)$r.squared #R2 of C-Q
}

# deriving parameters of the modelled C-Q with hysteresis 
for(i in event.list){
  eve.des$a[i] <- round(summary(modelallnls[[i]])$coefficients[1],digits = 3)
  eve.des$b[i] <- round(summary(modelallnls[[i]])$coefficients[2],digits = 3)
  eve.des$c[i] <- round(summary(modelallnls[[i]])$coefficients[3],digits = 3)
}

# deriving coefficient of determination of the modelled C-Q with hysteresis 
for (i in event.list) {
  predicted <- predict(modelallnls[[i]])
  eve.des$model.r.sq[i] <- cor(predicted, C[[i]])^2
}

# deriving cumulative discharge during the event
for (i in event.list) {## cumulative Q within event
  events_h.tem <- events_h_C[which(events_h_C$nb==i),]
  Q.cumsum <- sum(events_h.tem$Q.smooth)
  eve.des$Q.cumsum[i] <- Q.cumsum
}


# deriving AKAIKE information criterion for the simple C-Q model without hysteresis
for (i in event.list) {
  events_h.tem <- events_h_C[which(events_h_C$nb==i),]
  model <- lm(log(C)~log(Q.smooth),data = events_h.tem)
  eve.des$slopeCQ[i] <-model$coefficients[2]
  eve.des$r.sq.CQ[i] <-summary(model)$r.squared
  formu.lm= (C~(a*(Q^b)))
  
  Q.smooth2 = events_h.tem$Q.smooth
  C.2= events_h.tem$C
  model.lm  <- nlsLM(formu.lm, data = list(C=C.2,Q=Q.smooth2),
                     start = list(a=.1, b=.1))
  AKAIKE.lm <- AIC(model.lm)
  eve.des$AIC.lm[i] <- AKAIKE.lm
}

# deriving AKAIKE information criterion for the C-Q model with hysteresis
for (i in event.list) {## recording of correlations of models
  AKAIKE <- AIC(modelallnls[[i]])
  eve.des$AIC.Hysteresis[i] <- AKAIKE
}

save(eve.des,file="event_des.rdata")


