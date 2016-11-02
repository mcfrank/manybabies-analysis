#import the metafor meta-nalysis R library. You will need to install it first.
require("metafor")

# Import the data
RTVdat<- read.csv("RTV.csv")
original<-data.frame(study=0,Study.name="Sripada et al.",Subgroup.within.study="English",
          Control.Mean=0.2740440,Control.Std.Dev=0.05083227,
          Control.Sample.size=24,
          Ego.Depletion.Mean=0.3181729,Ego.Depletion.Std.Dev=0.07471862,
          Ego.Depletion.Sample.size=23, #Emailed by Daniel Kessler
          Std.diff.in.means=NA,Std.Err=NA,Hedges.s.g=NA,Std.Err.1=NA,Difference.in.means=NA,Std.Err.2=NA)
RTVdat<-rbind(RTVdat,original)
RTVdat<- RTVdat[order(RTVdat$study),]
### random-effects model meta-analysis 
# modeled after http://www.metafor-project.org/doku.php/tips:assembling_data_smd
effectSizesAll<- escalc(measure="SMD", #standardized mean difference
                     m1i= Ego.Depletion.Mean, m2i= Control.Mean,
                     sd1i=Ego.Depletion.Std.Dev, sd2i= Control.Std.Dev,
                     n1i=Ego.Depletion.Sample.size, n2i=Control.Sample.size,
                     data= RTVdat)
#(RTVdat$Ego.Depletion.Mean - RTVdat$Control.Mean) / RTVdat$Ego.Depletion.Std.Dev

res <- rma(data=effectSizesAll, yi,vi,  method="REML", slab=paste(Study.name))

#res <- rma(measure="RD", method="REML")

#For a standard random-effects model, we need to add random effects for the trials, which can be done with:
#  rma.mv(yi, vi, random = ~ 1 | trial, data=dat)

###############################################################
### FOREST PLOT WITH META-ANALYSIS ACROSS REPLICATION STUDIES
###############################################################

### Create the forest plot. 
### Rows specify the rows in which the subsets of studies appear.
### addfit=false eliminates the meta-analytic result for now. We'll first not calculate that
### so we can create a forest plot that includes all the studies, even the original, and
### then we'll calculate the meta-analysis based just on the RRR studies.

### decrease margins so the full space is used and set the font size for the forest plot
par(mar=c(4,4,1,2))
par(cex=.8, font=1)

#Create a forest plot that includes the original study
forest(res,
       ilab=cbind(RTVdat$Control.Mean, RTVdat$Ego.Depletion.Mean),
       addfit=FALSE, #Don't include polygon because that would include original
       ilab.xpos=c(-1.2,-.9), #ylim=c(-1, 47) 
       #xlim=c(-10,7)
       #xlim=c(-12,5)
       #xlim=c(-12,8)
#      xlim=c(-5,2.5)
      xlim=c(-4.5,2.5)
)
par("usr") #reveals xlims? http://r.789695.n4.nabble.com/tweaking-forest-plot-metafor-package-td4636818.html

### Create a subset of the data that excludes The Schooler and MTurk data and renumber it
RRRdat <- subset(RTVdat,study != 0)
rownames(repdata) <- NULL

# modeled after http://www.metafor-project.org/doku.php/tips:assembling_data_smd
effectSizesRRR<- escalc(measure="SMD", #standardized mean difference
                        m1i= Ego.Depletion.Mean, m2i= Control.Mean,
                        sd1i=Ego.Depletion.Std.Dev, sd2i= Control.Std.Dev,
                        n1i=Ego.Depletion.Sample.size, n2i=Control.Sample.size,
                        data= RRRdat)
### run a meta-analysis on just the RRR studies
res <- rma(data=effectSizesRRR, yi,vi,  method="REML", slab=paste(Study.name))

### add a polygon to the Forest plot showing the meta-analytic effect of the replication studies
addpoly(res, atransf=FALSE, row=0, cex=1.0, mlab="Meta-analytic effect for replications only")

#Formatting TO-DO
#Truncate authors, so list only first two
#Round means so don't have so many decimal points

###############################################################
### AFTER THIS IS DAN SIMONS' VERBAL OVERSHADOWING CODE !!!!!!!!!!!!!!!
###############################################################
### switch to bold, bigger font for headers and then add the headers
par(font=2, cex=1.0)
text(-3, 46, "Laboratory", pos=4)
text(-1.5, 46, "Verbal")
text(-1, 46, "Control")
text(2, 46, "Difference [95% CI]", pos=2)
text(-3, c(39, 14), c("Completed Both RRR Studies", "Completed RRR Study 1 Only"), pos=4)
abline(h=1)
abline(h=43)
abline(h=41)

### random-effects model meta-analysis that includes all replications, Mturk, and original
res <- rma(measure="RD", ai=V_Hit, bi=V_Wrong, ci=C_Hit, di=C_Wrong, 
           data=data, slab=paste(Author))

### Sets the default font size and aligns left
op <- par(cex=.75, font=4)

### Create the forest plot. 
### Rows specify the rows in which the subsets of studies appear.
### addfit=false eliminates the meta-analytic result for now. We'll recalculate it below
### in order to exclude the original data and mTurk replication.
forest(res, xlim=c(-3,2), at=c(-.60, -.40, -.20, 0, .20, .40, .60), 
       addfit=FALSE, atransf=FALSE,
       ilab=cbind(VerbalAccuracy, ControlAccuracy),
       ilab.xpos=c(-1.5,-1), ylim=c(-1, 47),
       rows=c(44, 42, 37:16, 12:4),
       xlab="Verbal Overshadowing Effect", mlab="Random Effects Model")


### switch to bold, bigger font for headers and then add the headers
par(font=2, cex=1.0)
text(-3, 46, "Study", pos=4)
text(-1.5, 46, "Verbal")
text(-1, 46, "Control")
text(2, 46, "Difference [95% CI]", pos=2)
text(-3, c(39, 14), c("Completed Both RRR Studies", "Completed RRR Study 1 Only"), pos=4)
abline(h=1)
abline(h=43)
abline(h=41)

### Create a subset of the data that excludes The Schooler and MTurk data and renumber it
repdata <- data[3:33,]
rownames(repdata) <- NULL

### run a meta-analysis on just the laboratory replication studies
repres <- rma(measure="RD", ai=repdata$N_verbal_correct, bi=(repdata$N_verbal_included - repdata$N_verbal_correct), ci=repdata$N_control_correct, di=(repdata$N_control_included - repdata$N_control_correct), data=repdata)

### add a polygon to the Forest plot showing the meta-analytic effect of the replication studies
addpoly(repres, atransf=FALSE, row=0, cex=1.0, mlab="Meta-analytic effect for laboratory replications only")


### set font size and format back to default back to the original settings
par(op)

### Print the results of the meta-analysis to the console
repres
