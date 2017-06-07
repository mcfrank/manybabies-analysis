read.csv("ids_ma.csv")->x
table(x$coder,x$participant_design)
#new lines don't have ES, they are all within_two
summary(x$corr) #all NA

x$pooled_SD=x$corr_imp=x$d_calc=x$d_calc_var=x$es_method=NA
x$corr_imp=0

#between calc
x$pooled_SD[x$participant_design=="between"]<- sqrt(((x$n_1[x$participant_design=="between"] - 1) * x$SD_1[x$participant_design=="between"] ^ 2 + (x$n_2[x$participant_design=="between"] - 1) * x$SD_2[x$participant_design=="between"] ^ 2) / (x$n_1[x$participant_design=="between"] + x$n_2[x$participant_design=="between"] - 2)) # Lipsey & Wilson, 3.14
x$es_method[x$participant_design=="between"]  <- "between"

#within_two calc
#summary(x[x$participant_design=="within_two",c("x_1","x_2","SD_1","SD_2")]) #checking - 41 NAs corresponds to the original studies
x$pooled_SD[x$participant_design=="within_two"]<- sqrt((x$SD_1[x$participant_design=="within_two"] ^ 2 + x$SD_2[x$participant_design=="within_two"] ^ 2) / 2) # Lipsey & Wilson, 3.14
x$es_method[x$participant_design=="within_two"]  <- "group_means_two"

x$d_calc <- (x$x_1 - x$x_2) / x$pooled_SD # Lipsey & Wilson (2001)
x$d_var_calc <- (2 * (1 - x$corr_imp)/ x$n_1) + (x$d_calc ^ 2 / (2 *x$n_1)) # Lipsey & Wilson (2001)

summary(x$d_var_calc) #50 NAs
x$d_calc[x$coder=="Alex Cristia"]<-x$d[x$coder=="Alex Cristia"]
x$d_var_calc[x$coder=="Alex Cristia"]<-x$d_var[x$coder=="Alex Cristia"]
summary(x$d_var_calc) #no NAs

x$se=sqrt(x$d_var_calc)
x$w=1/sqrt(x$d_var_calc)


library(metafor)
original=rma.uni(yi=d_calc, sei=se, weights=w,data=x,subset=c(coder=="Alex Cristia"))
summary(original)

withNew=rma.uni(yi=d_calc, sei=se, weights=w,data=x)
summary(withNew)

table(x$infant_type)
table(x$stim_language)
table(x$setting)
table(x$speaker)
table(x$speech_type)
table(x$dependent_measure)

subset(x, speech_type != "Filtered" & speech_type != "Synthesized" &
         speaker!="Child\342\200\231s mother" & speaker != "Unfamiliar male" & 
         setting=="Laboratory" & 
         stim_language=="English" & 
         infant_type=="typical" & 
         dependent_measure=="looking_time" &
         mean_age_1>3*30.25 & mean_age_1<15*30.25)->relevant
selected=rma.uni(yi=d_calc, sei=se, weights=w,data=relevant)
summary(selected)
relevant$age.c=relevant$mean_age_1-mean(relevant$mean_age_1,na.rm=T)
with_age=rma.uni(yi=d_calc, sei=se, weights=w,mods=age.c,data=relevant)
summary(with_age)

subset(x, speech_type != "Filtered" & speech_type != "Synthesized" &
         speaker!="Child\342\200\231s mother" & speaker != "Unfamiliar male" & 
         setting=="Laboratory" & 
         stim_language=="English" & 
         infant_type=="typical" & 
         dependent_measure=="looking_time" &
         mean_age_1>6*30.25 & mean_age_1<9*30.25)->relevant
selected_6to9=rma.uni(yi=d_calc, sei=se, weights=w,data=relevant)
summary(selected_6to9)

subset(x, speech_type != "Filtered" & speech_type != "Synthesized" &
         speaker!="Child\342\200\231s mother" & speaker != "Unfamiliar male" & 
         setting=="Laboratory" & 
         stim_language=="English" & 
         infant_type=="typical" & 
         dependent_measure=="looking_time" &
         mean_age_1>9*30.25 & mean_age_1<15*30.25)->relevant
selected_12to15=rma.uni(yi=d_calc, sei=se, weights=w,data=relevant)
summary(selected_12to15)
