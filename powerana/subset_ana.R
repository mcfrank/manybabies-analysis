read.csv("ids_ma.csv")->x
table(x$infant_type)
table(x$stim_language)
table(x$setting)
table(x$speaker)
table(x$speech_type)
subset(x, speech_type != "Filtered" & speech_type != "Synthesized" &
         speaker!="Child\342\200\231s mother" & speaker != "Unfamiliar male" & 
         setting=="Laboratory" & 
         stim_language=="English" & 
         infant_type=="typical")->relevant

library(metafor)
x$se=sqrt(x$d_var)
x$w=1/sqrt(x$d_var)
rma.uni(yi=d, sei=se, weights=w,data=x)
