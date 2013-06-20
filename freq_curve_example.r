source("C:/dssrt/dssrt.r")
library(ggplot2)

## Open file and get TSC
bwfile = opendss("C:/dssrt/test.dss")
bwcreek = getFullTSC(bwfile, "A=BRANDYWINE CREEK C=FLOW E=1DAY")
colnames(bwcreek) = "FLOW" ## TODO - add naming the columns by C-part

## Do people even use water years back east?
bwcreek$WY = 1900 + ifelse(.indexmon(bwcreek)+1 < 9, .indexyear(bwcreek), 1+.indexyear(bwcreek))

## Get peaks for each water year, assign probabilities
bwpeaks = aggregate(FLOW ~ WY, bwcreek, max)
bwpeaks = subset(bwpeaks, WY!=2013)
bwpeaks = bwpeaks[order(bwpeaks$FLOW),]
n = length(bwpeaks$FLOW)
bwpeaks$PROB = (1:n)/(1+n)

## Setup breaks for plot
xbreaks = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.995)
log.range = log10(range(bwpeaks$FLOW))
lower = 10^floor(log.range[1])
upper = 10^ceiling(log.range[2])
cap = lower
ybreaks = NULL
while(cap < upper){
  ybreaks = c(ybreaks, seq(cap, cap*9, by=cap)) 
  cap = cap*10
}

## ggplot2 frequency curve example
plt = ggplot(bwpeaks, aes(x=PROB, y=FLOW)) + geom_point() + theme_bw()
plt = plt + scale_y_continuous(trans="log10", breaks=ybreaks, name="Flow (cfs)")
plt = plt + scale_x_continuous(trans=probability_trans(distribution="norm"),
                               breaks=xbreaks, labels=paste0((1-xbreaks)*100,"%"),
                               name="Annual Chance of Exceedence")
plt = plt + ggtitle("Brandywine Creek nr. Wilmington, Delaware")
## show plot
plt
