#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
	# Application title
	titlePanel("MB1 ES simulation: paired versus unpaired trials"),
	
	# Sidebar with a slider input for number of bins
	sidebarLayout(
		sidebarPanel(
			sliderInput(
				"factorNoise",
				"Noise factor",
				min = 0,
				max = 3,
				value = 1,
				step = .25
			),
			sliderInput(
				"ageg_decrease",
				"% LT decrease over 5 age groups; 50 = LT is halved over the 5 age groups",
				min = 0,
				max = 100,
				value = 30
			),
			sliderInput(
				"trialtype_increase",
				"% LT increase for IDS compared to ADS; 20 = LTs are 20% longer for IDS than ADS",
				min = 0,
				max = 100,
				value = 20
			),
			sliderInput(
				"order_decrease",
				"% LT decrease over 8 trials; 30 means that LT on trial 8 is 70% that of trial 1",
				min = 0,
				max = 100,
				value = 30
			)
		),
		
		# Show a plot of the generated distribution
		mainPanel(tabsetPanel(
			tabPanel("Noise level", plotOutput("noisePlot")),
			tabPanel("Difference LT", plotOutput("difplot")),
			tabPanel("Data loss", plotOutput("dataLossPlot")),
			tabPanel("ESs", plotOutput("esplot"))
		))
	)
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
	dosim <-
		function(factorNoise,
						 ageg_decrease,
						 trialtype_increase,
						 order_decrease) {
			#fixed parameters
			nrep = 20 # number of replications
			nagegroups = 5 #number of age groups
			nkid = 20 #per replication
			trialdur = 10 #rough trial duration
			
			
			ageg_coeff = seq(1,
											 1 - (ageg_decrease / 100),
											 -(ageg_decrease / 100) / (nagegroups - 1))
			#to model that some kids look longer overall than others, and that the mean LT & variance decreases with age
			kidLT_coeff = NULL
			for (i in 1:nagegroups)
				kidLT_coeff = c(kidLT_coeff,
												rnorm(
													n = nrep * nkid,
													mean = 1 * ageg_coeff[i],
													sd = .2 * ageg_coeff[i]
												))
			
			trialtype_coeff = c(1, 1 + (trialtype_increase / 100))
			
			order_coeff = seq(1, 1 - (order_decrease / 100), -(order_decrease / 100) /
													(8 - 1))
			
			#lt here is entirely determined by the kid's own attention level(dependent on the age group), the trialtype, and trial order
			lts = (trialdur * kidLT_coeff)  %*% t(rep(order_coeff, each = 2) * trialtype_coeff)
			#dim(no_noise_lts) #2000 rows because 100 reps of 20 kids, 16 trials because 8 of each type
			
			#next a noise matrix #assumes independence of noise from all other factors
			noise = matrix(rnorm(
				n = dim(lts)[1] * dim(lts)[2],
				mean = factorNoise,
				sd = factorNoise
			),
			ncol = dim(lts)[2])
			
			lts = cbind(lts, lts + noise) #and here we add the noise
			
			lts = data.frame(lts) #make matrix more human readable
			colnames(lts) <-
				c(paste0(c("A", "I"), rep(c(1:8), each = 2), "orig"), paste0(c("A", "I"), rep(c(1:8), each =
																																												2)))
			lts$bb = paste0("bb", 1:2000)
			lts$rep_name = paste0("age",
														rep(1:5, each = nrep * nkid),
														"_rep",
														rep(1:nrep, each = nkid))
			lts$age = rep(1:5, each = nrep * nkid)
			
			#NA trials below 5s
			ltcols = paste0(c("A", "I"), rep(c(1:8), each = 2))
			for (thiscol in ltcols)
				lts[lts[, thiscol] < 5, thiscol] <- NA
			
			#add LTs taking into account paired trials - not the most legible code ever
			lts = cbind(lts[, ltcols], lts)
			colnames(lts)[1:16] <- paste0(names(lts)[1:16], "p")
			for (thispair in 1:8)
				lts[rowSums(is.na(lts[, paste0(c("A", "I"), thispair, "p")])) > 0, #if there is even one NA in this pair
						paste0(c("A", "I"), thispair, "p")] <-
				NA #NA both columns for this pair
			
			lts$mean.ads <- rowMeans(lts[, paste0("A", c(1:8))], na.rm = T)
			lts$mean.ids <- rowMeans(lts[, paste0("I", c(1:8))], na.rm = T)
			lts$mean.dif <- lts$mean.ids - lts$mean.ads
			
			lts$mean.paired.ads <- rowMeans(lts[, paste0("A", c(1:8), "p")], na.rm =
																				T)
			lts$mean.paired.ids <- rowMeans(lts[, paste0("I", c(1:8), "p")], na.rm =
																				T)
			lts$mean.paired.dif <- lts$mean.paired.ids - lts$mean.paired.ads
			
			return(lts)
		}
	
	redosim <-
		reactive(
			dosim(
				input$factorNoise,
				input$ageg_decrease,
				input$trialtype_increase,
				input$order_decrease
			)
		)
	
	output$noisePlot <- renderPlot({
		lts <- redosim()
		mycols = gray.colors(length(levels(factor(lts$age))), start = 0, end = .5)
		
		layout(matrix(1:2),1,.8)
		par(mar=c(1,4,1,1))
		
		plot(lts[, "A1"] ~ lts[, "A1orig"],	pch = 20, col = mycols[lts$age],
			main = "Trial A1 for all infants",
			xlab = "",xaxt="n", xlim=range(lts[, c("A1orig","A1","A8orig","A8")],na.rm=T),
			ylab = "LT after noising & exclusions"	, ylim=range(lts[, c("A1orig","A1","A8orig","A8")],na.rm=T)	)
		text(min(lts[, c("A1orig","A1")],na.rm=T),
				 max(lts[, c("A1orig","A1")],na.rm=T) * seq(.6, 1, .1),
				 paste("age", 1:5),
				 col = mycols)

		par(mar=c(4,4,1,1))
		plot(lts[, "A8"] ~ lts[, "A8orig"],	pch = 20, col = mycols[lts$age],
				 main = "Trial A8 for all infants",
				 xlab = "Original LT", xlim=range(lts[, c("A1orig","A1","A8orig","A8")],na.rm=T),
				 ylab = "LT after noising & exclusions"	, ylim=range(lts[, c("A1orig","A1","A8orig","A8")],na.rm=T)	)
		
	})
	
	output$dataLossPlot <- renderPlot({
		lts <- redosim()

		selcols = c(paste0(c("A", "I"), rep(c(1:8), each = 2)))
		natrialsum = aggregate(is.na(lts[, c(selcols)]), by = list(factor(lts$age)), sum)
		totalNtrials=16*20*20 #16 trials * 20 infants * 20 replications per age 

		mycols = gray.colors(length(levels(factor(lts$age))), start = 0, end = .5)
		names(mycols)<-levels(factor(natrialsum$Group.1))
		plot(		1,			type = "n",
			xlab = "Trial pair",xlim = c(1, 8),
			ylab = "Proportion trials lost", ylim = range(natrialsum[, -1]/totalNtrials),
			main="Trials lost per trial pair collapsing over all replications at a given age"
		)
		for (i in levels(factor(natrialsum$Group.1))) {
			points(
	(as.numeric(as.character(natrialsum[natrialsum$Group.1 == i, paste0("A", 1:8)]))/totalNtrials) ~ c((1:8)-.1),
						 pch = "a",	 col = mycols[i])
			points(
	(as.numeric(as.character(natrialsum[natrialsum$Group.1 == i, paste0("I", 1:8)]))/totalNtrials) ~ c((1:8)+.1),
						 pch = "i",	 col = mycols[i])
		}
		text(rep(1, 5),
				 max(natrialsum[, -1]/totalNtrials) * seq(.6, 1, .1),
				 paste("age", 1:5),
				 col = mycols)
		
	})
	
	output$difplot <- renderPlot({
		lts <- redosim()
		
		mycols = gray.colors(length(levels(factor(lts$age))), start = 0, end = .5)
		
		plot(lts$mean.dif ~ lts$mean.paired.dif,
			xlim = range(c(lts$mean.dif, lts$mean.paired.dif),na.rm=T),
			ylim = range(c(lts$mean.dif, lts$mean.paired.dif),na.rm=T),
			xlab = "Difference LT from paired trials",
			ylab = "Difference LT from ALL trials",
			pch = 20,
			col = mycols[lts$age],
			main="Difference scores (individual infant's data)"
		)
		lines(c(-50, max(c(lts$mean.dif, lts$mean.paired.dif),na.rm=T)), c(-50, max(c(lts$mean.dif, lts$mean.paired.dif),na.rm=T)), lty =
						2)
		
		abline(lm(lts$mean.dif ~ lts$mean.paired.dif),col="blue")
		
		text(
			rep(min(c(lts$mean.dif, lts$mean.paired.dif),na.rm=T), 5),
			max(c(lts$mean.dif, lts$mean.paired.dif),na.rm=T) * seq(.6, 1, .1),
			paste("age", 1:5),
			col = mycols
		)
		
	})
	
	
	output$esplot <- renderPlot({
		lts <- redosim()
		
		unpairedES = aggregate(
			lts$mean.dif,
			by = list(lts$rep_name),
			FUN = function(x)
				mean(x, na.rm = T) / sd(x, na.rm = T)
		)
		pairedES = aggregate(
			lts$mean.paired.dif,
			by = list(lts$rep_name),
			FUN = function(x)
				mean(x, na.rm = T) / sd(x, na.rm = T)
		)
		unpairedES$age = as.numeric(as.character(gsub(
			"age", "", gsub("_rep.*", "", unpairedES$Group.1)
		)))
		pairedES$age = as.numeric(as.character(gsub(
			"age", "", gsub("_rep.*", "", pairedES$Group.1)
		)))
		
		mycols = gray.colors(length(levels(factor(pairedES$age))), start = 0, end = .5)
		
		plot(
			unpairedES$x ~ pairedES$x,
			xlim = range(unpairedES$x, pairedES$x),
			ylim = range(unpairedES$x, pairedES$x),
			xlab = "Paired ES",
			ylab = "Unpaired ES",
			pch = 20,
			col = mycols[pairedES$age],
			main="Effect size (collapsing across infants in a given replication)"
		)
		lines(c(0, max(unpairedES$x, pairedES$x)), c(0, max(unpairedES$x, pairedES$x)), lty =
						2)
		abline(lm(unpairedES$x ~ pairedES$x),col="blue")
		
		text(
			rep(min(unpairedES$x, pairedES$x), 5),
			max(unpairedES$x, pairedES$x) * seq(.6, 1, .1),
			paste("age", 1:5),
			col = mycols
		)
		
	})
	
	
})

# Run the application
shinyApp(ui = ui, server = server)
