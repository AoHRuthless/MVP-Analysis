# Basketball Data
# 
# Author: Anand
###############################################################################
# Load necessary packages
install.packages("RCurl")
install.packages("XML")
install.packages("scrapeR")
install.packages("arm")
# Devtools and Rcpp need to both be installed and loaded to load rmongodb
install.packages("devtools")
install.packages("Rcpp")
library(devtools)
library(Rcpp)

install_github(repo ="mongosoup/rmongodb")

library(rmongodb)
library(RCurl)
library(XML)
library(scrapeR)
library(arm)

m <- mongo.create()
ns <- "player.stats"

# Grab mvp data from webpage
mvps <- getURL("http://www.nba.com/history/nba-mvp-award-winners/")
mvps <- readLines(tc <- textConnection(mvps))

mvptree <- htmlTreeParse(mvps, useInternalNodes = TRUE)

mvpEvens <- unlist(xpathApply(mvptree, "//*/tr[@class='cnnIERowAltBG']/td", xmlValue))
mvpOdds  <- unlist(xpathApply(mvptree, "//*/tr[@class='']/td", xmlValue))

# Parse the data of mvps into a readable data frame
# Args: recency, the number of years to include, up to 60
# Returns a data frame
mvpData <- function(recency) {
	season <- c()
	player <- c()
	team <- c()
	for (i in 1:length(mvpEvens)) {
		if (i > recency * 3 / 2) {
			break;
		}
		value <- trim(mvpEvens[i])
		valueAlt <- trim(mvpOdds[i])
		rem <- i %% 3
		if (rem == 0) {
			team <- c(team, value)
			team <- c(team, valueAlt)
		} else if (rem == 1) {
			season <- c(season, value)
			season <- c(season, valueAlt)
		} else {
			player <- c(player, value)
			player <- c(player, valueAlt)
		}
	}
	return(data.frame(player, team, season))
}

# Calculates the mvp for a given year
# Args: year an integer
# Returns the mvp for the year
mvp <- function(year) {
	data <- mvpData(60)
	for (i in 1:60) {
		if (equalYears(data[i, 3], year)) {
			return(data[i, 1])
		}
	}
	return(NULL)
}

# Fills the mongo database for the player stats for a given year
# Args: year an integer
# Generates the stats as a dataframe then fills the database
fillDb <- function(year) {
	df <- generateStats(year)
	
	for (i in 1:nrow(df)) {
		list <- list(player=df[i,1], pos=df[i,2], age=df[i,3], team=df[i,4], games=df[i,5], min_played=df[i,6], player_eff=df[i,7],
				true_shooting=df[i,8], three_pt_att_r=df[i,9], free_throw_att_r=df[i,10], orb_pct=df[i,11], drb_pct=df[i,12],
				trb_pct=df[i,13], assist_pct=df[i,14], steal_pct=df[i,15], block_pct=df[i,16], turnover_pct=df[i,17],
				usage_pct=df[i,18], off_ws=df[i,19], def_ws=df[i,20], ws=df[i,21], ws_per_48=df[i,22], obpm=df[i,23], dbpm=df[i,24],
				bpm=df[i,25],val=df[i,26])
		bson <- mongo.bson.from.list(list)
		mongo.insert(m, ns, bson)
	}
	return(df)
}

# Generates a data frame of the per-player stats for a given year
# Args: year an integer
# Returns a data frame of all the relevant player statistics
generateStats <- function(year) {
	stats <- getURL(paste("http://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html", sep=""))
	stats <- readLines(tc <- textConnection(stats))
	
	tree <- htmlTreeParse(stats, useInternalNodes = TRUE)
	
	data <- unlist(xpathApply(tree, "//*/tr[@class='full_table']/td", xmlValue))
	
	player <- c()
	pos <- c()
	age <- c()
	team <- c()
	games <- c()
	min_played <- c()
	player_eff <- c()
	true_shooting <- c()
	three_pt_att_r <- c()
	free_throw_att_r <- c()
	orb_pct <- c()
	drb_pct <- c()
	trb_pct <- c()
	assist_pct <- c()
	steal_pct <- c()
	block_pct <- c()
	turnover_pct <- c()
	usage_pct <- c()
	off_ws <- c()
	def_ws <- c()
	ws <- c()
	ws_per_48 <- c()
	obpm <- c()
	dbpm <- c()
	bpm <- c()
	val <- c()
	
	for (i in 1:length(data)) {
		rem <- i %% 28
		value <- trim(data[i])
		if (rem == 0) {
			val <- c(val, value)
		}
		else if (rem == 1) {
			player <- c(player, value)
		}
		else if (rem == 2) {
			pos <- c(pos, value)
		}
		else if (rem == 3) {
			age <- c(age, value)
		}
		else if (rem == 4) {
			team <- c(team, value)
		}
		else if (rem == 5) {
			games <- c(games, value)
		}
		else if (rem == 6) {
			min_played <- c(min_played, value)
		}
		else if (rem == 7) {
			player_eff <- c(player_eff, value)
		}
		else if (rem == 8) {
			true_shooting <- c(true_shooting, value)
		}
		else if (rem == 9) {
			three_pt_att_r <- c(three_pt_att_r, value)
		}
		else if (rem == 10) {
			free_throw_att_r <- c(free_throw_att_r, value)
		}
		else if (rem == 11) {
			orb_pct <- c(orb_pct, value)
		}
		else if (rem == 12) {
			drb_pct <- c(drb_pct, value)
		}
		else if (rem == 13) {
			trb_pct <- c(trb_pct, value)
		}
		else if (rem == 14) {
			assist_pct <- c(assist_pct, value)
		}
		else if (rem == 15) {
			steal_pct <- c(steal_pct, value)
		}
		else if (rem == 16) {
			block_pct <- c(block_pct, value)
		}
		else if (rem == 17) {
			turnover_pct <- c(turnover_pct, value)
		}
		else if (rem == 18) {
			usage_pct <- c(usage_pct, value)
		}
		else if (rem == 20) {
			off_ws <- c(off_ws, value)
		}
		else if (rem == 21) {
			def_ws <- c(def_ws, value)
		}
		else if (rem == 22) {
			ws <- c(ws, value)
		}
		else if (rem == 23) {
			ws_per_48 <- c(ws_per_48, value)
		}
		else if (rem == 25) {
			obpm <- c(obpm, value)
		}
		else if (rem == 26) {
			dbpm <- c(dbpm, value)
		}
		else if (rem == 27) {
			bpm <- c(bpm, value)
		}
	}
	result <- data.frame(player, pos, age, team, games, min_played, player_eff, true_shooting, three_pt_att_r, 
			free_throw_att_r, orb_pct, drb_pct, trb_pct, assist_pct, steal_pct, block_pct, 
			turnover_pct, usage_pct, off_ws, def_ws, ws, ws_per_48, obpm, dbpm, bpm, val)
	result$player <- as.character(result$player)
	result$pos <- as.character(result$pos)
	result$team <- as.character(result$team)
	
	result$age <- as.numeric(as.character(result$age))
	result$games <- as.numeric(as.character(result$games))
	result$min_played <- as.numeric(as.character(result$min_played))
	result$player_eff <- as.numeric(as.character(result$player_eff))
	result$true_shooting <- as.numeric(as.character(result$true_shooting))
	result$three_pt_att_r <- as.numeric(as.character(result$three_pt_att_r))
	result$free_throw_att_r <- as.numeric(as.character(result$free_throw_att_r))
	result$orb_pct <- as.numeric(as.character(result$orb_pct))
	result$drb_pct <- as.numeric(as.character(result$drb_pct))
	result$trb_pct <- as.numeric(as.character(result$trb_pct))
	result$assist_pct <- as.numeric(as.character(result$assist_pct))
	result$steal_pct <- as.numeric(as.character(result$steal_pct))
	result$block_pct <- as.numeric(as.character(result$block_pct))
	result$turnover_pct <- as.numeric(as.character(result$turnover_pct))
	result$usage_pct <- as.numeric(as.character(result$usage_pct))
	result$off_ws <- as.numeric(as.character(result$off_ws))
	result$def_ws <- as.numeric(as.character(result$def_ws))
	result$ws <- as.numeric(as.character(result$ws))
	result$ws_per_48 <- as.numeric(as.character(result$ws_per_48))
	result$obpm <- as.numeric(as.character(result$obpm))
	result$dbpm <- as.numeric(as.character(result$dbpm))
	result$bpm <- as.numeric(as.character(result$bpm))
	result$val <- as.numeric(as.character(result$val))
	
	return(result)
}


# Creates and returns a bayesian generalized linear model (glm) of the season stats for a given year from the database
# Args: Integer the given year
# Returns: A bayesglm model
createModel <- function(year) {
	df <- fillDb(year)
	v <- c()
	for (i in 1:nrow(df)) {
		p <- as.character(df[i, 1])
		value <- mongo.find.one(m, ns, list(player=p))
		listval <- mongo.bson.to.list(value)
		tmp <- as.vector(unlist(listval)[c(2:27)])
		v <- rbind(v, c(tmp, ifelse(mvp(year) == p, 1, 0)))
	}
	header <- c("player", "pos", "age", "team", "games", "min_played", "player_eff", "true_shooting", "three_pt_att_r", "free_throw_att_r", 
			"orb_pct", "drb_pct", "trb_pct", "assist_pct", "steal_pct", "block_pct", "turnover_pct", "usage_pct", "off_ws", "def_ws", "ws", 
			"ws_per_48", "obpm", "dbpm", "bpm", "val", "mvp")
	result <- data.frame(v)
	names(result) <- header
	
	result$player <- as.character(result$player)
	result$pos <- as.character(result$pos)
	result$team <- as.character(result$team)
	
	result$age <- as.numeric(as.character(result$age))
	result$games <- as.numeric(as.character(result$games))
	result$min_played <- as.numeric(as.character(result$min_played))
	result$player_eff <- as.numeric(as.character(result$player_eff))
	result$true_shooting <- as.numeric(as.character(result$true_shooting))
	result$three_pt_att_r <- as.numeric(as.character(result$three_pt_att_r))
	result$free_throw_att_r <- as.numeric(as.character(result$free_throw_att_r))
	result$orb_pct <- as.numeric(as.character(result$orb_pct))
	result$drb_pct <- as.numeric(as.character(result$drb_pct))
	result$trb_pct <- as.numeric(as.character(result$trb_pct))
	result$assist_pct <- as.numeric(as.character(result$assist_pct))
	result$steal_pct <- as.numeric(as.character(result$steal_pct))
	result$block_pct <- as.numeric(as.character(result$block_pct))
	result$turnover_pct <- as.numeric(as.character(result$turnover_pct))
	result$usage_pct <- as.numeric(as.character(result$usage_pct))
	result$off_ws <- as.numeric(as.character(result$off_ws))
	result$def_ws <- as.numeric(as.character(result$def_ws))
	result$ws <- as.numeric(as.character(result$ws))
	result$ws_per_48 <- as.numeric(as.character(result$ws_per_48))
	result$obpm <- as.numeric(as.character(result$obpm))
	result$dbpm <- as.numeric(as.character(result$dbpm))
	result$bpm <- as.numeric(as.character(result$bpm))
	result$val <- as.numeric(as.character(result$val))
	
	model <- bayesglm(mvp ~age+games+min_played+
	                player_eff+true_shooting+three_pt_att_r+
	                free_throw_att_r+orb_pct+drb_pct+trb_pct+assist_pct+
	                steal_pct+block_pct+turnover_pct+usage_pct+off_ws+
	                def_ws+ws+ws_per_48+obpm+dbpm+bpm+val, 
			family=binomial(link='logit'), 
			data=result)
	return(model)
}


# Removes trailing whitespace around a given string
# Args: String
# Returns a trimmed string
trim <- function(str) {
	return(gsub("^\\s+|\\s+$", "", str))
}

# Checks if the year of the given season is the same as the given year
# Season must be in the format YYYY-YY and year must be in the format YYYY
# Args: String String
# Returns true if the season and year are equal
equalYears <- function(season, year) {
	substring(season, 6, 7) == substring(year, 3, 4)
}

# Data Evaluation

model <- createModel(2016)

# Prints the summary of the model
display(model)

## 					coef.est coef.se
## (Intercept)      -8.70    13.58  
## age              -0.06     0.26  
## games             0.00     0.05  
## min_played        0.00     0.00  
## player_eff       -0.02     0.22  
## true_shooting     1.05    15.36  
## three_pt_att_r    1.87     5.25  
## free_throw_att_r -4.81     7.50  
## orb_pct          -0.03     0.28  
## drb_pct          -0.07     0.18  
## trb_pct          -0.08     0.24  
## assist_pct        0.06     0.10  
## steal_pct         0.01     1.10  
## block_pct        -0.08     0.69  
## turnover_pct      0.07     0.22  
## usage_pct         0.02     0.20  
## off_ws           -0.09     0.48  
## def_ws            0.19     0.85  
## ws               -0.01     0.37  
## ws_per_48        -0.53    15.61  
## obpm              0.08     0.40  
## dbpm             -0.06     0.56  
## bpm               0.04     0.34  
## val               1.01     1.19  
## ---
##         n = 475, k = 24
## residual deviance = 2.5, null deviance = 14.3 (difference = 11.8)


training <- generateStats(2015)
fittedTraining <- predict(model, training, type = 'response')
fittedTraining <- sort(fittedTraining, decreasing = T)[1:7]

test <- generateStats(2017)
fittedTest <- predict(model, test, type = 'response')
fittedTest <- sort(fittedTest, decreasing = T)[1:7]

## print(fittedTraining)
## 112        470        361        197        283        462        235 
## 0.72144267 0.27155854 0.26685687 0.15479778 0.05087575 0.03666147 0.03528111 

## print(training[c(112, 470, 361, 197, 283, 462, 235), 1])
## [1] "Stephen Curry"     "Russell Westbrook" "Chris Paul"       
## [4] "James Harden"      "Damian Lillard"    "John Wall"        
## [7] "LeBron James"     

## print(fittedTest)
## 458        172         98        220        351        452        424 
## 0.97903245 0.56815518 0.22376577 0.07850055 0.05485946 0.03601565 0.02596194 

## print(test[c(458, 172, 98, 220, 351, 452, 424), 1])
## [1] "Russell Westbrook" "James Harden"      "Stephen Curry"    
## [4] "LeBron James"      "Chris Paul"        "John Wall"        
## [7] "Isaiah Thomas"    