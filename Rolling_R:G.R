library(dplyr)
library(tidyverse)
library(ggplot2)

rolling_averages = read.csv(file.choose())

teams = c("BAL", "TBR", "BOS", "NYY", "TOR", "MIN", "DET", "CHW", "KCR", "CLE",
         "LAA", "OAK", "TEX", "HOU", "SEA", "LAD", "SFG", "ARI", "COL", "SDP",
         "MIL", "STL", "CHC", "CIN", "PIT", "NYM", "WSN", "ATL", "PHI", "MIA")

runs_game = c(4.1, 5.3, 5.1, 4.4, 5.2, 4.5, 4.3, 4.9, 4.2, 4.4, 
              4.5, 4.6, 3.9, 5.3, 4.3, 5.1, 5.0, 4.2, 4.6, 4.5,
              4.6, 4.4, 4.4, 4.9, 3.8, 3.9, 4.5, 4.9, 4.5, 3.8)

average_runs = data.frame(Team = teams,
                          R.G = runs_game)

index = c(1:148)

ggplot(rolling_averages, aes(index, rolling_averages$BAL)) +
  geom_line() +
  xlab("Games") +
  ylab("Rolling Avg: Runs Per Game")+
  ggtitle("Baltimore Orioles 2021, Rolling-Average: Runs per Game") +
  geom_hline(yintercept = average_runs$R.G[1])


ggplot(rolling_averages, aes(index, LAD)) +
  geom_line() +
  xlab("Games") +
  ylab("Rolling Avg: Runs Per Game")+
  ggtitle("Los Angeles Dodgers 2021, Rolling-Average: Runs per Game") +
  geom_hline(yintercept = average_runs$R.G[16])


### Rolling Averages for Pitching-----------------------------------------------
ra_per_game = c(5.9, 4.0, 4.6, 4.1, 4.1, 5.1, 4.7, 3.9, 4.9, 4.5, 
                5.0, 4.2, 5.0, 4.1, 4.6, 3.5, 3.7, 5.5, 4.9, 4.4, 
                3.8, 4.1, 5.2, 4.7, 5.1, 4.1, 5.1, 4.1, 4.6, 4.3)

average_runs_allowed = data.frame(Team = teams,
                          RA.G = ra_per_game)

rolling_avg_RA = read.csv(file.choose())

ggplot(rolling_avg_RA, aes(index, LAD)) +
  geom_line() +
  xlab("Games") +
  ylab("Rolling Avg: Runs Allowed Per Game")+
  ggtitle("Los Angeles Dodgers 2021, Rolling-Average: Runs Allowed per Game") +
  geom_hline(yintercept = average_runs_allowed$RA.G[16])



dodgers = data.frame(i = c(1:148),
                     x = rolling_averages$LAD,
                     y= rolling_avg_RA$LAD)

orioles = data.frame(i = c(1:148),
                     x = rolling_averages$BAL,
                     y= rolling_avg_RA$BAL)

braves = data.frame(i = c(1:148),
                     x = rolling_averages$ATL,
                     y= rolling_avg_RA$ATL)

ggplot(dodgers, aes(x = i)) + 
  geom_line(aes(y = x, color = "Runs Scored")) + 
  geom_line(aes(y = y, color = "Runs Allowed")) +
  geom_hline(yintercept = average_runs$R.G[16], col = 4) + 
  geom_hline(yintercept = average_runs_allowed$RA.G[16], col = 2) +
  xlab("Games") +
  ylab("Runs Scored & Runs Allowed Per Game")+
  ggtitle("Dodgers 2021, Rolling-Averages: Runs Score & Runs Allowed per Game")

ggplot(orioles, aes(x = i)) + 
  geom_line(aes(y = x, color = "Runs Scored")) + 
  geom_line(aes(y = y, color = "Runs Allowed")) +
  geom_hline(yintercept = average_runs$R.G[1], col = 4) + 
  geom_hline(yintercept = average_runs_allowed$RA.G[1], col = 2) +
  xlab("Games") +
  ylab("Runs Scored & Runs Allowed Per Game")+
  ggtitle("Orioles 2021, Rolling-Averages: Runs Score & Runs Allowed per Game")

ggplot(braves, aes(x = i)) + 
  geom_line(aes(y = x, color = "Runs Scored")) + 
  geom_line(aes(y = y, color = "Runs Allowed")) +
  geom_hline(yintercept = average_runs$R.G[28], col = 4) + 
  geom_hline(yintercept = average_runs_allowed$RA.G[28], col = 2) +
  xlab("Games") +
  ylab("Runs Scored & Runs Allowed Per Game")+
  ggtitle("Braves 2021, Rolling-Averages: Runs Score & Runs Allowed per Game")




r_avg_runs_plot<- data.frame(x = 1:nrow(rolling_averages),                            
                         y = c(rolling_averages$BAL,rolling_averages$TBR,
                               rolling_averages$BOS,rolling_averages$NYY,
                               rolling_averages$TOR,rolling_averages$MIN,
                               rolling_averages$DET,rolling_averages$CHW,
                               rolling_averages$KCR,rolling_averages$CLE,
                               rolling_averages$LAA,rolling_averages$OAK,
                               rolling_averages$TEX,rolling_averages$HOU,
                               rolling_averages$SEA,rolling_averages$LAD,
                               rolling_averages$SFG,rolling_averages$ARI,
                               rolling_averages$COL,rolling_averages$SDP,
                               rolling_averages$MIL,rolling_averages$STL,
                               rolling_averages$CHC,rolling_averages$CIN,
                               rolling_averages$PIT,rolling_averages$NYM,
                               rolling_averages$WSN,rolling_averages$ATL,
                               rolling_averages$PHI,rolling_averages$MIA),
                         group = c(rep("BAL", nrow(rolling_averages)), rep("TBR", nrow(rolling_averages)),
                                   rep("BOS", nrow(rolling_averages)), rep("NYY", nrow(rolling_averages)),
                                   rep("TOR", nrow(rolling_averages)), rep("MIN", nrow(rolling_averages)),
                                   rep("DET", nrow(rolling_averages)), rep("CHW", nrow(rolling_averages)),
                                   rep("KCR", nrow(rolling_averages)), rep("CLE", nrow(rolling_averages)),
                                   rep("LAA", nrow(rolling_averages)), rep("OAK", nrow(rolling_averages)),
                                   rep("TEX", nrow(rolling_averages)), rep("HOU", nrow(rolling_averages)),
                                   rep("SEA", nrow(rolling_averages)), rep("LAD", nrow(rolling_averages)),
                                   rep("SFG", nrow(rolling_averages)), rep("ARI", nrow(rolling_averages)),
                                   rep("COL", nrow(rolling_averages)), rep("SDP", nrow(rolling_averages)),
                                   rep("MIL", nrow(rolling_averages)), rep("STL", nrow(rolling_averages)),
                                   rep("CHC", nrow(rolling_averages)), rep("CIN", nrow(rolling_averages)),
                                   rep("PIT", nrow(rolling_averages)), rep("NYM", nrow(rolling_averages)),
                                   rep("WSN", nrow(rolling_averages)), rep("ATL", nrow(rolling_averages)),
                                   rep("PHI", nrow(rolling_averages)), rep("MIA", nrow(rolling_averages))))

r_avg_runs_allowed_plot<- data.frame(x = 1:nrow(rolling_avg_RA),                            
                             y = c(rolling_avg_RA$BAL,rolling_avg_RA$TBR,
                                   rolling_avg_RA$BOS,rolling_avg_RA$NYY,
                                   rolling_avg_RA$TOR,rolling_avg_RA$MIN,
                                   rolling_avg_RA$DET,rolling_avg_RA$CHW,
                                   rolling_avg_RA$KCR,rolling_avg_RA$CLE,
                                   rolling_avg_RA$LAA,rolling_avg_RA$OAK,
                                   rolling_avg_RA$TEX,rolling_avg_RA$HOU,
                                   rolling_avg_RA$SEA,rolling_avg_RA$LAD,
                                   rolling_avg_RA$SFG,rolling_avg_RA$ARI,
                                   rolling_avg_RA$COL,rolling_avg_RA$SDP,
                                   rolling_avg_RA$MIL,rolling_avg_RA$STL,
                                   rolling_avg_RA$CHC,rolling_avg_RA$CIN,
                                   rolling_avg_RA$PIT,rolling_avg_RA$NYM,
                                   rolling_avg_RA$WSN,rolling_avg_RA$ATL,
                                   rolling_avg_RA$PHI,rolling_avg_RA$MIA),
                             group = c(rep("BAL", nrow(rolling_avg_RA)), rep("TBR", nrow(rolling_avg_RA)),
                                       rep("BOS", nrow(rolling_avg_RA)), rep("NYY", nrow(rolling_avg_RA)),
                                       rep("TOR", nrow(rolling_avg_RA)), rep("MIN", nrow(rolling_avg_RA)),
                                       rep("DET", nrow(rolling_avg_RA)), rep("CHW", nrow(rolling_avg_RA)),
                                       rep("KCR", nrow(rolling_avg_RA)), rep("CLE", nrow(rolling_avg_RA)),
                                       rep("LAA", nrow(rolling_avg_RA)), rep("OAK", nrow(rolling_avg_RA)),
                                       rep("TEX", nrow(rolling_avg_RA)), rep("HOU", nrow(rolling_avg_RA)),
                                       rep("SEA", nrow(rolling_avg_RA)), rep("LAD", nrow(rolling_avg_RA)),
                                       rep("SFG", nrow(rolling_avg_RA)), rep("ARI", nrow(rolling_avg_RA)),
                                       rep("COL", nrow(rolling_avg_RA)), rep("SDP", nrow(rolling_avg_RA)),
                                       rep("MIL", nrow(rolling_avg_RA)), rep("STL", nrow(rolling_avg_RA)),
                                       rep("CHC", nrow(rolling_avg_RA)), rep("CIN", nrow(rolling_avg_RA)),
                                       rep("PIT", nrow(rolling_avg_RA)), rep("NYM", nrow(rolling_avg_RA)),
                                       rep("WSN", nrow(rolling_avg_RA)), rep("ATL", nrow(rolling_avg_RA)),
                                       rep("PHI", nrow(rolling_avg_RA)), rep("MIA", nrow(rolling_avg_RA))))


split_r_avg_runs = split(r_avg_runs_plot, r_avg_runs_plot$group)

split_r_avg_runs_allowed = split(r_avg_runs_allowed_plot, r_avg_runs_allowed_plot$group)

averages = data.frame(Team = average_runs$Team,
                      R.G = average_runs$R.G,
                      RA.G = average_runs_allowed$RA.G)


proportions = data.frame(Type = c("R.G", "RA.G"))

for (team in teams) {
  test = data.frame(i = c(1:148),
                    R.G = split_r_avg_runs[[team]]$y,
                    RA.G = split_r_avg_runs_allowed[[team]]$y)
  
  h_line1 = averages$R.G[which(averages$Team == team)]
  h_line2 = averages$RA.G[which(averages$Team == team)]
  
  
  if(h_line1 >= h_line2) {
    plot = ggplot(test, aes(x = i)) + 
      geom_line(aes(y = R.G, color = "Runs Scored")) + 
      geom_line(aes(y = RA.G, color = "Runs Allowed")) +
      geom_hline(aes(yintercept = h_line1), col = 4) +
      geom_text(aes(0,h_line1,label = h_line1, vjust = -0.5)) +
      geom_hline(aes(yintercept = h_line2), col = 2) +
      geom_text(aes(0,h_line2,label = h_line2, vjust = 1.5)) +
      xlab("Games") +
      ylab("Runs Scored & Runs Allowed Per Game")+
      ggtitle(paste(team, "Rolling-Averages: Runs Score & Runs Allowed per Game"))
  } else {
    plot = ggplot(test, aes(x = i)) + 
      geom_line(aes(y = R.G, color = "Runs Scored")) + 
      geom_line(aes(y = RA.G, color = "Runs Allowed")) +
      geom_hline(aes(yintercept = h_line1), col = 4) +
      geom_text(aes(0,h_line1,label = h_line1, vjust = 1.5)) +
      geom_hline(aes(yintercept = h_line2), col = 2) +
      geom_text(aes(0,h_line2,label = h_line2, vjust = -0.5)) +
      xlab("Games") +
      ylab("Runs Scored & Runs Allowed Per Game")+
      ggtitle(paste(team, "Rolling-Averages: Runs Score & Runs Allowed per Game"))
  }

  print(plot)
  
######--------------------------------------------------------------------------
  
  above_average = 0
  for (i in test$R.G) {
    above_average = above_average + ifelse(i >= h_line1 , 1, 0)
  }
  
  above_r_avg_runs = round((above_average / nrow(orioles)) * 100, 1)
  above_r_avg_runs
  
######
  
  below_average = 0
  for (i in test$RA.G) {
    below_average = below_average + ifelse(i <=  h_line2, 1, 0)
  }
  
  below_r_avg_ra = round((below_average / nrow(orioles)) * 100, 1)
  below_r_avg_ra
  
  proportions[team] = c(above_r_avg_runs, below_r_avg_ra)
}


