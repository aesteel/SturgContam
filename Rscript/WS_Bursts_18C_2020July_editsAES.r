
library(tidyverse)


#setwd("/Users/Anna/Google Drive/2020 GS Multiple Stressors/2020Expts/Expt Docs - Range Test 2020 WS/Burst")
# run this from SturgContam.Rproj in Users/Anna/Documents/ResearchGit/SturgContam


## Build Segment Dataset (all possible combs of inter-gate distances)----
tunnel_specs <- read_csv("rawData/Small_Burst_Tunnel_Specs_Spring_2020_KZ.csv")

segment_dat <- tribble(  ~GATE_A, ~GATE_B, ~START, ~END  )
seg_length = nrow(tunnel_specs)

   # Creates a large tibble with all possible permutations of gates 
    for (p in 1:seg_length){
      for (q in 1:seg_length){
        segment_dat <- segment_dat %>% 
          add_row(GATE_A = tunnel_specs$GATE_ID[p], GATE_B = tunnel_specs$GATE_ID[q], 
                  START = tunnel_specs$GATE_DISTANCES[p], 
                  END = tunnel_specs$GATE_DISTANCES[q])
      }
    }
    remove(p,q)

    #  Clean the segment dataframe
    segment_dat_1 <- segment_dat %>% 
      mutate(DIFF = END-START) %>% # calculates the difference between two gates
      mutate(MIDPOINT = (START+END)/2) %>% # calculates the midpoint between two gates
      filter(DIFF > 0) # get rid of combos of the same gate
      
    
  
# pull metadata for index to all trials and metadata
trial_specs <- read.csv("rawData/Burst_Metadata_WS2020.csv")

filestart <- unique(trial_specs$raspPI_filestart)

# create empty dataframe to recoding final mean burst velocity of the 'good' trials, as well as sd and N good trials considered
mean.topvels = data.frame(treatmentrep = filestart, 
                          mnburstGood = NA, sdburstGood = NA, nburstGood = NA)

# create empty data frame to record gates with fastest speeds for each burst for each fish

fastPos.comp = data.frame(treatmentrep = "EMPTY", burstID = NA, burstQual = NA, fastVel = NA, fastGate = NA, fastPos = NA)






### loop through all burst trials and fill in meantopvels as well as write out 'pdf of burst speeds over multiple tunnel segments

# first define segment lengths to use in summarizing data
min_distance = 1.5        
max_distance = 15    

for(f in 1:length(filestart)) {
 filename = dir("rawData/BurstTrialRaw", pattern=filestart[f])
 
 figureoutputfolder = paste0("figures/Bursts_Outputfigures/",filestart[f])
     if (file.exists(figureoutputfolder) == FALSE) { dir.create(figureoutputfolder)}
  dataoutputfolder = paste0("outputData/Bursts_Outputdata/",filestart[f])
      if (file.exists(dataoutputfolder) == FALSE) { dir.create(dataoutputfolder)}

 
 dat = read.csv(paste0("rawData/BurstTrialRaw/",filename)  )
  dat = dat[1:(nrow(dat)-3),]  
  # removes last three rows with start/end/total time stamps not burst events

 metadat = trial_specs[trial_specs$raspPI_filestart==filestart[f],]
 
 num_burst<-nrow(metadat)
 
 gate_names <- names(dat)[grep("Gate",names(dat))] #subsets all the gate names
 
    ##### Unnecessary code from Ken; replaced with single line above #####
    # num_gates <- length(names(dat)[grep("Gate",names(dat))])  # figure out number of gates from the tunnel specs.
    # first_gate_L <- which(names(dat)=="Gate0")  # finds the first gate in the dataframe
    # first_gate_R <- which(names(dat)==paste0("Gate",(num_gates-1))) # finds the index of the rightmost gate, MAYBE CHANGE THIS DEPENDING ON HOW THE RASPBERRY PI HANDLES LEFT AND RIGHT
    # gate_names <- names(dat[first_gate_L:first_gate_R]) #subsets all the gate names
    #####
 # str(dat)

## filter out non-fish gate trips using metadata, and perhaps later removing 'poor' bursts as well
 
### pseudocode:
 #  for each row in dat
 #    pull data from metadat for corresponding burst (same row index)
 #    translate 'stalledAt' number to GateXX name
 #    remove all data from the columns from GateXX and after
 #   repeat for all rows

 dat2 = data.frame(burstQual=as.character(NA), dat)# to be filled in with the following loop

 for(b in 1:nrow(dat2)) {
  dat2$burstQual[b] <- metadat[b,"burstQual"]
  finalgate = metadat[b,"stalledAt"]
  if(is.na(finalgate)) next
  if(finalgate==25) next
  badcol_index = finalgate+4
  dat2[b,badcol_index:ncol(dat2)] <- NA
 }
 
###  make previous line re-write as -99, then convert to NA in a separate row so I can revisit the dropped ones seperatly from the gates that just didn't trigger
 

### Create an empty list of datafames for each burst in the target trial
burst.df.list <- replicate(num_burst, data.frame()) 
 
# Calulate Metrics for each burst attempt in target trial----
for (i in 1:num_burst){
  assign("temp.dat", dat2[i,],.GlobalEnv)
  temp.dat.gathered <- gather(temp.dat, all_of(gate_names), key = "GATE_ID",value = "TIMING") %>% 
    mutate("POSITION" = tunnel_specs$GATE_DISTANCES) %>% # trust these things line up
    mutate("POSITION_DIFF" = POSITION - lag(POSITION,1)) %>% # Calulates the difference in position between gates
    mutate("TIMING_DIFF" = TIMING - lag(TIMING,1)) %>% #Creates a column which calculates teh differences between two sequential timings
    mutate("VELOCITY" = POSITION_DIFF/TIMING_DIFF) %>% # calculates velocity
    mutate("VELOCITY_DIFF" = VELOCITY - lag(VELOCITY,1)) %>%  # creates column which calculates teh differnce between two sequential speeds
    mutate("ACCEL" = VELOCITY_DIFF/TIMING_DIFF) # calculates accleration
 burst.df.list[[i]]<-temp.dat.gathered #stores them all as dataframes in a list
}

rbind_burst.df.list = do.call(rbind, burst.df.list)

#####write.csv(rbind_burst.df.list, paste0(dataoutputfolder,"/Gate_by_Gate_metrics_",filestart[f],".csv"), row.names=F)  


    
## Calculate metrics for non-sequential gates ---

# min_distance = 1.5        
# max_distance = 15    
veldat_maxXcm = function(burst) {
  #segment_dat_1i <- segment_dat_1 # this is created above as a universal dataframe for all subsequent code create temp 'i' version for this burst 
  vel_dat_2 <- segment_dat_1 %>% ## complete the dataframe with velocities for each burst event
    mutate(burstQual = unique(burst$burstQual)) %>%
    mutate(BURST_NUMBER = unique(burst$BURST_NUMBER)) %>%
    mutate(ORIENTATION = unique(burst$ORIENTATION)) %>%
    mutate(TIMING_A = (burst$TIMING[GATE_A+1])) %>% 
    mutate(TIMING_B = (burst$TIMING[GATE_B+1])) %>% 
    mutate(TIME_DIFF = TIMING_B - TIMING_A) %>% 
    mutate(VELOCITY = DIFF/TIME_DIFF) %>% 
    filter(DIFF >= min_distance & DIFF <= max_distance) %>% # sub sets plotted velocities for only those occurring across distances of 1.5-10 cm
    filter(TIME_DIFF <= 2) %>% # removes erroneous values generated by comparing gates which were triggered way too far apart
    filter(VELOCITY >= 0) ## should this be filtered out elsewhere?
  return(vel_dat_2)
}
  
plot_vel_data = lapply(burst.df.list,  veldat_maxXcm)

## Plot data for each burst

# i = 1
# vel_plot_test <- ggplot(data = plot_vel_data[[i]]) +
#     geom_errorbarh(aes(xmax = END, xmin = START, y = SEG_VELOCITY, height = 0))+
#   geom_point(aes(x=MIDPOINT, y = SEG_VELOCITY), color = "orangered2") + # currently set up to plot velocity, but it could track the other metrics
#     xlim(0,100) +
#     ggtitle(paste("Velocity: Burst",i," (max segment length =",max_distance,")")) + 
#   ylab("Velocity (cm/s)")+
#     xlab("Tunnel Position (cm)")+
#     theme(axis.text.x = element_text(angle=90))
# 
# vel_plot_test
# # could iterate this into pages of a pdf, with each page as a burst, and each file as a fish?


      # pdf(paste0(figureoutputfolder,"/filtered_segmentBursts_",filestart[f],".pdf"), onefile=TRUE, )
      # for(i in 1:15) {
      #   if(nrow(plot_vel_data[[i]]) == 0) {next} else {
      #   print ( ggplot(data = plot_vel_data[[i]]) +
      #     geom_errorbarh(aes(xmax = END, xmin = START, y = SEG_VELOCITY, height = 0))+
      #   geom_point(aes(x=MIDPOINT, y = SEG_VELOCITY), color = "orangered2") + # currently set up to plot velocity, but it could track the other metrics
      #     coord_cartesian(xlim=c(0,100), ylim=c(0,70))+
      #     ggtitle(paste("Velocity: Burst",i," (max segment length =",max_distance,")")) +
      #   ylab("Velocity (cm/s)")+
      #     xlab("Tunnel Position (cm)")+
      #     theme(axis.text.x = element_text(angle=90)) ) }
      # }
      # dev.off()


# ### other exploratory plots from Ken's code ####
#  Timeseries_pos_plot <- ggplot(data = burst.df.list[[i]],
#                            aes(x=TIMING, y = POSITION)) +
#     geom_line(color = "#0273e9")+
#     geom_point() + 
#     ggtitle(paste("Time Series of Position: Burst",i)) +
#     ylab("Tunnel Position (cm)")+
#     xlab("Time (s)")+
#     theme(axis.text.x = element_text(angle=90))
#  
#  
#  Timeseries_vel_plot <- ggplot(data = burst.df.list[[i]],
#                                aes(x=TIMING, y = VELOCITY)) +
#     geom_point() + 
#     geom_line(color = "#3c5c00")+
#     ggtitle(paste("Time Series of Velocities: Burst",i)) +
#     coord_cartesian(ylim = c(0,100)) +
#     ylab("Segment Velocity (cm/s)")+
#     xlab("Time (s)")
#  
#  
#   Timeseries_acc_plot <- ggplot(data = burst.df.list[[i]],
#                                aes(x=TIMING, y = ACCEL)) +
#     geom_point() + 
#     geom_line(color = "#3c5c00")+
#     ggtitle(paste("Time Series of Acceleration: Burst",i)) +
#     #coord_cartesian(ylim = c(0,100)) +
#     ylab("Segment Acceleration (cm/s)")+
#     xlab("Time (s)")
# ####
  
  
# Next up: what metrics to pull? Average of top three velocities per burst, and use fastest one as metric for fish?
  
# consider more filtering before running this, based on 'bad' bursts, and maybe within the trial I'll need to pull out points after experimenter intervenes. Manual and a pain?
  # in the future, if end the trial manually, cut ONLY last laser, and if shepherding, shepherd SLOWLY. Might make the post-processing easier. 
  # at this point it doesn't seem to cause a problem?
  
  
# pull mean of fastest 3 velocities per burst, after dropping fastest two, 
#  and plot by subjective burst quality
  
topvel_func= function(x,n) {
  vel.list = sort(x$VELOCITY, decreasing=TRUE) # runs with either burst.df.list or plot_vel_data 
  n_avg <- n
  if(length(vel.list)<5) {mnvel=NA; n_avg<-0} else {  # if there isn't enough data, change the output in the summary to reflect this
  mnvel = mean(vel.list[3:(n+1)]) }
  return(data.frame(burstID = unique(x$BURST_NUMBER), 
                    mntopvel = mnvel, 
                    n_avg = n_avg,
                    burstQual = unique(x$burstQual)) ) }
                    
### drop fastest (sometimes erroneous) and take mean of next N values (sequential, not overlapping gates)
nn=3
 burst.mntopvels = do.call(rbind, lapply(burst.df.list, topvel_func, n=nn)) # seems to give similar answers; not exhaustively or formally evaluated though. Using this avoids pseudoreplication, and using this may be less ideal if there are gaps in data where lasers are frequently missed. 
 
 # burst.mntopvels = do.call(rbind, lapply(plot_vel_data, topvel_func, n=nn))
 
#####write.csv(burst.mntopvels, paste0(dataoutputfolder,"/Subjective_Rank_Mn_dropTop2_avgNext",nn,"_",filestart[f],".csv"), row.names=F)

 # returns NA if there are =< n velocity values measured
burst.mntopvels$burstID = as.numeric(burst.mntopvels$burstID )
burst.mntopvels$burstQual = factor(burst.mntopvels$burstQual, levels=c("poor","okay","good") )

fastPos_func = function(x) { 
  fast.pos = which(x$VELOCITY > quantile(x$VELOCITY,.75, na.rm=T))
   if(length(fast.pos)==0) {return(NULL)} else {
  fastVel = x$VELOCITY[fast.pos]   
  fastGate = x$GATE_ID[fast.pos]
  fastPos = x$POSITION[fast.pos]
  return(cbind(data.frame(treatmentrep = filestart[f],
                          burstID = unique(x$BURST_NUMBER), 
                          burstQual = unique(x$burstQual)),
               fastVel, fastGate, fastPos) ) } 
  }

fastPos = do.call(rbind, lapply(burst.df.list, fastPos_func))

fastPos.comp = rbind(fastPos.comp, fastPos)


# boxplots of mean top speeds within each subjective rank category
subjective_rank_spds = ggplot( data = burst.mntopvels[!is.na(burst.mntopvels$mntopvel),],
        aes(x=burstQual, y=mntopvel)) +
        geom_boxplot() +
          geom_point()+
        theme_bw()


      # pdf(paste0(figureoutputfolder,"/Subjective_Rank_Mn_dropTop2_avgNext",nn,"_",filestart[f],".tiff",filestart[f],".pdf"), onefile=TRUE)
      # subjective_rank_spds
      # dev.off()


mn.burstvalue = mean(burst.mntopvels$mntopvel[burst.mntopvels$burstQual=="good"], na.rm=T)
sd.burstvalue = sd(burst.mntopvels$mntopvel[burst.mntopvels$burstQual=="good"], na.rm=T)
n.burstvalue = length(burst.mntopvels$mntopvel[burst.mntopvels$burstQual=="good"])

mean.topvels[mean.topvels$treatmentrep == filestart[f], "mnburstGood"] <- mn.burstvalue
mean.topvels[mean.topvels$treatmentrep == filestart[f], "sdburstGood"] <- sd.burstvalue
mean.topvels[mean.topvels$treatmentrep == filestart[f], "nburstGood"] <- n.burstvalue

}


mean.topvels
mean.topvels$treatment = as.numeric(substring(mean.topvels$treatmentrep, 10, 12))
mean.topvels$rep = substring(mean.topvels$treatmentrep, 15, 16)

fastPos.comp2 = fastPos.comp[fastPos.comp$treatmentrep!="EMPTY",]
                             



# look at relationship between estimated speed and number of samples or SD of sample
hist(mean.topvels$nburstGood/15, 
     xlab="Percent of bursts classified as GOOD", 
     main="Data Quality")

ggplot(mean.topvels, aes(x=nburstGood, y=mnburstGood)) + geom_point() + geom_smooth()

ggplot(filter(mean.topvels, nburstGood>4), aes(x=nburstGood, y=mnburstGood)) + 
  geom_point() + geom_smooth()
  # perhaps only considering fish with >4 good bursts will be more reliable?

ggplot(filter(mean.topvels, nburstGood>3), aes(x=nburstGood, y=sdburstGood)) + 
  geom_point() + geom_smooth()

ggplot(mean.topvels, aes(x=mnburstGood, y=sdburstGood)) + 
  geom_point(aes(color=nburstGood), size=3) + geom_smooth()

ggplot(filter(mean.topvels, nburstGood>7), aes(x=mnburstGood, y=sdburstGood)) + 
  geom_point(aes(color=nburstGood), size=3) + geom_smooth()
  # threshold for number as it relates to sd isn't as clear

# look at relationship between burst quality and treatment
ggplot(mean.topvels, aes(x=factor(treatment), y=nburstGood)) + geom_boxplot()

testmod_n = lm(nburstGood~factor(treatment), data = filter(mean.topvels))
  plot(testmod_n)
  summary(aov(testmod_n))
  TukeyHSD(aov(testmod_n))


# look at relatinship between treatment and burst speeds
ggplot(mean.topvels, aes(x=log(treatment), y=mnburstGood)) + geom_point() + geom_smooth()
ggplot(mean.topvels, aes(x=factor(log(treatment)), y=mnburstGood)) + geom_boxplot()

testmod = lm(mnburstGood~factor(treatment), data = filter(mean.topvels, nburstGood>4))
  plot(testmod)
  summary(aov(testmod))
  TukeyHSD(aov(testmod))


  
# look for patterns on where in the tunnel the fastest burst occurred (building new one?)
ggplot(data = fastPos.comp2, aes(x=fastPos)) + 
  geom_histogram(binwidth=5, fill="grey80", color="grey20") + theme_bw() +
  facet_grid(.~burstQual)

ggplot(data = filter(fastPos.comp2, fastVel>20) , aes(x=fastPos)) + 
  geom_histogram(binwidth=5, fill="grey80", color="grey20") + theme_bw() + 
  facet_wrap(~burstQual)
    
  
# next steps; review data sheets and basic plots for outliers and corrections
# look at sample sizes after filtering


