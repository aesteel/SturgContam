
library(tidyverse)
library(cowplot)


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


######*******########********#########**********#########***********#########
  ### ***** something is off with T30; check the output file, the =raw data sheet, and the metadata csv to see what isn't aligning. Note from AES 3/23/2021 **** ###
######*******########********#########**********#########***********#########



filestart <- unique(trial_specs$raspPI_filestart)


# create empty dataframe to record mean of top 3 burst velocities, after removing top 2; for each burst of each fish
burst.topvels = data.frame(treatmentrep = "EMPTY", burstID = NA, burstQual = as.factor(NA),
                          n_vels = NA, n_avg = NA, n_dropped = NA,
                          mn.topburstvels = NA, sd.topburstvels = NA)

  # create empty dataframe to record above burst velocity averaged across the the 'good' trials for each fish, as well as sd of bursts and and N good bursts considered
mean.topvels = data.frame(treatmentrep = filestart, 
                          mnburstGood = NA, sdburstGood = NA, nburstGood = NA,
                          mnburstGoodFair = NA, sdburstGoodFair = NA, nburstGoodFair = NA)

# create empty data frame to record gates with top 25% of speeds within a burst for each fish
fastPos.comp = data.frame(treatmentrep = "EMPTY", burstID = as.factor(NA), burstQual = NA, 
                          fastVel = NA, fastGate = as.factor(NA), fastPos = NA)#, nGatesMissed = NA)






### loop through all burst trials and fill in above df, as well as write out 'pdf of burst speeds over multiple tunnel segments

# first define segment lengths to use in summarizing data
min_distance = 1.5        
max_distance = 15 
write.file = "No"

for(f in 1:length(filestart)) {
     filename = dir("rawData/BurstTrialRaw", pattern=filestart[f])
     
     figureoutputfolder = paste0("figures/Bursts_Outputfigures/",filestart[f])
         if (file.exists(figureoutputfolder) == FALSE) { dir.create(figureoutputfolder)}
      dataoutputfolder = paste0("outputData/Bursts_Outputdata/",filestart[f])
          if (file.exists(dataoutputfolder) == FALSE) { dir.create(dataoutputfolder)}
    
  ## setup:
     dat = read.csv(paste0("rawData/BurstTrialRaw/",filename)  )
      dat = dat[1:(nrow(dat)-3),]  
      # removes last three rows with start/end/total time stamps not burst events
    
     metadat = trial_specs[trial_specs$raspPI_filestart==filestart[f],]
     
     num_burst<-nrow(metadat)
     
     gate_names <- names(dat)[grep("Gate",names(dat))] # subsets all the gate names
     
        ##### Unnecessary code from Ken; replaced with single line above #####
        # num_gates <- length(names(dat)[grep("Gate",names(dat))])  
        ## figure out number of gates from the tunnel specs.
        # first_gate_L <- which(names(dat)=="Gate0")  
        ## finds the first gate in the dataframe
        # first_gate_R <- which(names(dat)==paste0("Gate",(num_gates-1))) 
        ## finds the index of the rightmost gate
         ## MAYBE CHANGE THIS DEPENDING ON HOW THE RASPBERRY PI HANDLES LEFT AND RIGHT
        # gate_names <- names(dat[first_gate_L:first_gate_R]) 
        ## subsets all the gate names
        #####

 ## filter out non-fish gate trips using metadata (perhaps remove 'poor' bursts later)
     dat2 = data.frame(burstQual=as.character(NA), dat)# to be filled in with the following loop
    
     for(b in 1:nrow(dat2)) {                       #  for each row in dat
      dat2$burstQual[b] <- metadat[b,"burstQual"]   #   pull metadat for corresponding burst 
      finalgate = metadat[b,"stalledAt"]            #   translate 'stalledAt' number to GateXX name
      if(is.na(finalgate)) next                     #   fix bug in code if there isn't enough data 
      if(finalgate==25) next
      badcol_index = finalgate+4                    #  replace data from columns GateXX and after
      dat2[b,badcol_index:ncol(dat2)] <- -99        #    with -99, to be removed in next stage 
     }
     
     # dat.missinggates = dat2  
     ## use this to look at which gates were missed (vs bad burst behavior)
     #       trash = data.frame(apply(dat.missinggates, 2, is.na))
     #       
     #     # I need more time to get into this ##
       
     
     # switch -99 to NA for rest of code
     dat2 = dat2 %>% 
      mutate_all(~na_if(., '-99'))
    
    
    
  ### Create an empty list of datafames for each burst in the target trial
    burst.df.list <- replicate(num_burst, data.frame()) 
     
    # Calulate Metrics for each burst attempt in target trial----
    for (i in 1:num_burst){
      assign("temp.dat", dat2[i,],.GlobalEnv)
      temp.dat.gathered <- gather(temp.dat, all_of(gate_names), 
                                  key = "GATE_ID",value = "TIMING") %>% 
        mutate("POSITION" = tunnel_specs$GATE_DISTANCES) %>% # trust these things line up
        mutate("POSITION_DIFF" = POSITION - lag(POSITION,1)) %>% 
          # Calulates the difference in position between gates
        mutate("TIMING_DIFF" = TIMING - lag(TIMING,1)) %>% 
           # Creates a column which calculates teh differences between two sequential timings
        mutate("VELOCITY" = POSITION_DIFF/TIMING_DIFF) %>% 
           # calculates velocity
        mutate("VELOCITY_DIFF" = VELOCITY - lag(VELOCITY,1)) %>%  
           # creates column which calculates teh differnce between two sequential speeds
        mutate("ACCEL" = VELOCITY_DIFF/TIMING_DIFF) %>%
        filter(VELOCITY >= 0) ## removes row if the time recorded was before the previous; might be better to remove the previous? Or maybe more conservative to keep the slower time incase the out-of-order laser wasn't broken by the fish?
           # calculates accleration
     burst.df.list[[i]]<-temp.dat.gathered #
           # stores them all as dataframes in a list
    }
    
   # rbind_burst.df.list = do.call(rbind, burst.df.list)
   # works great, just don't use it again
    
    # filter to remove all elements of the list that have no data; they cause problems later
   burst.df.list = Filter(function(b) {sum(!is.na(b$TIMING_DIFF)) > 0}, burst.df.list)

    
  ## Write out to previously defined output folder  
    if(write.file=="Yes") {write.csv(rbind_burst.df.list, paste0(dataoutputfolder,"/Gate_by_Gate_metrics_",filestart[f],".csv"), row.names=F) }
    
    
  #####      
  # Calculate metrics for non-sequential gates; store in dataframe created above
  # I changed something and broke this on 3/26/2021
    
    # veldat_maxXcm = function(burst) {
    #     if(nrow(burst)<1) {burst[1,"burstQual"] <- "none"} # deals with missing bursts from raspPi
    #     vel_dat_2 <- segment_dat_1 %>%  # segment_dat_1 is tunnel metadata
    #     mutate(burstQual = unique(burst$burstQual)) %>%
    #     mutate(BURST_NUMBER = unique(burst$BURST_NUMBER)) %>%
    #     mutate(ORIENTATION = unique(burst$ORIENTATION)) %>%
    #     mutate(TIMING_A = (burst$TIMING[GATE_A+1])) %>% 
    #     mutate(TIMING_B = (burst$TIMING[GATE_B+1])) %>% 
    #     mutate(TIME_DIFF = TIMING_B - TIMING_A) %>% 
    #     mutate(VELOCITY = DIFF/TIME_DIFF) %>% 
    #     filter(DIFF >= min_distance & DIFF <= max_distance) %>% # dist defined prior to loop
    #     filter(TIME_DIFF <= 2) %>% # removes erroneous values generated by comparing gates which were triggered way too far apart
    #     filter(VELOCITY >= 0) ## should this be filtered out elsewhere?
    #   return(vel_dat_2)
    # }
    #   
    # plot_vel_data = lapply(burst.df.list,  veldat_maxXcm)
    
    # ## Plot data for each burst
    # if(write.file=="Yes") {
    #     
    #     pdf(paste0(figureoutputfolder,"/filtered_segmentBursts_",filestart[f],".pdf"), 
    #         onefile=TRUE, )
    #     for(i in 1:15) {
    #       if(nrow(plot_vel_data[[i]]) == 0) {next} else {
    #       print ( ggplot(data = plot_vel_data[[i]]) +
    #         geom_errorbarh(aes(xmax = END, xmin = START, y = VELOCITY, height = 0))+
    #       geom_point(aes(x=MIDPOINT, y = VELOCITY), color = "orangered2") + 
    #         coord_cartesian(xlim=c(0,100), ylim=c(0,70))+
    #         ggtitle(paste("Velocity: Burst",i," (max segment length =",max_distance,")")) +
    #       ylab("Velocity (cm/s)")+
    #         xlab("Tunnel Position (cm)")+
    #         theme(axis.text.x = element_text(angle=90)) ) }  
    #                }
    #     dev.off()
    #   }
    # 
    # ### other exploratory plots from Ken's code ##
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
    #####
      
      
  ### pull velocity metrics
    #  mean of fastest 3 velocities per burst, after dropping fastest two, 
    
    topvel_func= function(x,n,d) { ## runs with x = either burst.df.list or plot_vel_data 
      vel.list = sort(x$VELOCITY, decreasing=TRUE) 
      n_vels <- length(vel.list)
      n_avg <- n
      n_dropped <- d
      if(length(vel.list)<(n+d)) {mnvel=NA; sdvel=NA; n_avg<-0} else {
      mnvel = mean(vel.list[(d+1):(d+n)]); sdvel = sd(vel.list[(d+1):(d+n)]) }
      return(data.frame(treatmentrep = filestart[f],
                        burstID = unique(x$BURST_NUMBER), 
                        burstQual = factor(unique(x$burstQual), levels=c("poor","okay","good")),
                        n_vels = n_vels,
                        n_avg = n_avg,
                        n_dropped = n_dropped,
                        mn.topburstvels = mnvel, 
                        sd.topburstvels = sdvel)) 
     }
                        
  ### drop fastest (sometimes erroneous) and take mean of next N sequential segment velocities
    dd = 2 # drop fastest dd
    nn = 3 # average remaining nn
    burst.mntopvels = do.call(rbind, lapply(burst.df.list, topvel_func, n=nn, d=dd)) # seems to give similar answers to results from all (overlapping) gate combinations; not exhaustively or formally evaluated. Using this avoids pseudoreplication, but may be less ideal if there are gaps in data where lasers are frequently missed. 
     # returns NA vel if there are =< n velocity values measured
   
    
    if(write.file=="Yes") {write.csv(burst.mntopvels, paste0(dataoutputfolder,"/Subjective_Rank_Mn_dropTop2_avgNext",nn,"_",filestart[f],".csv"), row.names=F)}
    
      ## append to empty dataframe  
    burst.topvels = rbind(burst.topvels, burst.mntopvels)

  
    
    
  ### function to pull the top 25% of speeds (measured at each sequential gate) for each burst and identify where along the burst tunnel they are happening
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
    fastPos$burstID = factor(fastPos$burstID, levels=c(1:15))
    fastPos$fastGate = factor(fastPos$fastGate, levels=paste0("Gate",1:24))
    
      ggplot(fastPos, aes(y=fastVel,x=burstID, color=burstQual)) + geom_point() + 
        theme_bw() 
      ggplot(fastPos, aes(x=fastGate, fill=burstQual)) + geom_histogram(stat="count", position="dodge", color="black") + facet_grid(.~burstQual)+
        theme_bw() 
    
    # append the burst from the current loop to an existing object
    fastPos.comp = rbind(fastPos.comp, fastPos)
    
    
    # boxplots of mean top speeds within each subjective rank category
    subjective_rank_spds = ggplot( data = burst.mntopvels[!is.na(burst.mntopvels$mntopvel),],
            aes(x=burstQual, y=mntopvel)) +
            geom_boxplot() +
              geom_point()+
            theme_bw()
    
    
    if(write.file=="Yes") {
      pdf(paste0(figureoutputfolder,"/Subjective_Rank_Mn_dropTop2_avgNext",nn,"_",filestart[f],".tiff"), onefile=TRUE)
      
        subjective_rank_spds
        
      dev.off()}
    
    # calculate metrics to output
    mn.burstvalue = mean(burst.mntopvels$mn.topburstvels[burst.mntopvels$burstQual=="good"], na.rm=T)
    mn.burstvalue.notpoor = mean(burst.mntopvels$mn.topburstvels[burst.mntopvels$burstQual%in%c("good","okay")], na.rm=T)
    
    sd.burstvalue = sd(burst.mntopvels$mn.topburstvels[burst.mntopvels$burstQual=="good"], na.rm=T)
    sd.burstvalue.notpoor = sd(burst.mntopvels$mn.topburstvels[burst.mntopvels$burstQual%in%c("good","okay")], na.rm=T)
    
    n.burstvalue = length(burst.mntopvels$mn.topburstvels[burst.mntopvels$burstQual=="good"])
    n.burstvalue.notpoor = length(burst.mntopvels$mn.topburstvels[burst.mntopvels$burstQual%in%c("good","okay")])
    
    # add them to the appropriate row in empty dataframe
    mean.topvels[mean.topvels$treatmentrep == filestart[f], "mnburstGood"] <- mn.burstvalue
    mean.topvels[mean.topvels$treatmentrep == filestart[f], "mnburstGoodFair"] <- mn.burstvalue.notpoor
    mean.topvels[mean.topvels$treatmentrep == filestart[f], "sdburstGood"] <- sd.burstvalue
    mean.topvels[mean.topvels$treatmentrep == filestart[f], "sdburstGoodFair"] <- sd.burstvalue.notpoor
    mean.topvels[mean.topvels$treatmentrep == filestart[f], "nburstGood"] <- n.burstvalue
    mean.topvels[mean.topvels$treatmentrep == filestart[f], "nburstGoodFair"] <- n.burstvalue.notpoor

}


### explore resulting data for patterns

# look at compiled data and fix formatting
mean.topvels
mean.topvels$treatment = as.numeric(substring(mean.topvels$treatmentrep, 10, 12))
mean.topvels$rep = substring(mean.topvels$treatmentrep, 15, 16)

# remove one line used to start the df
burst.topvels2 = burst.topvels[burst.topvels$treatmentrep!="EMPTY",]
fastPos.comp2 = fastPos.comp[fastPos.comp$treatmentrep!="EMPTY",]
                        

### review study design and analysis approach first:     
  # look at where the fastest burst segment were
    ggplot(fastPos.comp2, aes(y=fastVel,x=burstID, color=burstQual)) + geom_point() + 
      theme_bw() 
    
    ggplot(fastPos.comp2, aes(x=fastGate, fill=burstQual)) + geom_histogram(stat="count", position="dodge", color="black") + facet_grid(.~burstQual)+
      theme_bw() 
  
  
  
  # look at relationship between estimated speed and number of samples or SD of sample
  hist(mean.topvels$nburstGoodFair/15, 
       xlab="Percent of bursts classified as GOOD", 
       main="Data Quality")
  
  # look at relationship between estimated burst speeds and number of good bursts
  ggplot(mean.topvels, aes(x=nburstGood, y=mnburstGood)) + geom_point() + geom_smooth()
  
  # reduce this to only fish that gave >4 good bursts
  table(mean.topvels[mean.topvels$nburstGood>4,]$treatment)
  ggplot(filter(mean.topvels, nburstGood>4), aes(x=nburstGood, y=mnburstGood ,color=factor(treatment))) + 
    geom_point(size=4) + geom_smooth() + 
    scale_color_manual(values=c("black","green3","yellow","red"))
    # perhaps only considering fish with >4 good bursts will be more reliable?
  
  
  # look at relationship between number of bursts and sd in bursts wihtin a fish
  ggplot(mean.topvels, aes(x=mnburstGood, y=sdburstGood)) + 
    geom_point(aes(color=nburstGood), size=3) + geom_smooth()
  
  ggplot(filter(mean.topvels, nburstGood>4), aes(x=nburstGood, y=sdburstGood)) + 
    geom_point() + geom_smooth()
  
  # relationship between mean and sd
  ggplot(filter(mean.topvels, nburstGood>0), aes(x=mnburstGood, y=sdburstGood)) + 
    geom_point(aes(color=nburstGood), size=3) + geom_smooth()
  

  
    # look at relationship between burst quality and treatment
  ggplot(mean.topvels, aes(x=factor(treatment), y=nburstGood)) + geom_boxplot()
  
  # test this statistically
  testmod_n = lm(nburstGoodFair~factor(treatment), data = filter(mean.topvels))
    plot(testmod_n) # meets assumptions well
    summary(aov(testmod_n)) # low power, no significance
    TukeyHSD(aov(testmod_n)) # biggest difference is between 500-0, then 100-0, then 500-5. 
    
  # in general, trend for more 'good' bursts from treated fish, which seems to align with jumpier, more active fish coming out of higher treatments. This makes me suspicious of my 'good' criteria. 
  # but I went back and added a tally of good/okay to the dataframe and it shows the same pattern, maybe even clearer. Hm. 
    
    

# look at relatinship between treatment and burst speeds
ggplot(mean.topvels, aes(x=log(treatment), y=mnburstGood)) + geom_point() + geom_smooth()
ggplot(mean.topvels, aes(x=factor(log(treatment)), y=mnburstGood)) + geom_boxplot() + theme_bw()

# plot points for each fish, jittered around the log of the exposure concentration; presents same dataset but with more information
# this is the plot used for Ken's bay delta poster
set.seed(38)
mean.topvels$random.offset = runif(n=nrow(mean.topvels), min = -0.5, max=0.5)
mainplot = ggplot(mean.topvels, aes(x=log(treatment+1)+random.offset, y=mnburstGood))+ 
         geom_errorbar(aes(ymin=mnburstGood-sdburstGood, ymax=mnburstGood+sdburstGood), width=0) + 
         geom_point(aes(size=nburstGood, fill=factor(treatment)), pch=21, alpha=0.8) + 
         theme_bw() + 
         ylab("Mean Burst Swim Speed (cm/s)") +
         scale_x_continuous(breaks=c(0,1.6,4.6,6.2), name="log(Bifenthrin Concentration)")  +
         scale_size_continuous(breaks=c(5,10,15), name = "Number of Bursts") + 
         scale_fill_discrete(name="Bifenthrin\nConcentration (ng/L)")

sizeplot = ggplot(trial_specs, aes(y=tlmm/10)) + 
  geom_boxplot(alpha=.5) + theme_bw()+
  ylab("Green Sturgeon Length (cm)")  
  #xlab("log(Bifenthrin Concentration)") 
sizeplot
      # sizeplot_treatment = ggplot(trial_specs, aes(x=factor(log(nomconc+1)), y=tlmm/10, fill=factor(nomconc))) + 
      #   geom_boxplot(alpha=.5) + theme_bw()+
      #   ylab("Green Sturgeon Length (cm)") + 
      #   scale_fill_discrete(name="Bifenthrin\nConcentration (ng/L)")+
      #   scale_x_discrete(labels=c(0,1.6,4.6,6.2), name="log(Bifenthrin Concentration)") 
      # sizeplot_treatment

plot_grid(mainplot, sizeplot)


testmod = lm(mnburstGood~log(treatment+1), data = filter(mean.topvels, nburstGood>4))
testmod = lm(mnburstGood~factor(treatment), data = filter(mean.topvels, nburstGood>4))
  plot(testmod) # fits assumptions fine
  summary(aov(testmod)) # not significant; likely not enough power (n=8); when duplicate the dataset to have a power of n=16, p=0.0127 (log(treatment+1))
  TukeyHSD(aov(testmod)) 
  # biggest differences is 100-0, then 500-0, then 5-0, 100-5, 500-100, and finally 500-5. Not much of a clear trend. When I duplicate the dataset to simulate larger sample size, there is a significant difference between 100-0 but no others. Might be driven by the one fast fish in 100ng/L. This was the fish we labeled as 'champ' which was exceptional at bursting (or at least cooperating with our trial design). 


  
# look for patterns on where in the tunnel the fastest burst occurred (building new one?)
ggplot(data = fastPos.comp2, aes(x=fastPos)) + 
  geom_histogram(binwidth=5, fill="grey80", color="grey20") + theme_bw() +
  facet_grid(.~burstQual)

ggplot(data = filter(fastPos.comp2, fastVel>20) , aes(x=fastPos)) + 
  geom_histogram(binwidth=5, fill="grey80", color="grey20") + theme_bw() + 
  facet_wrap(~burstQual)
    
  
# next steps; review data sheets and basic plots for outliers and corrections
# look at sample sizes after filtering


