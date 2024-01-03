
dat20gs = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2020Expts/Expt Docs - Range Test 2020 GS/GS_EXPOSURE/Exposure_DailyMorts_GS2020.csv")


dat20gssub = dat20gs[,c(4,6:8)]
dat20gssubt = data.frame(dat20gssub %>% pivot_wider(names_from = Exposure.Hr, names_prefix = "hr", values_from = Nmorts))

dat20gsend = dat20gs[!is.na(dat20gs$final.mort), c(1,2,6,7,10)]

dat20gsfinal = merge(dat20gssubt,dat20gsend)
dat20gsfinal = dat20gsfinal[, c("species","contaminant","nomconc", "RepDish","est.initial",
                                "hr24", "hr48", "hr72", "hr96")]




dat20ws = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2020Expts/Expt Docs - Range Test 2020 WS/Exposure/Exposure_DailyMorts_WS2020.csv")


dat20wssub = dat20ws[,c(4,6:8)]
dat20wssubt = data.frame(dat20wssub %>% pivot_wider(names_from = Exposure.Hr, names_prefix = "hr", values_from = Nmorts))

dat20wsend = dat20ws[!is.na(dat20ws$final.mort), c(1,2,6,7,10)]

dat20wsfinal = merge(dat20wssubt,dat20wsend)
dat20wsfinal = dat20wsfinal[, c("species","contaminant","nomconc", "RepDish","est.initial",
                                "hr24", "hr48", "hr72", "hr96")]



dat20final = rbind(dat20gsfinal,dat20wsfinal)

dat20final$tempC = 15
dat20final$totalMort = apply(dat20final[,6:9], 1, sum)
dat20final$year = 2020





dat21gs = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2021Expts/Expt Docs - Range Test 2021 GS Fipronil/Exposure_GSfip/Exposure_DailyMorts_GS2021.csv")

dat21gssub = dat21gs[,c(4,6:8)]
dat21gssubt = data.frame(dat21gssub %>% pivot_wider(names_from = Exposure.Hr, names_prefix = "hr", values_from = Nmorts))

dat21gsend = dat21gs[dat21gs$final.count!="",c(1,2,6,7,9,10)]

dat21gsfinal = merge(dat21gssubt,dat21gsend)
dat21gsfinal = dat21gsfinal[, c("species","contaminant","nomconc", "RepDish","est.initial",
                                "hr24", "hr48", "hr72", "hr96")]



dat21ws = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2021Expts/Expt Docs - Range Test 2021 WS Fipronil/Exposure_WSfip/Exposure_DailyMorts_WS2021.csv")

dat21wssub = dat21ws[,c(1,2,4,6:8)]
dat21wssubt = data.frame(dat21wssub %>% pivot_wider(names_from = Exposure.Hr, names_prefix = "hr", values_from = Nmorts))

dat21wsfinal = data.frame(dat21wssubt, est.initial=NA)
dat21wsfinal = dat21wsfinal[, c("species","contaminant","nomconc", "RepDish","est.initial",
                                "hr24", "hr48", "hr72", "hr96")]


dat21final = rbind(dat21gsfinal,dat21wsfinal)

dat21final$tempC = 15
dat21final$totalMort = apply(dat21final[,6:9], 1, sum)
dat21final$year = 2021





dat23gs = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2023Expts/Exposures/Exposure_DailyMorts_GS2023.csv")

dat23gssub = dat23gs[,c(4,6:9)]
dat23gssubt = data.frame(dat23gssub %>% pivot_wider(names_from = Exposure.Hr, names_prefix = "hr", values_from = Nmorts))

dat23gsend = dat23gs[!is.na(dat23gs$est.initial),c(1,2,6:8,13)]

dat23gsfinal = merge(dat23gssubt,dat23gsend)
dat23gsfinal = dat23gsfinal[, c("species","contaminant","nomconc", "RepDish","est.initial",
                                "hr24", "hr48", "hr72", "hr96", "tempC")]
                      
dat23gsfinal$totalMort = apply(dat23gsfinal[,6:9], 1, sum)
dat23gsfinal$year = 2023
          






dat22ws = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2022Expts/WS experiments/Exposures/Exposure_DailyMorts_WS2022.csv")
dat22ws = rename(dat22ws, final.count=Nfinal.transferredFW)

dat22wssub = dat22ws[,c(1,2,4,6:9)]
dat22wssubt = data.frame(dat22wssub %>% pivot_wider(names_from = Exposure.Hr, names_prefix = "hr", values_from = Nmorts))

dat22wsend = dat22ws[!is.na(dat22ws$final.count), c(1,2,6:8,13)]

dat22wsfinal = merge(dat22wssubt, dat22wsend)
dat22wsfinal = dat22wsfinal[, c("species","contaminant","nomconc", "RepDish","est.initial",
                                "hr24", "hr48", "hr72", "hr96", "tempC")]


dat22wsfinal$totalMort = apply(dat22wsfinal[,6:9], 1, sum)
dat22wsfinal$year = 2022




datfinal = rbind(dat20final, dat21final, dat22wsfinal, dat23gsfinal)


write.csv(datfinal, "/Users/Anna/Desktop/google drive backup files/2020 contaminants/Data Curated for CDFW/morts_all_tempfile.csv", row.names=F)
