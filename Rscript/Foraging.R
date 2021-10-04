

fdataGS = read.csv("/Users/Anna/Google Drive/2020 GS Multiple Stressors/2021Expts/Expt Docs - Range Test 2021 GS Fipronil/Foraging_GSfip/Foraging_Metadata_GS2021.csv")

fdataWS = read.csv("/Users/Anna/Google Drive/2020 GS Multiple Stressors/2021Expts/Expt Docs _Range Test 2021 WS Fipronil/Foraging_WSfip/Foraging_Metadata_WS2021.csv")


fdata = rbind(fdataGS, fdataWS)
 fdata$color = factor(fdata$color, levels=c("white","green","yellow", "red","pink","blue"))
 
 
 
table(fdata$color, fdata$spp)



templot = ggplot(fdata, aes(x=testtemp)) + 
        geom_histogram(binwidth=.2, color="black",fill="grey40") + theme_bw()
templot + facet_grid(color~spp)



sizeplot = ggplot(fdata) + 
        geom_boxplot(aes(x=color, y=tlmm, fill=color)) +
        scale_fill_viridis(discrete=T, direction=-1) + theme_bw()
sizeplot + facet_wrap(~spp)
 



lwplot = ggplot(fdata, aes(x=tlmm, y=massg)) + 
        geom_point(aes(color=color)) + 
        scale_color_viridis(discrete=T) +
        theme_bw()  
lwplot + geom_density2d() + facet_wrap(~spp)

lwplot + geom_smooth(aes(color=color), method="lm", lwd=.5, se=FALSE) + 
        geom_smooth(method="lm", color="black", lwd=1.25) + facet_wrap(~spp)



