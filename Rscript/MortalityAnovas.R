mortdat = read.csv("/Volumes/GoogleDrive/My Drive/2020 GS Multiple Stressors/2021Expts/ExposureMortalitySummaryAll.csv")


str(mortdat)

Bifplot = ggplot(mortdat[mortdat$pesticide=="bifenthrin",], aes(x=factor(nomconc), y=percMort*100, fill=spp)) + 
  geom_boxplot() + facet_grid(.~spp)  + 
  theme_bw() + 
  scale_fill_manual(guide="none", values=c("palegreen3","grey90")) + 
  ylab("Percent Mortality") + xlab("Nominal Bifenthrin Concentration (ng/L)") + 
  ylim(c(0,10))

Fipplot = ggplot(mortdat[mortdat$pesticide=="fipronil",], aes(x=factor(nomconc), y=percMort*100, fill=spp)) + 
  geom_boxplot() + facet_grid(.~spp)  + 
  theme_bw() + 
  scale_fill_manual(guide="none", values=c("palegreen3","grey90")) + 
  ylab("Percent Mortality") + xlab("Nominal Fipronil Concentration (ug/L)") + 
  ylim(c(0,10))


library(patchwork)
Bifplot + Fipplot + plot_layout(ncol = 1)



bifGSdat = subset(mortdat, spp=="Green Sturgeon" & pesticide=="bifenthrin")
fipGSdat = subset(mortdat, spp=="Green Sturgeon" & pesticide=="fipronil")
bifWSdat = subset(mortdat, spp=="White Sturgeon" & pesticide=="bifenthrin")
fipWSdat = subset(mortdat, spp=="White Sturgeon" & pesticide=="fipronil")

bif.modelGS = anova(lm(percMort ~ conclabel, bifGSdat))
 plot(lm(percMort ~ conclabel, bifGSdat))
 bif.modelGS
 TukeyHSD(aov(percMort ~ conclabel, bifGSdat))

bif.modelWS = anova(lm(percMort ~ conclabel, bifWSdat))
 plot(lm(percMort ~ conclabel, bifWSdat))
 bif.modelWS
 TukeyHSD(aov(percMort ~ conclabel, bifWSdat))

fip.modelGS = anova(lm(percMort ~ conclabel, fipGSdat))
 plot(lm(percMort ~ conclabel, fipGSdat))
 fip.modelGS
 TukeyHSD(aov(percMort ~ conclabel, fipGSdat))
 
fip.modelWS = anova(lm(percMort ~ conclabel, fipWSdat))
 plot(lm(percMort ~ conclabel, fipWSdat))
 fip.modelWS
 TukeyHSD(aov(percMort ~ conclabel, fipWSdat))

 ## no significant differences between treatments in any spp-pesticide combinations