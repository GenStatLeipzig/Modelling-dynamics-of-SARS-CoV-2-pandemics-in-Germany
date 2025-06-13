#################################################################################.
## Initialise script ----
#################################################################################.

require(toolboxH) # https://github.com/holgerman/toolboxH
require(ggplot2)
require(ggthemes)
require(scales)
require(lubridate)
require(here)
require(patchwork)

initializeSkript()

#################################################################################.
## LOAD data ----
#################################################################################.

### data truth ----

observed_data = fread(here(paste0("data/FIGURE_impact_npr_wavebreaker_saxony/042_datint_ecdc_saxony_2022-06-01_v5_agestrat.txt"))) # RKI data, original data made availabe via https://github.com/robert-koch-institut/ 

observed_data[,DateRep:= as_date(DateRep)]


### modelled data ----
allfolders = c(
  "data/FIGURE_impact_npr_wavebreaker_saxony/Eff0_6FittedParams/",
  "data/FIGURE_impact_npr_wavebreaker_saxony/Eff0_6NoRestrictions/",
  "data/FIGURE_impact_npr_wavebreaker_saxony/Eff0_6Restrictions/"
)
stopifnot(file.exists(here(paste0(allfolders[[1]], "BestSolution_0_14PreictionFor350Days.csv"))))

extractData <- function(myfolder) {
  
  model_0_14 = fread(here(paste0(myfolder, "BestSolution_0_14PreictionFor350Days.csv")), dec = ",")
  model_0_14$folder = myfolder
  model_0_14$Altersgruppe = "A00-A14"
  
  model_15_34 = fread(here(paste0(myfolder, "BestSolution_15_34PreictionFor350Days.csv")), dec = ",")
  model_15_34$folder = myfolder
  model_15_34$Altersgruppe = "A15-A34"
  
  model_35_59 = fread(here(paste0(myfolder, "BestSolution_35_59PreictionFor350Days.csv")), dec = ",")
  model_35_59$folder = myfolder
  model_35_59$Altersgruppe = "A35-A59"
  
  
  model_60_79 = fread(here(paste0(myfolder, "BestSolution_60_79PreictionFor350Days.csv")), dec = ",")
  model_60_79$folder = myfolder
  model_60_79$Altersgruppe = "A60-A79"
  
  model_80plus = fread(here(paste0(myfolder, "BestSolution_80PreictionFor350Days.csv")), dec = ",")
  model_80plus$folder = myfolder
  model_80plus$Altersgruppe = "A80+"
  
  
  model_all = fread(here(paste0(myfolder, "BestSolution_allPreictionFor350Days.csv")), dec = ",")
  model_all$folder = myfolder
  model_all$Altersgruppe = "all"
  
  modeldat = rbind(model_0_14,model_15_34, model_35_59, model_60_79, model_80plus,model_all)
  modeldat[,DateRep := as_date(Date)]
  
  modeldat
}


#################################################################################.
## data wrangling ----
#################################################################################.

modeldat = lapply(allfolders, function(x) extractData(myfolder = x)) %>% rbindlist(use.names = T)


modeldat[,Impfeffizienz := ifelse(grepl('Eff0_6', folder), '60%', NA)]

modeldat[,Lockdowneffizienz := ifelse(grepl('Fitted', folder), 'geschätzt', 
                                      # ifelse(grepl('LimitationsAs22122021', folder), 'wie Dez2020', 
                                      ifelse(grepl('NoRestrictions', folder), 'keine', 
                                             ifelse(grepl('Restrictions', folder), 'Dez2020', folder)))]

modeldat[, phase := ifelse(DateRep> (as_date("2021-11-22")-21) & DateRep< as_date("2021-11-22"), "pre", 
                           ifelse(DateRep> as_date("2021-11-22") & DateRep< (as_date("2021-11-22")+21), "post", NA))]

observed_data$Impfeffizienz = "60%" 
observed_data$ Lockdowneffizienz = "geschätzt"
observed_data = observed_data[Altersgruppe != "unbekannt"]


modeldatpre = modeldat[Impfeffizienz=="60%" & Altersgruppe == "all" & Lockdowneffizienz!="Dez2020" & DateRep <= as_date("2022-01-20" ) & DateRep >= as_date("2021-10-01")]


#################################################################################.
## input plotting  ----
#################################################################################.

### plot figure section A)-----

plotdat1 = modeldat[is.na(phase)==F, .(DateRep,phase, Lockdowneffizienz, Impfeffizienz, vr1, vr2, Altersgruppe)]
plotdat1[,.N, .(phase, Lockdowneffizienz, Impfeffizienz, Altersgruppe)]

plotdat2 = plotdat1[, .(vr1mean = mean(vr1)), .(phase, Lockdowneffizienz, Impfeffizienz, Altersgruppe)]

plotdat3 = plotdat2[Altersgruppe!= "all" & phase !="pre" & Impfeffizienz =="60%"]
plotdat3[, zeile := .I]
plotdat3[,geschaetzt_rel := vr1mean[Lockdowneffizienz=="geschätzt"]/vr1mean[Lockdowneffizienz=="keine"], .(Altersgruppe)]
plotdat3[,geschaetzt_dez := vr1mean[Lockdowneffizienz=="Dez2020"]/vr1mean[Lockdowneffizienz=="keine"], .(Altersgruppe)]

plotdat3m = melt(plotdat3, id.vars = c("phase", 'Impfeffizienz', 'Altersgruppe'), measure.vars = c('geschaetzt_rel',  'geschaetzt_dez'))

NogpaletteReihe <-  c("#CB769E", "#DE639A", "#A85C85", "#0081AF", "#4F6D7A", "#7C6A0A", "#368F8B", "#246A73", "#5CC1BC", "#62C370", "#F7C548", "#F97E44", "#FB3640", "#B7245C", "#0D3B66", "#3E2F5B", "#B2675E", "#644536")
 
mycolors1 = c("#CB769E",   "#3E2F5B" )

p1 = ggplot(plotdat3m, aes(Altersgruppe, -(1-value), fill = variable)) + geom_col(position = "dodge") + scale_y_continuous(breaks = pretty_breaks(5), labels = label_percent(accuracy = 1), name = "Reduction infectivity\nafter NPI adoption") + theme_hc(base_size = 16) + xlab( "") + labs(fill = "")+

  scale_fill_manual(values = mycolors1, labels=c("Effect new NPIs Nov.2021", "Effect NPIs Dec 2020")) + theme(legend.position = "top") 



### plot figure section B)-----



plottrendm =  melt(modeldatpre, id.vars = c("DateRep", "Lockdowneffizienz"), measure.vars = c("ReportedDailyPosCases","simulatedcumuldeath", "criticalallcomp")) 




observed_datam = melt(observed_data[DateRep <= max(modeldatpre$Date)& DateRep >= as_date("2021-10-01")& Altersgruppe =="all"  ], id.vars = "DateRep", measure.vars = c('NewConfCases', 'AllDeaths', 'covid_inICU_upscaled'))

observed_datam[, gruppe := ifelse(variable == "NewConfCases", "testpositives", 
                                  ifelse(variable== "AllDeaths", "total deaths", 
                                         ifelse(variable =="covid_inICU_upscaled", "ICU bed-occupancy", variable)))]



plottrendm[, gruppe := ifelse(variable == "ReportedDailyPosCases", "testpositives", 
                              ifelse(variable== "simulatedcumuldeath", "total deaths", 
                                     ifelse(variable =="criticalallcomp", "ICU bed-occupancy", variable)))]

plottrendm[,gruppe := factor(gruppe, levels = c("testpositives", 'total deaths', "ICU bed-occupancy"))]
observed_datam[,gruppe := factor(gruppe, levels = c("testpositives", 'total deaths', "ICU bed-occupancy"))]

plottrendm2 = plottrendm#[(gruppe %in% c("testpositives", "ICU bed-occupancy") & DateRep >as_date("2022-01-10")) ==F]
plottrendm2[,Lockdowneffizienz2 := ifelse(Lockdowneffizienz=="keine", "Excluding NPIs Nov 2021",  
                                          ifelse(Lockdowneffizienz =="geschätzt", "Including new NPIs Nov 2021", Lockdowneffizienz))]

plottrendm2[,Lockdowneffizienz2 := factor(Lockdowneffizienz2, levels = c("Including new NPIs Nov 2021", "Excluding NPIs Nov 2021", "Reported data"))]
observed_datam$Lockdowneffizienz2 =  "Reported data"
plottrendm2[,Lockdowneffizienz2 := factor(Lockdowneffizienz2, levels = c("Including new NPIs Nov 2021", "Excluding NPIs Nov 2021", "Reported data"))]



mycolors = c("#CB769E","#368F8B")
p2 = ggplot(plottrendm2, aes(DateRep,value  , col = Lockdowneffizienz2 )) + 
  geom_line(data = plottrendm2[Lockdowneffizienz == "keine"], alpha = 0.9, lwd = 1.5) + 
  geom_line(data = plottrendm2[Lockdowneffizienz != "keine"], alpha = 0.9, lwd = 1.5) + 
  scale_x_date(breaks = date_breaks(width = "1 month"), labels =  date_format("%b-%y")) + 
  theme(axis.text.x = element_text(angle = 45 , vjust = 1, hjust =1),
        legend.position = "top")+
  facet_wrap(~gruppe, scales = "free")  +
  # scale_y_continuous(breaks = pretty_breaks(10)) + 
  scale_y_log10(breaks = log_breaks(10)) + 
  theme_pander(base_size = 16)+
  geom_line(data = observed_datam, aes(DateRep, value ), color = "black", alpha = 0.3,lwd = 2, lty = 1)  + xlab("")+ labs(col = "")+
  scale_color_manual(drop=FALSE, values = c(mycolors, "grey27")) +
  theme(legend.position = "top") + ylab("Daily numbers") + 
  geom_vline(xintercept = as_date("2021-12-13"), linetype = "dashed", color = "grey27", size = 1) 


finplot = (p1 + theme(axis.text.x = element_text(angle  = 45, vjust = 1, hjust = 1), legend.position = c(0.35,1.15))+ guides(fill=guide_legend(ncol=1)) + p2) + patchwork::plot_layout(widths = c(1,3)) + patchwork::plot_annotation(tag_levels = "A") &   theme(plot.tag = element_text(face = 'bold', size = 19))

finplot

jpeg(file = here("results/FIGURE 8impact_npr_wavebreaker_saxony.jpeg"), width = 12, height = 6, units = "in", res = 300, quality = 100)
finplot
dev.off()

#################################################################################.
## finalize script ----
#################################################################################.

finalizeSkript()

