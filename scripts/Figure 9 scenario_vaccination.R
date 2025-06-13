
#################################################################################.
# # Initialise script ####
#################################################################################.

require(toolboxH) # https://github.com/holgerman/toolboxH
require(ggplot2)
require(ggthemes)
require(scales)
require(lubridate)
require(here)
require(patchwork)


source(here("scripts/functions_model_240112.R"))

maxdate_diagrams = as_date('2022-09-30')

#################################################################################.
# # LOAD data ####
#################################################################################.


## # load epidata ####
epidata = fread(here("data/FIGURE_and_SUPPLEMENT_general/s1030_2_datint_ecdc_DE_BL_2023-03-26_v5_agestrat.txt"), dec = ",") # RKI data, original data made availabe via github.com/robert-koch-institut/

epidata[ CountryExp =="Deutschland", CountryExp := "Germany"]




## # load variant frequencies ####
vocdata = fread(here("data/FIGURE_scenario_vaccination/s245_1_simplified_coronavariants_DE_until2023-11-08.txt")) # RKI and ECDC via https://github.com/robert-koch-institut/SARS-CoV-2-Sequenzdaten_aus_Deutschland/ and https://www.ecdc.europa.eu/sites/default/files/documents/PathogenVariant_public_mappings.csv
vocdata[,.N, .(LINEAGE_model)]

vocdata2 = vocdata[LINEAGE_model %in% c("Alpha", "Delta", "BA.1", "BA.2", "BA.4+BA.5"), .(date = as_date(DateRep),
                                                                                          proportion = proz7d,LINEAGE_model)]
vocdata2[, variant2 := ifelse(LINEAGE_model=="Alpha", "alpha",
                              ifelse(LINEAGE_model=="Delta", "delta",
                                     ifelse(LINEAGE_model=="BA.1", "BA1",
                                            ifelse(LINEAGE_model=="BA.2", "BA2",
                                                   ifelse(LINEAGE_model=="BA.4+BA.5", "BA4+5",LINEAGE_model)))))]
vocdata2[, variant2b:= factor(variant2, levels = c("WT","alpha", "delta", "BA1", "BA2", "BA4+5"))]

datesVariantsEmerging = vocdata2[proportion>0.05 & date>as_date("2020-12-01") &proportion<0.7 , .(date, proportion, variant2)][duplicated(variant2)==F][order(date)]
datesVariantsEmerging
datesVariantsEmerging[variant2=="BA1", variant2 := "BA1  "]
datesVariantsEmerging[variant2=="BA2", variant2 := "   BA2"]

## load simulation data ####

modeldata = fread(here("data/FIGURE_scenario_vaccination/modeldata_saarland.csv"))
modeldata

## # load vaccination data -----

vaccdata = fread(here("data/FIGURE_scenario_vaccination/data_vacc_saarland.csv"))
vaccdata

#################################################################################.
# #  vaccination data ####
#################################################################################.
 

## plot vacc ####


p7 =ggplot(vaccdata, aes(DateRep, value, lty = variable2, col  = country )) +    
  scale_color_wsj( palette = "colors6") + 
  ylim(c(0,1))+
  geom_line(lwd=1) + 
  theme_minimal(base_size = 14) + 
  scale_y_continuous(breaks = pretty_breaks(10), 
                     labels = label_percent(accuracy = 1), 
                     sec.axis = dup_axis()) + 
  ylab("vaccinated") +
  scale_x_date( limits = range(vaccdata$DateRep))+
  theme(axis.title.x = element_blank(),
        legend.position = c(0.21, 0.7),
        legend.key.height = unit(x = 0.3, units = "cm")) + 
  labs(lty = "")+
  scale_x_date(breaks = date_breaks(width = "2 month"), labels =  date_format("%b-%y"))+
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ), 
        legend.position = "top") + 
  
  geom_vline( xintercept = datesVariantsEmerging$date, lty = 2, alpha = 0.5) + 
  coord_cartesian(xlim = c(min(vaccdata$DateRep), maxdate_diagrams))  +geom_vline( xintercept = datesVariantsEmerging$date, lty = 2)  +
  annotate("text",x = (datesVariantsEmerging$date+21)[1], y = 0.95,label = paste0(datesVariantsEmerging$variant2[1], "         "), angle = 0 ) +
  annotate("text",x = (datesVariantsEmerging$date+21)[2], y = 0.95,label = paste0(datesVariantsEmerging$variant2[2], "         "), angle = 0) +
  annotate("text",x = (datesVariantsEmerging$date+21)[3], y = 0.95,label = paste0(datesVariantsEmerging$variant2[3], "         "), angle = 0) +
  annotate("text",x = (datesVariantsEmerging$date+21)[4], y = 0.95,label = paste0(datesVariantsEmerging$variant2[4], "         "), angle = 0) +
  annotate("text",x = (datesVariantsEmerging$date+21)[5], y = 0.95,label = paste0(datesVariantsEmerging$variant2[5], "            "), angle = 0) +
  theme(legend.position = "top")


p7

#################################################################################.
# # processing scenario data ####
#################################################################################.

## # prepare incident data ----
modeldata[,.N, .(type, outcome,scenario, region)]


modeldata[ type =="modelled" & region =="Saarland" & outcome =="died"][order( outcome,-datum, scenario)][allDuplicatedEntries(datum)]


modeldata_saarland = modeldata[(scenario =="observed" & region =="Saarland") | ( region =="Saarland" & type =="modelled" & scenario == "simulated German vacc")]
modeldata_saarland[,.N, .(type, outcome,scenario, region)]


modeldata_saarland[, scenario := factor(scenario, levels = c("simulated German vacc","observed"))]
modeldata_saarland[,.N, .(type, scenario, region)]

modeldata_saarland = modeldata_saarland[ datum <= maxdate_diagrams  ]
modeldata_saarland[outcome =="reported", outcome := "testpositives\n7-day-average"]
modeldata_saarland[outcome =="died", outcome := "total\ndeaths"]
modeldata_saarland[, outcome := factor(outcome, levels = c("testpositives\n7-day-average", "total\ndeaths"))]

modeldata_saarland[scenario =="observed", scenario := "Model fit Saarland"]
modeldata_saarland[scenario =="simulated German vacc", scenario := "Model Saarland with vaccination rate Germany"]
modeldata_saarland[type =="reported", scenario := "Saarland reported"]


p_model =ggplot(modeldata_saarland, aes(datum, value,col = scenario,  alpha = type)) + 
  geom_line(lwd = 1.2) + 
  scale_color_manual( values = c(wsj_pal(palette = "colors6")(2),  "grey33")) + 
  facet_grid(outcome~., scales = "free") +
  
  scale_y_continuous(label= label_comma(accuracy = 1), breaks = pretty_breaks(10)) +
  
  theme_minimal(base_size = 14) + ylab("number") +
  scale_alpha_manual(values = c(1, 0.7))+
  scale_size_manual(values = c(0.6, 1.75)) +
  scale_x_date(breaks = date_breaks(width = "2 month"), labels =  date_format("%b-%y"))+
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ), 
        legend.position = "top") + 
  labs(col = "", lty = "") +
  scale_linetype_manual(values = c(1,2))+
  coord_cartesian(xlim = c(min(vaccdata$DateRep ), maxdate_diagrams))+
  
  geom_vline( xintercept =datesVariantsEmerging$date, lty = 2)

p_model
#################################################################################.
# Combined plot ----
#################################################################################.


combiplot = (
  (p_model  + 
     guides( lty = "none", alpha = "none") 
  )
  / 
    
    (p7 +
       scale_y_continuous(limits = c(0,1), labels = label_percent(accuracy = 1))+
       labs(color= "", lty = "") +
       theme(legend.box = "horizontal",legend.spacing.y = unit(0, 'cm'))
    ) +     plot_layout(heights = c(3,1)) + plot_annotation(tag_levels = "A" ) &   theme(plot.tag = element_text(face = 'bold', size = 19))
)
combiplot


pdf(here("results/FIGURE_9 scenario_vaccination.pdf"), width = 8,height = 7)
combiplot
dev.off()

#################################################################################.
## finalize script ----
#################################################################################.


finalizeSkript()

