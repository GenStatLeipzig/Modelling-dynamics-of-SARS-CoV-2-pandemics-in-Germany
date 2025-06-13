rm(list = ls())
require(toolboxH)
require(ggplot2)
require(ggthemes)
require(scales)
require(lubridate)
require(here)
require(plotly)
require(patchwork)
initializeSkript()

source(here("scripts/functions_model_241106.R"))

maxdate_diagrams = as_date('2024-03-31')


##########################################################################################.# ## # # LOAD  =====
##########################################################################################.        

federal_countryinfo = fread(here("data/FIGURE_and_SUPPLEMENT_general/federal_countries.csv"))

## # load reported dat ####

epidata = fread(here("data/FIGURE_and_SUPPLEMENT_general//s230_1_datint_ecdc_DE_BL_2024-09-17_v6_agestrat.txt"), dec = ",") # from RKI  https://github.com/robert-koch-institut/
epidata[, region:= federal_countryinfo[match_hk(epidata$CountryExp, federal_countryinfo$dataname), region]]


## # load variant frequencies ####
vocdata_pre = fread(here("data/FIGURE_and_SUPPLEMENT_general//s236_5_simplified_coronavariants_DEFrom04032020_until2024-09-19_smoothed_yuriformat.txt")) # RKI and ECDC via https://github.com/robert-koch-institut/SARS-CoV-2-Sequenzdaten_aus_Deutschland/ and https://www.ecdc.europa.eu/sites/default/files/documents/PathogenVariant_public_mappings.csv
# vocdata_old = fread(here("data/FIGURE_and_SUPPLEMENT_general/s245_1_simplified_coronavariants_DE_until2023-11-08.txt")) # RKI and ECDC via
# 
# venn2(names(vocdata), names(vocdata_old))
vocdata = melt(vocdata_pre, id.vars = c("DateRep"), value.name = 'proportion', variable.name ="LINEAGE_model")

vocdata[,.N, .(LINEAGE_model)] %>% data.frame()

vocdata2 = vocdata[, .(date = as_date(DateRep), proportion), LINEAGE_model]#[
# LINEAGE_model %in% c("Alpha", "Delta", "BA.1", "BA.2", "BA.4+BA.5"), .(date = as_date(DateRep),
#                                                                                         proportion = proz7d,LINEAGE_model)]
vocdata2[, variant2 := ifelse(LINEAGE_model=="Alpha", "alpha",
                              ifelse(LINEAGE_model=="Delta", "delta",
                                     ifelse(LINEAGE_model=="BA.1", "BA1",
                                            ifelse(LINEAGE_model=="BA.2", "BA2",
                                                   ifelse(LINEAGE_model=="BA5", "BA4+5",
                                                          ifelse(LINEAGE_model=="BQ1__BA2_75", "BQ1+BA2.75",
                                                                 ifelse(LINEAGE_model=="XBB_", "XBB*",
                                                                        ifelse(LINEAGE_model=="BA2_86", "BA2.86",
                                                                               LINEAGE_model))))))))]
vocdata2[, variant2b:= factor(variant2, levels = unique(variant2))]

datesVariantsEmerging = vocdata2[proportion>0.05 & date>as_date("2020-12-01") &proportion<0.7 , .(date, proportion, variant2)][duplicated(variant2)==F][order(date)]
datesVariantsEmerging


## # load tensor data ####
todo = data.table(tensor_fn = dir(here("data/FIGURE_and_SUPPLEMENT_general/"), pattern = "Simulations"))

todo[, id := tensor_fn %>% str_replace("SimulationsLand_", "") %>% str_split( "_|[A-Z]") %>% sapply("[", 1)]

todo[, region := federal_countryinfo[match_hk(id, federal_countryinfo$ID), region]]



todo = todo[region=="Germany"]



all_extracted_data = lapply(todo$region, function(myregion){
  # myregion = todo$region[6]
  mytensor  = load_obj(paste0(here("data/FIGURE_and_SUPPLEMENT_general//",todo[region == myregion,tensor_fn])))
  mydata = extractTensordata(tensor = mytensor, firstdate = as_date("2020-03-04"), scenarioname = myregion)
  
  res= c()
  res$compartmentdata = mydata$compartmentdata
  res$incidencedata = mydata$incidencedata
  res$variantdata = mydata$variantdata
  res
}) 

names(all_extracted_data) = todo$region

###############
# altcode ----
# plot compartments -----
# 

plot_all_immunstates <- function(extracted_data, region, federal_countryinfo, epidata,maxdate_diagrams= maxdate_diagrams) {
  # extracted_data = all_extracted_data[["Germany"]]
  # region = "Germany"
  maxdate = maxdate_diagrams
  myextracted_data = copy(extracted_data)
  myregion = region
  
  n_gesamtbevoelkerung = federal_countryinfo[match_hk(myregion, federal_countryinfo$region), ew2021 ]
  stopifnot(n_gesamtbevoelkerung >0)
  
  ## epidata regonalize
  myepidata = epidata[region==myregion][DateRep <=maxdate]
  
  
  vaccdata = melt(myepidata, id.vars = c("region","DateRep", "Altersgruppe"), measure.vars = c("AllVaccinations_vollrel", 'AllVaccinations_1boosterrel')) #
  vaccdata[,variable2:= ifelse(variable=="AllVaccinations_vollrel", "Basic Immunisation", 
                               ifelse(variable =="AllVaccinations_1boosterrel", "Booster 1+2", variable))]
  vaccdata[,variable2:= factor(variable2, levels = c("Basic Immunisation", "Booster 1+2"))]
  
  vaccdata[, Altersgruppe := factor(Altersgruppe, levels = c("A00-A14","A15-A34",  "A35-A59",  
                                                             "A60-A79", "A80+","all", "unbekannt"))]
  
  
  
  
  ## modelled 
  
  plotdat2a = myextracted_data$compartmentdata[agegroup!="all", .(value= sum(value, na.rm = T)), .(datum, compartmentGrober)] # ungleich all sonst doppelt
  plotdat2a[, value_norm := value]# * n_gesamtbevoelkerung/sum(value), datum]
  
  
  
  datesVariant5proz_model = myextracted_data$variantdata[dataVariant_anteil>0.05 , .(date=datum, proportion=dataVariant_anteil, variant2)][duplicated(variant2)==F][order(date)][variant2!="WT"]
  
  datesVariant5proz_model[, date_5proz_reported := datesVariantsEmerging[match_hk(datesVariant5proz_model$variant2, datesVariantsEmerging$variant2),date]]
  datesVariant5proz_model
  
  plotdat2a[, compartmentGrober := factor(compartmentGrober, levels = c("S/Vac0", 
                                                                        "Infected", "D", "Vac1", "R1", "Vac2","R2", "Vac3",  "R3"))]
  
  p5b = ggplot(plotdat2a, aes(datum, value_norm, fill = compartmentGrober)) + 
    geom_col(width = 1) +
    # scale_fill_paletteer_d("trekcolors::lcars_cardassian", direction = -1) +
    scale_fill_manual(values = c("coral3","red", "grey89", "darkblue",  "steelblue3", 'darkgreen',"green",  "#7C6A0A","#F7C548", "grey"))+
    scale_y_continuous(breaks = seq(0, n_gesamtbevoelkerung, n_gesamtbevoelkerung/10) %>% signif(2), sec.axis = sec_axis(~ ./n_gesamtbevoelkerung, name = "Population", label = label_percent(), breaks = seq(0,1,0.1))) + xlab("") +
    
    # geom_vline(xintercept = as_date("2022-03-31"), lty = 3) +
    geom_vline( xintercept = datesVariant5proz_model$date, lty = 2) +
    annotate("text",x = datesVariant5proz_model$date+7, y = rep(n_gesamtbevoelkerung/27.7, nrow(datesVariant5proz_model)),label = datesVariant5proz_model$variant2, angle = 90) +
    theme_minimal(base_size = 14) + ylab("Individuals") +
    coord_cartesian(xlim = c(min(plotdat2a$datum), maxdate))
    theme(legend.position = c(0.2,0.5), 
          legend.background = element_rect(fill = "grey95"), 
          legend.title = element_blank(), 
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ),
          axis.title.y.right = element_blank()
    )
  p5b
  
  
  
  p7 =ggplot(vaccdata[Altersgruppe == "all" & value >0 & DateRep<= maxdate], aes(DateRep, value, lty = variable2 )) +
    geom_line(lwd=1) + 
    theme_minimal(base_size = 14) + 
    scale_y_continuous(breaks = pretty_breaks(5), 
                       labels = label_percent(accuracy = 1), 
                       sec.axis = dup_axis()) + 
    ylab("Vaccinations") +
    scale_x_date( limits = range(vaccdata$DateRep))+
    theme(axis.title.x = element_blank(),
          legend.position = c(0.21, 0.7),
          legend.key.height = unit(x = 0.3, units = "cm"),
          axis.text.x = element_blank(),
          axis.title.y.right = element_blank()) + 
    labs(lty = "")+
    geom_vline( xintercept = datesVariant5proz_model$date, lty = 2, alpha = 0.5) + 
    coord_cartesian(xlim = c(min(plotdat2a$datum), maxdate))
  
  p7
  
  
  
  # add relative total MODELLED infections----
  plotdat2b = myextracted_data$incidencedata[, .(value= sum(value, na.rm = T)), .(datum,  agegroup2)][agegroup2=="all"]
  
  population = federal_countryinfo[region==myregion, ]
  stopifnot(nrow(population)==1)
  
  plotdat2b[, Ntotal := population$ew2021]
  
  setorder(plotdat2b, agegroup2,datum, na.last = T)
  plotdat2b[, cumvalue := cumsum(value), agegroup2]
  
  plotdat2b[, cumvalue_rel := cumvalue/Ntotal]
  setorder(plotdat2b, -cumvalue_rel)
  # plotdat2b[, agegroup2:= factor(agegroup2, levels = unique(agegroup2))]
  p9 =ggplot(plotdat2b[ , .(DateRep=datum, cumvalue_rel, agegroup2)], aes(DateRep,  cumvalue_rel, col = agegroup2 )) + 
    geom_line(lwd=1) + 
    theme_minimal(base_size = 14) + 
    scale_y_continuous(breaks = pretty_breaks(5), labels = label_percent(accuracy = 1), sec.axis = dup_axis()) + 
    ylab("modelled\ncumul.Infections") +
    scale_x_date( limits = range(vaccdata$DateRep), labels =  date_format("%b-%y"))+
    theme(axis.title.x = element_blank(),
          legend.position = c(0.16, 0.7),
          legend.key.height = unit(x = 0.3, units = "cm"),
          # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ),
          axis.text.x = element_blank(),
          axis.title.y.right = element_blank()
    ) + 
    labs(col = "")+
    scale_color_manual(values = c("black"))+
    geom_vline( xintercept = datesVariant5proz_model$date, lty = 2, alpha = 0.5) + 
    coord_cartesian(xlim = c(min(plotdat2a$datum), maxdate))
  
  p9
  
  
  p_infectedCompsDet = p5b / p7 /p9 + patchwork::plot_layout(heights = c(3,1,1))
  p_infectedCompsDet

}

p_infectedCompsDet_GER = plot_all_immunstates(extracted_data = all_extracted_data[["Germany"]], region = "Germany", federal_countryinfo, epidata,maxdate_diagrams= maxdate_diagrams)
p_infectedCompsDet_GER

jpeg2(here("results//FIGURE_4_immunstates_allcompartments_germany.jpeg"), 12,8)
p_infectedCompsDet_GER
dev.off()

pdf(here("results//FIGURE_4_immunstates_allcompartments_germany.pdf"), 12,8)
p_infectedCompsDet_GER
dev.off()


# for(i in todo$region) {
#   # # i = todo$region[7]
#   # extracted_data =res
#   message("plotting", i)
#   jpeg2(here(paste0("results//SUPPLEMENTimmunstates_allcompartments_", i, ".jpeg")), 12,8)
#   myplot =  plot_all_immunstates(extracted_data = all_extracted_data[[i]], region = i, federal_countryinfo, epidata,maxdate_diagrams= maxdate_diagrams)  + plot_annotation(title =i)
#   # myplot =  plot_all_immunstates(extracted_data = res, region = i, federal_countryinfo, epidata,maxdate_diagrams= maxdate_diagrams)  + plot_annotation(title =i)
#   
#   print(myplot )
#   dev.off()
# }

###################################################################.
# # Immune states infected per age group ####
###################################################################.


plot_infecteds_immunestate <- function(extracted_data, region,  epidata,maxdate_diagrams= maxdate_diagrams) {

  # extracted_data = all_extracted_data[["Germany"]]
  # region = "Germany"
  # 
  
  myextracted_data = copy(extracted_data)
  stopifnot(length(myextracted_data)>0)
  myregion = region
  myepidata = copy(epidata[region ==myregion])
  maxdate= maxdate_diagrams
  
  epidatam = melt(myepidata, id.vars = c("DateRep", "Altersgruppe","Ntotal", "region"), measure.vars = c("NewConfCases7","NewNormalStation7", "covid_inICU_upscaled", 'NewDeaths7'))
  epidatam[, outcome:= ifelse(variable =='NewConfCases7', "reported",
                             ifelse(variable =='NewNormalStation7', "Hospital",
                                    ifelse(variable =="covid_inICU_upscaled", "ICU",
                                           ifelse(variable =="NewDeaths7", "died", variable))))]
  epidatam[, .N, .(variable,outcome)]
  
  epidatam[,type := 'reported']
  
  # virus variant entry dates ---- 
  datesVariant5proz_model = myextracted_data$variantdata[dataVariant_anteil>0.05 , .(date=datum, proportion=dataVariant_anteil, variant2)][duplicated(variant2)==F][order(date)][variant2!="WT"]
  
  datesVariant5proz_model[, date_5proz_reported := datesVariantsEmerging[match_hk(datesVariant5proz_model$variant2, datesVariantsEmerging$variant2),date]]
  datesVariant5proz_model
  
  
  ## plot cumulative infections in Model
  plotdat2b = myextracted_data$incidencedata[, .(value= sum(value, na.rm = T)), .(datum,  agegroup2)]
  
  setorder(plotdat2b, agegroup2,datum, na.last = T)
  plotdat2b[, cumvalue := cumsum(value), agegroup2]
  
  
  ## get number people in each age group
  population = epidatam[,.(Altersgruppe, Ntotal)] %>% unique()
  stopifnot(all(plotdat2b$agegroup2 %in% population$Altersgruppe))
  
  plotdat2b[, Ntotal := population[match_hk(plotdat2b$agegroup2, population$Altersgruppe), Ntotal]]
  plotdat2b[, cumvalue_rel := cumvalue/Ntotal]
  setorder(plotdat2b, -cumvalue_rel)
  
  
  plotdat2b[, agegroup2:= factor(agegroup2,  levels = c("A00-A14","A15-A34",  "A35-A59", "A60-A79", "A80+","all", "unbekannt"))]
  p9 =ggplot(plotdat2b[ , .(DateRep=datum, cumvalue_rel, agegroup2)], aes(DateRep,  cumvalue_rel, col = agegroup2 )) + geom_line(lwd=1) + theme_minimal(base_size = 14) + scale_y_continuous(breaks = pretty_breaks(5), labels = label_percent(accuracy = 1)) + ylab("cumul.Infections") +
    scale_x_date( labels =  date_format("%b-%y"))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ), 
          legend.position = c(0.16, 0.7),
          legend.key.height = unit(x = 0.3, units = "cm")
    ) + 
    labs(col = "")+
    geom_vline( xintercept = datesVariant5proz_model$date, lty = 2, alpha = 0.5) + 
  coord_cartesian(xlim = c(min(plotdat2b$datum), maxdate))
  
  p9
  # plot immune states -----
  
  myextracted_data$incidencedata[, agegroup2 := factor(agegroup2, levels = c("A00-A14","A15-A34",  "A35-A59", "A60-A79", "A80+","all", "unbekannt"))]
  plotdat2c = myextracted_data$incidencedata[, .(value= sum(value, na.rm = T)), .(datum,immunstate2,  agegroup2)]
  
  
  
  
  p_infected_age =ggplot(plotdat2c[agegroup2 %nin% c("all", "unbekannt")], aes(datum, value,col = immunstate2, fill = immunstate2)) +
    geom_col(position = 'fill')  + 
    scale_color_wsj() +
    scale_fill_wsj() +
    facet_grid(agegroup2~., scales = "free")+
    # scale_y_log10(label= label_comma(accuracy = 1), sec.axis = dup_axis()) +
    scale_x_date(labels =  date_format("%b-%y"))+
    xlab("") +
    theme_minimal(base_size = 14) + ylab("") +
   
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ), 
          legend.position = "top") + 
    labs(col = "", lty = "", fill = "") +
    # coord_cartesian(xlim = c(min(epidatam$DateRep ), maxdate_diagrams))+
    ylab("Proportion within patient with active Infection")+
    # geom_vline(xintercept = as_date("2022-03-31"), lty = 3) +
    geom_vline( xintercept = datesVariant5proz_model$date, lty = 2) + 
    scale_y_continuous(label= label_percent(accuracy = 1), breaks = pretty_breaks(5))  + 
    coord_cartesian(xlim = c(min(plotdat2b$datum), maxdate)) 
  p_infected_age
  
  
  
  
  
  # add vaacination plot -----
  vaccdata = melt(myepidata, id.vars = c("region","DateRep", "Altersgruppe"), measure.vars = c("AllVaccinations_vollrel", 'AllVaccinations_1boosterrel')) #
  vaccdata[,variable2:= ifelse(variable=="AllVaccinations_vollrel", "Basic Immunisation", 
                               ifelse(variable =="AllVaccinations_1boosterrel", "Booster 1+2", variable))]
  vaccdata[,variable2:= factor(variable2, levels = c("Basic Immunisation", "Booster 1+2"))]
  
  vaccdata[, Altersgruppe := factor(Altersgruppe, levels = c("A00-A14","A15-A34",  "A35-A59",  
                                                             "A60-A79", "A80+","all", "unbekannt"))]
  
  
  setorder(vaccdata,variable2, -value, na.last = T)
  vaccdata[duplicated(Altersgruppe)==F]
  # vaccdata[,Altersgruppe := factor(Altersgruppe, levels = unique(Altersgruppe))]
  p7b =ggplot(vaccdata[Altersgruppe != "unbekannt"& DateRep<= maxdate], aes(DateRep, value, col = Altersgruppe, lty = variable2 )) + geom_line(lwd=1) + 
    theme_minimal(base_size = 14) + 
    scale_y_continuous(breaks = pretty_breaks(8), labels = label_percent(accuracy = 1) ) + 
    ylab("Vaccinations") +
    scale_x_date( labels =  date_format("%b-%y"))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = c(0.21, 0.7),
          legend.box = "horizontal",
          # legend.text = element_text(size = 7),
          legend.key.height = unit(x = 0.3, units = "cm"), 
          legend.title = element_blank()
    ) +
    # legend.position = "right") + 
    # labs(lty = "")+
    geom_vline( xintercept = datesVariant5proz_model$date, lty = 2, alpha = 0.5)  + 
    coord_cartesian(xlim = c(min(plotdat2b$datum), maxdate))
  
  p7b
  
  p_comp_infected =  p_infected_age /p7b/ (p9+
                                             annotate("text",x = datesVariant5proz_model$date+7, y = rep(1, nrow(datesVariant5proz_model)),label = datesVariant5proz_model$variant2, angle = 90, size = 3))  + patchwork::plot_layout(heights = c(3,1,1))
  p_comp_infected

}

# plot for germany
p_comp_infected_germany = plot_infecteds_immunestate( extracted_data = all_extracted_data[["Germany"]], region = "Germany",  epidata, maxdate_diagrams = maxdate_diagrams )
p_comp_infected_germany


# plot for all regions

# for(i in todo$region) {
#   # # i = todo$region[9]
#   message("plotting ", i)
#   jpeg2(here(paste0("results//SUPPLEMENT_infected_compartment_immunstates", i, ".jpeg")), 11,9)
#   myplot =  plot_infecteds_immunestate(extracted_data = all_extracted_data[[i]], region = i, epidata,maxdate_diagrams= maxdate_diagrams)  + plot_annotation(title =i)
#   print(myplot )
#   dev.off()
# }








##########################################################################################.
# # finalize Skript
##########################################################################################.

finalizeSkript()

