#' ---
#' output:
#' 
#'   html_document:
#'     keep_md: true
#'     toc: true
#'     toc_float: true
#'     code_folding: hide
#' ---
#'

#################################################################################.
# # INITIALIZE ####
#################################################################################.
rm(list = ls())
knitr::opts_chunk$set(cache = F, results = "markup", echo=T, fig.height= 8, fig.width = 12)


library(data.table)
library(lubridate)
library(toolboxH) #https://github.com/holgerman/toolboxH
library(magrittr)
library(ggplot2)
library(scales)
library(plotly)
library(here)
library(lemon)
library(patchwork)
library(boot)
require(plotly)


maxdate = as_date("2022-09-30") # last date of data


#################################################################################.
# # LOAD data ####
#################################################################################.

## Testing number and positivity from RKI ####
## per region
fn_positivdata = here("data/FIGURE_and_SUPPLEMENT_parameter/data_wochenbericht_2023-KW02.xlsx")# https://ars.rki.de/Content/COVID19/Reports.aspx
fn_positivdata
testzahlenGER = read_excel2(fn_positivdata, "Abb.1 Gesamt", skip = 4) 
testzahlenGER$Bundesland  = "Deutschland"

testzahlenBL = read_excel2(fn_positivdata, "Abb. 3 Bundesland", skip = 4)

testzahlen = rbind(testzahlenGER, testzahlenBL)

## per age
testzahlenAGE = read_excel2(fn_positivdata, "Abb. 6 - 9 Altersgruppe", skip = 4)
testzahlenAGE[, Alter := factor(Alter, levels = unique(Alter))]
testzahlenAGE[, jahrwoche := paste(`Jahr KW Entnahme`, `KW Entnahme`)] 



## # reported Testpositivees from RKI ####
# original data now availabe via https://github.com/robert-koch-institut/ 

inz = fread(here("data/FIGURE_and_SUPPLEMENT_general/s1030_2_datint_ecdc_DE_BL_2023-03-26_v5_agestrat.txt"), dec = ",")[Altersgruppe =="all", .(DateRep, CountryExp, Altersgruppe, Ntotal, NewConfCases,NewConfCases7, AllConfCases)][is.na(NewConfCases)==F]
setorder(inz, CountryExp, Altersgruppe, DateRep)
inz[, AllConfCasesRel := AllConfCases/Ntotal]
inz[, isGermany :=CountryExp=="Deutschland"]
inz[grep("Baden", CountryExp), CountryExp := "Baden-Württemberg"]
inz[grep("^Th", CountryExp), CountryExp := "Thüringen"]


## demografical data ####
# total N people per federal country 
ew = structure(list(bl = c("Nordrhein-Westfalen", "Bayern", "Baden-Württemberg", 
                           "Niedersachsen", "Hessen", "Rheinland-Pfalz", "Sachsen", "Berlin", 
                           "Schleswig-Holstein", "Brandenburg", "Sachsen-Anhalt", "Thüringen", 
                           "Hamburg", "Mecklenburg-Vorpommern", "Saarland", "Bremen"), ew2021 = c(17925000L, 
                                                                                                  13177000L, 11125000L, 8027000L, 6295000L, 4106000L, 4043000L, 
                                                                                                  3677000L, 2922000L, 2538000L, 2169000L, 2109000L, 1854000L, 1611000L, 
                                                                                                  982000L, 676000L)), class = "data.frame", row.names = c(NA, -16L
                                                                                                  )) %>% data.table()

# age-specific people per federal country 

destatis = read_excel2(here("data/SUPPLEMENT_estimation_darknumber/DeStatis_12411-0012.xlsx"), "R")
destatis[, model_categ2 := str_replace_all(model_categ, "A0", "")]
destatis[, model_categ2 := str_replace_all(model_categ2, "A", "")]
destatis[, model_categ2 := str_replace_all(model_categ2, "80\\+", ">=80")]

included_regions = c("Deutschland", "Rheinland-Pfalz", "Bayern", "Baden-Württemberg", "Thüringen", "Sachsen-Anhalt", "Niedersachsen", "Brandenburg", "Sachsen", "Hessen", "Berlin", "Nordrhein-Westfalen", "Bremen",   "Schleswig-Holstein", "Hamburg", "Mecklenburg-Vorpommern", "Saarland")

 
destatism = melt(destatis, id.vars = c('model_categ2' ,'datenstand', 'quelle'), variable.name = "Bundesland", value.name = "Bevoelkerung", measure.vars = included_regions)
destatism2 = destatism[,.(N = sum(Bevoelkerung)), .(region= Bundesland, Alter=model_categ2,  datenstand, quelle)]
destatism2[,Bevoelkerung_stand := "2020-12-31"]

destatism3 = destatism2
destatism3[,Anteil := N/sum(N), .(region)]
destatism3


namingtable = fread(here("data/FIGURE_and_SUPPLEMENT_general/federal_countries.csv"))
#################################################################################.
# # DATA WRANGLING ####
#################################################################################.

# english names for federal countries
destatism[, region := namingtable[match_hk(destatism$region, namingtable$german), region]]
destatism2[, region := namingtable[match_hk(destatism2$region, namingtable$german), region]]
destatism3[, region := namingtable[match_hk(destatism3$region, namingtable$german), region]]
ew[, bl := namingtable[match_hk(ew$bl, namingtable$german), region]]
inz[, CountryExp := namingtable[match_hk(inz$CountryExp, namingtable$german), region]]
testzahlen[, Bundesland := namingtable[match_hk(testzahlen$Bundesland, namingtable$german), region]]
testzahlenBL[, Bundesland := namingtable[match_hk(testzahlenBL$Bundesland, namingtable$german), region]]


# more wrangling 
datehelper = data.table(DatumDesMittwochs = as_date("2020-01-08") + (0:200)*7 )
datehelper[, wochentag := weekdays(DatumDesMittwochs)]
datehelper[, wochennum  := isoweek(DatumDesMittwochs)]
datehelper[, jahr  := year(DatumDesMittwochs)]
datehelper[,jahrwoche := paste(jahr, wochennum)]

testzahlen[, jahrwoche := paste(`Jahr KW Entnahme`, `KW Entnahme`)] 
testzahlen[, DatumDesMittwochs := datehelper[match_hk(testzahlen$jahrwoche, datehelper$jahrwoche), DatumDesMittwochs]]

ew2 = rbind(ew, data.table(bl = "Germany", ew2021 = sum(ew$ew2021)))

testzahlen[, Einwohner := ew2[match_hk(testzahlen$Bundesland ,ew2$bl),ew2021]]
testzahlen2pre = testzahlen[, .(
  Bundesland, 
  Kalenderwoche = jahrwoche,
  DatumDesMittwochs ,
  Anzahl_Gesamt = `Anzahl Gesamt`,
  Einwohner,
  `Anzahl Testungen pro 1k` = `Anzahl Gesamt`/(Einwohner/1000),
  `Positiv getestet` = `Anzahl Gesamt`* `Anteil positiv`,
  `Positivenanteil (%)` = `Anteil positiv`,
  `Anzahl übermittelnder Labore` = NA,
  TestsProLabor =  NA,
  woche =`KW Entnahme` %>% as.numeric(),
  jahr = `Jahr KW Entnahme`%>% as.numeric()
)
]
testzahlen2pre[,.N, jahr]
if(any(as_date(0) %in% testzahlen2pre$DatumDesMittwochs)) stop("Checke DAtumskonversion")

testzahlen2 = testzahlen2pre[Bundesland %nin% "unknown"]
testzahlen2[,.N, Bundesland]

if(any(as_date(0) %in% testzahlen2$DatumDesMittwochs)) stop("Checke DAtumskonversion")

testzahlen2[`Positivenanteil (%)`>0, logit_positivanteil :=boot::logit(`Positivenanteil (%)`) ]

testzahlen2[, AllConfCasesRel := inz[match_hk(testzahlen2[,paste(Bundesland, DatumDesMittwochs)],
                                               inz[,paste(CountryExp, DateRep)]),AllConfCasesRel]]

testzahlen2[, AllConfCases := inz[match_hk(testzahlen2[,paste(Bundesland, DatumDesMittwochs)],
                                           inz[,paste(CountryExp, DateRep)]),AllConfCases]]


testzahlen2[, isGermany :=Bundesland=="Germany"]

#################################################################################.
# # CALIBRATION FACTOR SETTING ----
#################################################################################.

# This was set so that the dark figure is in line with reported serostatus and expert statements
# Serostatus see below, expert status e.g. Personal Communication Berit Lange in accordance with multiple expert statements: in the first months of the 2nd wave approx ratio was all/reported 2-3, see email Sent: Thursday, 14 April 2022 12:58. dark figure is all/reported -1, others, see # Max 3 https://www.zdf.de/nachrichten/panorama/corona-dunkelziffer-dreimal-so-hoch-pcr-100.html

# the dark figure is calculated as the ratio of all cases to reported cases -1,i.e. a dark figure of 1 means one undetectable case for each reported case

center_used_logit = 1
sd_for_scale_used_logit = 3
scale_used_logit = testzahlen2[, sd(logit_positivanteil ,na.rm = T)*sd_for_scale_used_logit]
scale_used_logit

testzahlen2[,logit_positivanteil_scaled :=  scale(logit_positivanteil , scale = scale_used_logit)+center_used_logit]  

testzahlen2[,logit_positivanteil_scaled :=  ifelse(logit_positivanteil_scaled<0, 0, logit_positivanteil_scaled)] # verwendet

# input provided positivit rate

p0 =ggplot(testzahlen2, aes(DatumDesMittwochs, `Positivenanteil (%)`, col = Bundesland, alpha  = isGermany,lwd = isGermany)) +
  
  geom_line() + 
  theme_minimal(base_size = 22) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = label_comma(accuracy = 0.1)) + 
  scale_x_date(breaks = date_breaks("2 months")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  ylab('Dark number estimate\n(all age groups)') + xlab("") +
  # geom_vline(xintercept = as_date("2022-09-30"), lty = 3) +
  labs(color ="Federal State") + 
  guides(alpha = "none", lwd = "none")+
  # ggtitle("", paste0("via logit using scale SD(logit_positivanteil) *", sd_for_scale_used_logit %>% signif(.,4), " and center ", center_used_logit%>% signif(.,4)))+ 
  scale_linewidth_manual(values = c(0.8,2)) + scale_alpha_manual(values = c(0.3,1)) +
  coord_cartesian(xlim = as_date(c(min(testzahlen2$DatumDesMittwochs)-30, maxdate)), expand = 0)

p0
ylabelpos = c(0.001,0.005,0.01, 0.025, 0.05, 0.1, 0.25 , 0.5,   0.7, 0.8, 0.9, 0.95)
p0 = ggplot(testzahlen2, aes(DatumDesMittwochs, boot::logit(`Positivenanteil (%)`), col = Bundesland, alpha = isGermany, lwd = isGermany)) +
  geom_line() + 
  theme_minimal(base_size = 22) + 
  scale_y_continuous(
    breaks = boot::logit(ylabelpos), 
    labels = scales::percent(ylabelpos)
  ) + 
  scale_x_date(breaks = date_breaks("2 months")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab('Reported testpositiviy\n(all age groups, logit scale)') + 
  xlab("") +
  labs(color = "Federal State") + 
  guides(alpha = "none", lwd = "none") +
  scale_linewidth_manual(values = c(0.8, 2)) + 
  scale_alpha_manual(values = c(0.3, 1)) +
  coord_cartesian(
    xlim = as_date(c(min(testzahlen2$DatumDesMittwochs)-30, maxdate)), 
    expand = 0
  )
p0

# Recalculate cumulative cases considering dark figure

setorder(testzahlen2, Bundesland, DatumDesMittwochs, na.last = T)

testzahlen2[, NewConfCases := c(AllConfCases[1],diff(AllConfCases)), Bundesland]
testzahlen2[, cumsumtest := cumsum(NewConfCases), Bundesland]
stopifnot(identical((testzahlen2[, AllConfCases - cumsumtest] %>% na.omit() %>% unique()), 0L))

testzahlen2[, NewConfCases_upscaled := NewConfCases*(logit_positivanteil_scaled+1)]

testzahlen2[, AllConfCases_upscaled := cumsum(NewConfCases_upscaled), Bundesland]

testzahlen2[, AllConfCasesRel_upscaled := AllConfCases_upscaled/Einwohner ]

testzahlen2[, DateRep:= DatumDesMittwochs]


p1 =ggplot(testzahlen2, aes(DateRep , AllConfCasesRel_upscaled, col = Bundesland, alpha  = isGermany,lwd = isGermany)) +
  geom_line() + 
  theme_minimal(base_size = 22) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = label_comma(accuracy = 0.1)) + 
  scale_x_date(breaks = date_breaks("2 months")) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  # annotate(size = 3.3,"rect", ymax = Inf, ymin = -Inf, xmin = as_date("2021-11-01"), xmax = as_date("2021-12-15"), alpha = 0.2, col = NA,fill = "coral")   +
  # annotate(size = 3.3,"rect", ymax = Inf, ymin = -Inf, xmin = as_date("2020-03-01"), xmax = as_date("2021-12-15"), alpha = 0.2, col = NA,fill = "coral")+
  ylab('Cumulative relative cases\n(accounting for dark number)') + xlab("") +
  
  # ggtitle("", paste0("via logit using scale SD(logit_positivanteil) *", sd_for_scale_used_logit%>% signif(.,4), " and center ", center_used_logit%>% signif(.,4)))+ 
  # geom_vline(xintercept = as_date("2022-09-30"), lty = 3) +
  scale_linewidth_manual(values = c(0.8,2)) + scale_alpha_manual(values = c(0.3,1)) +
  labs(color ="Federal State") + 
  guides(alpha = "none", lwd = "none")+
  geom_line(
    data = data.table(DateRep = as_date(c("2022-06-01", "2022-09-30")), AllConfCasesRel_upscaled = 0.444, CountryExp="Germany", isGermany=T), alpha = 0.9, color = "purple2") +
  annotate(size = 3.3,geom = "text", x = as_date("2022-06-01"), y = 0.46, label = "IMMUNEBRIDGE Payback", color = "purple2" ) + #https://www.aerzteblatt.de/archiv/230671/Grundimmunitaet-gegen-SARS-CoV-2-in-der-deutschen-Bevoelkerung
  
  geom_line(data = data.table(DateRep = as_date(c("2020-10-01", "2021-02-28")), AllConfCasesRel_upscaled = 0.018, CountryExp="Germany", isGermany=T),alpha = 0.9, color = "purple2") +
  annotate(size = 3.3,geom = "text", x = as_date("2020-06-11"), y = 0.05, label = "RKI-SOEP\nnon-healthcareworkers", color = "purple2") + # https://www.aerzteblatt.de/int/archive/article/222155
  
  geom_line(data = data.table(DateRep = as_date(c("2022-04-24", "2022-05-21")), AllConfCasesRel_upscaled = 0.477, CountryExp="Germany", isGermany=T),alpha = 0.9, color = "purple2") +
  annotate(size = 3.3,geom = "text", x = as_date("2022-03-01"), y = 0.5, label = "RKI-SeBluCo\nblood donors", color = "purple2") +# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/SeBluCo_Zwischenbericht.html
  
  geom_line(data = data.table(DateRep = as_date(c("2021-09-05", "2021-09-18")), AllConfCasesRel_upscaled = 0.086, CountryExp="Germany", isGermany=T),alpha = 0.9, color = "purple2") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-09-01"), y = 0.11, label = "RKI-SeBluCo blood donors", color = "purple2") + # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/SeBluCo_Zwischenbericht.html
  
  annotate(size = 3.3,"rect", ymax = .378, ymin = .274, xmin = as_date("2021-07-01"), xmax = as_date("2021-07-31"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-07-31"), y = c(0.3), label = "M:Chemnitz2", color = "grey22") +# DOI: 10.3238/arztebl.m2021.0364 
  
  annotate(size = 3.3,"rect", ymax = .143, ymin = .094, xmin = as_date("2021-05-01"), xmax = as_date("2021-05-31"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-06-30"), y = c(0.15), label = "M:Greifswald1", color = "grey22") +
  
  annotate(size = 3.3,"rect", ymax = .037, ymin = .097, xmin = as_date("2021-04-01"), xmax = as_date("2021-04-30"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-07-01"), y = c(0.08), label = "M:Magdeburg2", color = "grey22") +
  
  
  annotate(size = 3.3,"rect", ymax = .115, ymin = .149, xmin = as_date("2021-03-01"), xmax = as_date("2021-03-31"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-01-31"), y = c(0.15), label = "M:Chemnitz1", color = "grey22") +
  
  annotate(size = 3.3,"rect", ymax = .032, ymin = .052, xmin = as_date("2021-03-01"), xmax = as_date("2021-03-31"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-06-15"), y = c(0.03), label = "M:Osnabrueck2", color = "grey22") +
  
  annotate(size = 3.3,"rect", ymax = .065, ymin = .044, xmin = as_date("2021-02-01"), xmax = as_date("2021-02-28"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-02-01"), y = c(0.07), label = "M:Aachen2", color = "grey22") +
  
  annotate(size = 3.3,"rect", ymax = .019, ymin = .031, xmin = as_date("2020-11-01"), xmax = as_date("2020-11-30"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2021-01-01"), y = c(-0.01), label = "M:Freiburg2\nM:Magdeburg1", color = "grey22") +
  
  annotate(size = 3.3,"rect", ymax = .021, ymin = .037, xmin = as_date("2020-11-01"), xmax = as_date("2020-11-30"),alpha = 0.4, fill = "grey22") +
  annotate(size = 3.3,geom = "text", x = as_date("2020-09-01"), y = c(0.01), label = "", color = "grey22") +
  coord_cartesian(xlim = as_date(c(min(testzahlen2$DateRep)-30, maxdate)), expand = 0)

p1


mws = testzahlen2[  DatumDesMittwochs<=as_date("2022-09-30") ,.(mw_bis_sept22 =mean(logit_positivanteil_scaled, na.rm=T )), .(Bundesland, isGermany)]
mws

stopifnot(`Calibrated FActor for unreporteds (=Dunkelziffer) estimated from logit scaled positivity rates should be  between 1 and 3 ...stoppping....`=(mws[, min(mw_bis_sept22)] >0) &
            mws[, max(mw_bis_sept22)]<2) # 


min_max_mittwoch = testzahlen2[DatumDesMittwochs<=as_date("2022-09-30"),.(DatumDesMittwochs=DatumDesMittwochs[DatumDesMittwochs %in% c(min(DatumDesMittwochs), max(DatumDesMittwochs))]), .(Bundesland)]


mws2 = merge(mws, min_max_mittwoch, by = "Bundesland")

p2 =ggplot(testzahlen2, aes(DatumDesMittwochs, logit_positivanteil_scaled, col = Bundesland, alpha  = isGermany,lwd = isGermany)) +
  geom_line(data = mws2[, .(DatumDesMittwochs,logit_positivanteil_scaled = mw_bis_sept22, Bundesland, isGermany)]) +  
  geom_line() + 
  theme_minimal(base_size = 22) + 
  scale_y_continuous(breaks = pretty_breaks(10), labels = label_comma(accuracy = 0.1)) + 
  scale_x_date(breaks = date_breaks("2 months")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  ylab('Dark number estimate\n(all age groups)') + xlab("") +
  # geom_vline(xintercept = as_date("2022-09-30"), lty = 3) +
  labs(color ="Federal State") + 
  guides(alpha = "none", lwd = "none")+
  # ggtitle("", paste0("via logit using scale SD(logit_positivanteil) *", sd_for_scale_used_logit %>% signif(.,4), " and center ", center_used_logit%>% signif(.,4)))+ 
  scale_linewidth_manual(values = c(0.8,2)) + scale_alpha_manual(values = c(0.3,1)) +
  coord_cartesian(xlim = as_date(c(min(testzahlen2$DateRep)-30, maxdate)), expand = 0)

p2 


#################################################################################.
# # ADD AGE specificity ----
#################################################################################.
testzahlenAGE[, DatumDesMittwochs := datehelper[match_hk(testzahlenAGE$jahrwoche, datehelper$jahrwoche), DatumDesMittwochs]]
setnames(testzahlenAGE, "Bevölkerungszahl", 'Bevoelkerungszahl')

## # merging reported age groups 0-4  and  5-14 age to age group 0-15 used in model ####
testzahlenAGE_0_14 = testzahlenAGE[Alter %in% c('0-4', "5-14"), .(Alter = "0-14",
                                                                  Bevoelkerungszahl = sum(Bevoelkerungszahl),
                                                                  `Anzahl Gesamt` = sum(`Anzahl Gesamt`),
                                                                  `Anteil positiv` = sum(`Anteil positiv`*`Anzahl Gesamt`)/sum(`Anzahl Gesamt`)
),
.(jahrwoche,`Jahr KW Entnahme`,
  `KW Entnahme`, DatumDesMittwochs)]

testzahlenAGE_15plus = testzahlenAGE[Alter %nin% c('0-4', "5-14")]

testzahlenAGE2 = rbind(testzahlenAGE_0_14, testzahlenAGE_15plus)

testzahlenAGE2[DatumDesMittwochs ==DatumDesMittwochs[1]]

testzahlenprops_tested = testzahlenAGE2[, .(Alter, anteil = `Anzahl Gesamt`/sum(`Anzahl Gesamt`)), .(DatumDesMittwochs)]
testzahlenprops_tested[, sum(anteil), DatumDesMittwochs][, table(V1)] 
testzahlenprops_tested$type = "Anteil getestet DE"


## # Transfering Germanys age specific testpositivity rate to federal countries on the logit scale ----

# assuming the same age proportion of tested and the same age distribution among positives in Federal Countries as in Germany 

testzahlenBL[, jahrwoche := paste(`Jahr KW Entnahme`, `KW Entnahme`)] 
testzahlenBL[, DatumDesMittwochs := datehelper[match_hk(testzahlenBL$jahrwoche, datehelper$jahrwoche), DatumDesMittwochs]]

testzahlenBL2 = merge(testzahlenprops_tested[, .(DatumDesMittwochs, Alter)], testzahlenBL, by = 'DatumDesMittwochs', allow.cartesian=TRUE)

testzahlenBL2[,id := paste(DatumDesMittwochs, Alter)]



testzahlenDE= testzahlen[Bundesland =="Germany"]

testzahlenBL2[, `Anteil positiv DE` := testzahlenDE[match_hk(testzahlenBL2$DatumDesMittwochs, testzahlenDE$DatumDesMittwochs),`Anteil positiv`]]
testzahlenAGE2[, id := paste(DatumDesMittwochs, Alter)]
testzahlenBL2[, `Anteil positiv agegroup DE` := testzahlenAGE2[match_hk(testzahlenBL2$id, testzahlenAGE2$id),`Anteil positiv`]]


changeViaLogiDiff = function(gesamtDE, gesamtBL, ageDE) {
  # gesamtDE = 0.4
  # gesamtBL = 0.3
  # ageDE = (1:5)/10
  
  diff_logit = boot::logit(gesamtBL) - boot::logit(gesamtDE)
  ageDE_logit = boot::logit(ageDE) 
  ageDEcorr_logit =  ageDE_logit + diff_logit
  ageDEcorr = boot::inv.logit(ageDEcorr_logit)
  ageDEcorr
}


testzahlenBL2[, anteil_positiv_tests_agegroup_logitmeth := changeViaLogiDiff(
  gesamtDE = `Anteil positiv DE`, 
  ageDE = `Anteil positiv agegroup DE`, 
  gesamtBL =  `Anteil positiv` 
  
)]
 

testzahlenBL2$Bundesland %>% sort() %>% unique()
testzahlenAGE_all= rbind(testzahlenBL2[Bundesland != "unknown", .(anteil_positiv_tests = anteil_positiv_tests_agegroup_logitmeth, 
                                            region = Bundesland,
                                            DatumDesMittwochs,
                                            Alter, 
                                            typ = "alterspezifisch"
)], 
testzahlenAGE2[, .(anteil_positiv_tests = `Anteil positiv`, 
                   region = "Germany",
                   DatumDesMittwochs,
                   Alter,
                   typ = "alterspezifisch"
)],
testzahlen2pre[Bundesland != "unknown",.(anteil_positiv_tests = `Positivenanteil (%)`,
               region = Bundesland,
               DatumDesMittwochs, 
               Alter = "all",
               typ = "gesamt")])

## Assumed testpositivity federal states----


p3 = ggplot(testzahlenAGE_all, aes(DatumDesMittwochs, anteil_positiv_tests, col = Alter,lwd = typ, alpha = typ )) + 
  geom_line()  + 
  scale_size_manual(values = c(1.2, 2.2))+
  scale_alpha_manual(values = c(0.5,0.9)) + 
  scale_color_viridis_d(direction = -1) + 
  theme_minimal(base_size = 22) + 
  scale_y_continuous(labels =  percent_format(accuracy = 1), breaks = pretty_breaks(10), sec.axis = dup_axis()) + 
  facet_wrap(~region %>% str_wrap(width = 10,whitespace_only=F) , nrow = 2)+ 
  scale_linewidth_manual(values = c(0.8,2)) +
  guides(lwd = "none", alpha = "none") + 
  labs(x = "", col = "Age", y = "Estimated percentage positive tests")+
  theme(legend.position = "top", axis.title.y.right = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_cartesian(xlim = as_date(c(min(testzahlenAGE_all$DatumDesMittwochs)-30, maxdate)), expand = 0)
p3
  

## age- and time-resolved dark figure estimation federal states----

testzahlenAGE_all$region %>% unique()
testzahlenAGE_all[,logit_anteil_positiv_tests :=   boot::logit(anteil_positiv_tests)] 
testzahlenAGE_all[anteil_positiv_tests!=0,dunkelziffer_aus_posrate_logit :=  scale(boot::logit(anteil_positiv_tests), scale = scale_used_logit)+center_used_logit] # verwendet

testzahlenAGE_all[,dunkelziffer_aus_posrate_logit :=  ifelse(dunkelziffer_aus_posrate_logit<0, 0, dunkelziffer_aus_posrate_logit)] # verwendet

testzahlenAGE_all[anteil_positiv_tests==0,dunkelziffer_aus_posrate_logit :=0]

p4 = ggplot(testzahlenAGE_all, aes(DatumDesMittwochs, dunkelziffer_aus_posrate_logit, col = Alter, lwd = typ, alpha = typ )) + 
  geom_line()  + 
  scale_size_manual(values = c(1.2, 2.2))+
  scale_alpha_manual(values = c(0.5,0.9)) + 
  scale_color_viridis_d(direction = -1) + 
  theme_minimal(base_size = 22) + 
  scale_y_continuous(breaks = pretty_breaks(5), sec.axis = dup_axis()) + 
  facet_wrap(~region %>% str_wrap(width = 10,whitespace_only=F) , nrow = 2)+ 
  scale_linewidth_manual(values = c(0.8,2)) +
  guides(lwd = "none", alpha = "none") + 
  labs(x = "", col = "Age", y = "Estimated dark figure")+
  theme(legend.position = "top", axis.title.y.right = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_cartesian(xlim = as_date(c(min(testzahlenAGE_all$DatumDesMittwochs)-30, maxdate)), expand = 0)

p4

#################################################################################.
# # COMBINED FIGURE ####
#################################################################################.
p_suppl  =(p0 + guides(col = "none"))/(p2+ guides(col = "none"))/p1/(p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") + theme(legend.position = "top")


#################################################################################.
# # saving ----
#################################################################################.
jpeg2 = function(fn, myres = 600, ...) jpeg(fn, quality = 100, unit ="in", res = myres, ...)

jpeg2(here("results//SUPPLEMENT_estimation_darknumber_part1.jpeg"), height = 24, width = 24)
p_suppl
dev.off()

pdf(here("results//SUPPLEMENT_estimation_darknumber_part1.pdf"), height = 24, width = 24)
p_suppl
dev.off()


fwrite(testzahlenAGE_all,here("results//SUPPLEMENT_estimation_darknumber_part1.txt"))
#################################################################################.
# # FINALIZE  -------
#################################################################################.
finalizeSkript()
