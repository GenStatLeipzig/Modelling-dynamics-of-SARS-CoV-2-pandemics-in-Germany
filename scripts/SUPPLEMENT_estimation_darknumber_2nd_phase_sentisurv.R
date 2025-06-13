########################################### .
# Initialize Script ----
########################################### .
rm(list = ls())
library(ggplot2)
library(patchwork)
library(data.table)
library(readxl)
library(here)
library(lubridate)
library(plotly)
library(scales)
  library(stringr)

maxplotdata  <-  as_date("2024-04-01")
Sys.setlocale("LC_TIME", "en_US")
# maxplotdata = as_date("2024-09-19")
## special functions ----
# typically provided by custom R-library but included as function here for demonstration purposes

# improved reading of excel files, essential a data.table-friendly update of readxl::read_excel()
read_excel2 <- function(fn, sheet = 1, skip = 0, na = "", ...) {
  sheetnames <- readxl:::xlsx_sheets(fn)
  if (is.numeric(sheet)) {
    sheetnum <- sheet
  } else {
    sheetnum <- which(sheetnames == sheet)
  }
  if (is.numeric(sheetnum) == FALSE) {
    stop("not numeric sheetnum created in function read_excel2, please debug")
  }
  withWarnings <- function(fn, sheet = sheet) {
    myWarnings <- NULL
    wHandler <- function(w) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(readxl::read_excel(fn, sheet,
                                                  skip = skip, na = na, ...
    ), warning = wHandler)
    list(value = val, warnings = myWarnings)
  }
  myfile_list <- withWarnings(fn, sheet)
  newwarning <- sapply(myfile_list$warnings, "[[", 1)
  if (any(grepl("expecting", newwarning))) {
    newwarning <- grep("expecting", newwarning, value = TRUE)
    cols2text <- sort(as.numeric(unique(sapply(stringr::str_split(
      newwarning,
      " |\\]"
    ), "[", 2))))
    message("Reimporting \n", fn, "\n with column(s) ", paste(cols2text,
                                                              collapse = ", "
    ), " as type text, so you can ignore the warnings from read_excel import...")
    col_types <- readxl:::xlsx_col_types(path = fn, sheet = sheetnum -
                                           1, n = 1000, nskip = 1)
    stopifnot(length(col_types) == ncol(myfile_list$value))
    col_types[cols2text] <- "text"
    myfile <- readxl::read_excel(fn,
                                 sheet = sheet, col_types = col_types,
                                 skip = skip, na = na, ...
    )
  } else {
    myfile <- myfile_list$value
  }
  data.table::setDT(myfile)
  myfile
}

# not in list operation
`%nin%` <- function (x, y) !(x %in% y)

# move column to first position
moveColFront <- function (d = dataframe, colname = "colname") {
  stopifnot(all(colname %in% names(d)))
  index <- match(colname, names(d))
  old_order <- 1:ncol(d)
  new_order <- c(index, old_order[old_order %nin% index])
  if (data.table::is.data.table(d)) 
    data.table::setcolorder(d, new_order)
  if (data.table::is.data.table(d) == F) 
    d = d[, new_order]
  return(d)
}

# show all duplicated entries in a vector
allDuplicatedEntries <- function(vektor) {
  if (length(vektor) == 0) {
    return(0)
  }
  vektab <- data.table::data.table(myvektor = vektor, num = 1:length(vektor))
  duplicated_vals <- vektab[duplicated(myvektor), myvektor]
  duplicated_entries <- vektab[myvektor %in% duplicated_vals]
  data.table::setkey(duplicated_entries, myvektor)
  duplicated_entries$num
}

match_hk <- function(x, y, testunique = TRUE, makeunique = FALSE, importcol = NULL,
                     showMessages = TRUE, ...) {
  yname <- deparse(substitute(y))
  if (testunique == TRUE) {
    check <- as.numeric(sum(duplicated(stats::na.omit(y))))
    if (identical(check, 0)) {
      return(match(x, y, incomparables = c(NA, NaN), ...))
    }
    if (identical(check, 0) == FALSE & makeunique == FALSE) {
      if (showMessages == TRUE) {
        message("Duplicated entries:\n", paste(y[duplicated(y)],
                                               collapse = "\n"
        ))
      }
      stop(paste(yname, "ist nicht unique"))
    }
    if (identical(check, 0) == FALSE & makeunique == TRUE) {
      if (is.null(importcol)) {
        stop("When asking for make unique, please provide vector with values to be imported")
      }
      if (length(importcol) != length(y)) {
        stop("When asking for make unique, please provide vector with values to be imported")
      }
      datatable_da <- "data.table" %in% rownames(installed.packages())
      datatable_da
      if (datatable_da) {
        matcher <- unique(data.table::data.table(
          index = y,
          importcol = importcol
        ))
        matcher <- matcher[index %in% x, ]
        matchercheck <- matcher[, as.numeric(sum(duplicated(stats::na.omit(index))))]
        if (identical(matchercheck, 0) == FALSE) {
          if (showMessages == TRUE) {
            print(matcher[allDuplicatedEntries(matcher$index)])
          }
          stop(paste(yname, "ist nicht unique after trying to make index and importcol unique..."))
        }
      }
      if (datatable_da == FALSE) {
        matcher <- unique(data.frame(index = y, importcol = importcol))
        matcher <- matcher[matcher$index %in% x, ]
        matchercheck <- as.numeric(sum(duplicated(stats::na.omit(matcher$index))))
        if (identical(matchercheck, 0) == FALSE) {
          if (showMessages == TRUE) {
            print(matcher[allDuplicatedEntries(matcher$index), ])
          }
          stop(paste(yname, "ist nicht unique after trying to make index and importcol unique..."))
        }
      }
      return(match(x, y, incomparables = c(NA, NaN), ...))
    }
  }
  if (testunique == FALSE) {
    return(match(x, y, incomparables = c(NA, NaN), ...))
  }
}

## extend geom_smooth
# https://stackoverflow.com/questions/26705554/extend-geom-smooth-in-a-single-direction

## decorate lm object with a new class lm_right
lm_right <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_right", class(mod))
  mod
}

## decorate lm object with a new class lm_left
lm_left <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_left", class(mod))
  mod
}
# Then for each method we create a predict_df specialization where we truncate the x values in the opposite side.
predictdf_lm_right <-
  function(model, xseq, se, level) {
    ## here the main code: truncate to x values at the right
    init_range <- range(model$model$x)
    xseq <- xseq[xseq >= init_range[1]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
# Same thing for the left extension :

predictdf_lm_left <-
  function(model, xseq, se, level) {
    init_range <- range(model$model$x)
    ## here the main code: truncate to x values at the left
    xseq <- xseq[xseq <= init_range[2]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
########################################### .
# Read input -----
########################################### .

## epidemiological RKI data ----
# source https://github.com/robert-koch-institut, reorganized and provided in script s210_4__agestrat_testpositives_deaths_rki_github.R
alldata <- fread(here("data/SUPPLEMENT_estimation_darknumber/s210_4_rki_eingangsdatum_positives_dead_agegroups_2024-09-10.txt.gz"))

# subset on federal country Rheinland Pfalz (RPL) and Germany
alldata_rpl <- alldata[Bundesland == "Rheinland-Pfalz" & agegroup == "all" & region_level == "Bundesland" & datatype == "as_registered"]
alldata_rpl[allDuplicatedEntries(DateRep)]

alldata_de <- alldata[agegroup == "all" & region_level == "Land" & datatype == "as_registered"]


## RPL Sentisurv  ----
# source Sentisurv: https://www.unimedizin-mainz.de/SentiSurv-RLP/dashboard/index.html#tage-inzidenz
sentisurv_fn = here( "data/SUPPLEMENT_estimation_darknumber/rpl_SentiSurv.xlsx")
sentisurv <- read_excel2(sentisurv_fn, "timeseries")
sentisurv[, datum := as_date(datum)]

# add 7d100k from RKI data
sentisurv[, RPL_RKI_7d100k := alldata_rpl[match_hk(sentisurv$datum %>% as_date(), alldata_rpl$DateRep %>% as_date()), NewConfCases7d100k]]
sentisurv[, DE_RKI_7d100k := alldata_de[match_hk(sentisurv$datum %>% as_date(), alldata_de$DateRep %>% as_date()), NewConfCases7d100k]]


setnames(sentisurv, "7-Tage-Inzidenz", "RPL_SentiSurv_7d100k")

## Dark number from sero- and testpositivity ----
old_dn <- fread(here("data/SUPPLEMENT_estimation_darknumber/DunkelzifferSTANDARD_DEF_DE_BL2023-06-01_V2.txt"))
old_dn[, date_of_wednesday := as_date(DatumDesMittwochs)]

old_dn2 <- old_dn[Bundesland == "Rheinland-Pfalz" & date_of_wednesday > as_date("2022-06-01")]
old_dn2$typ <- "RPL Dark figure from testpositivity"


sentisurv[, ratio_rpl := RPL_SentiSurv_7d100k / RPL_RKI_7d100k]

## plot imported data ----
sentisurvm <- melt(sentisurv, id.vars = "datum", measure.vars = c("RPL_SentiSurv_7d100k", "RPL_RKI_7d100k", "DE_RKI_7d100k", "ratio_rpl"))

p9 <- ggplot(sentisurvm, aes(datum %>% as_date(), value, col = variable)) +
  geom_line(data = sentisurvm[variable != "ratio_rpl"]) +
  geom_point() +
  scale_y_log10(breaks = log_breaks(15), labels = label_comma(accuracy = 1), sec.axis = dup_axis()) +
  theme_minimal(base_size = 22) +
  scale_x_date(breaks = date_breaks(width = "2 months"), labels = label_date(format = "%y-%b-%d"), limits = c(as_date("2022-08-01"),maxplotdata)) +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "top", legend.key.width = unit(4, "line"), panel.grid.major = element_line(), panel.spacing.y = unit(2, "cm"), panel.background = element_rect(fill = alpha("grey90", 0.17), color = "white"))

p9 <- ggplot(sentisurvm, aes(datum %>% as_date(), value, col = variable)) +
  geom_line(data = sentisurvm[variable != "ratio_rpl"]) +
  geom_point() +
  scale_y_log10(breaks = log_breaks(15), labels = label_comma(accuracy = 1), 
                sec.axis = dup_axis()) +
  theme_minimal(base_size = 22) +
  scale_x_date(breaks = date_breaks(width = "2 months"), 
               labels = label_date(format = "%y-%b-%d"), 
               limits = c(as_date("2022-08-01"), maxplotdata)) +

  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = "top", 
        legend.key.width = unit(4, "line"), 
        panel.grid.major = element_line(), 
        panel.spacing.y = unit(2, "cm"), 
        panel.background = element_rect(fill = alpha("grey90", 0.17), 
                                        color = "white"))

## plot scaled
p9_2 <- p9 + geom_line(data = old_dn2[, .(datum = date_of_wednesday, value = Dunkelziffer_all + 1, variable = typ)]) + ggtitle("Dark figure calibration via SentiSurv Rheinland-Pfalz", subtitle = "Shown are dark ratio, i.e. dark figure +1") 
p9_2


## import age specific sentisurv ----
senti_age <- read_excel2(sentisurv_fn, "ages")

alldata_rpl_age <- alldata[Bundesland == "Rheinland-Pfalz" & region_level == "Bundesland" & datatype == "as_registered" & DateRep >= as_date("2023-10-01")]


########################################### .
# Defining Dark figure ----
########################################### .

## phase 1 Sentisurv-----
sentisurv_dates_phase1 <- sentisurvm[datum >= as_date("2022-11-15") & datum <= as_date("2023-04-30"), unique(datum)]

limits_phase1 <- c(as_date("2022-11-15"), as_date("2023-05-31"))

## phase 2 senti-----
sentisurv_dates_phase2 <- sentisurvm[datum >= as_date("2023-06-01") & datum <= as_date("2023-08-15"), unique(datum)]

summerlevel <- sentisurv[datum %in% sentisurv_dates_phase2, mean(ratio_rpl)] %>% round(-1)
summerlevel

19517 %>% as_date()
limits_phase2 <- c(as_date("2023-06-01"), as_date("2023-08-15"))

## phase 3 senti ----
sentisurv_dates_phase3 <- sentisurvm[datum >= as_date("2023-08-15") & datum <= as_date("2023-08-31"), unique(datum)]

limits_phase3 <- c(as_date("2023-08-16"), as_date("2023-08-31"))

## phase 4 senti ----
sentisurv_dates_phase4 <- sentisurvm[datum >= as_date("2023-09-01") & datum <= as_date("2024-02-14"), unique(datum)]

autumnwinterlevel <- 75
limits_phase4 <- c(as_date("2023-09-01"), as_date("2024-02-14"))

## phase 5 senti ----
sentisurv_dates_phase5 <- sentisurvm[datum >= as_date("2024-02-14") & datum <= as_date("2024-03-31"), unique(datum)]
limits_phase5 <- c(as_date("2024-02-15"), as_date("2024-03-31"))

summerlevel2 = 225
## phase 6 senti ----
sentisurv_dates_phase6 <- sentisurvm[datum >= as_date("2024-03-31") & datum <= maxplotdata, unique(datum)]


limits_phase6 <- c(as_date("2024-04-01"), as_date("2024-04-15"))



## combining all phases ----

phase_pre <- data.table(old_dn2[date_of_wednesday <= min(limits_phase1), .(datum = date_of_wednesday, phase = "Dark ratio from testpositivity", value = Dunkelziffer_all + 1)])

phase_pre

phase_1 <- data.table(datum = seq(from = min(limits_phase1), to = max(limits_phase1), by = 1), phase = "SentiSurv phase 1")

phase_1[, value := seq(last(phase_pre$value) %>% log10(), summerlevel %>% log10(), by = (summerlevel %>% log10() - last(phase_pre$value) %>% log10()) / (.N - 1)) %>% 10^.]


phase_2 <- data.table(
  datum = seq(from = min(limits_phase2), to = max(limits_phase2), by = 1),
  phase = "SentiSurv phase 2",
  value = summerlevel
)


phase_3 <- data.table(datum = seq(from = min(limits_phase3), to = max(limits_phase3), by = 1), phase = "SentiSurv phase 3")

phase_3[, value := seq(last(phase_2$value) %>% log10(), autumnwinterlevel %>% log10(), by = (autumnwinterlevel %>% log10() - last(phase_2$value) %>% log10()) / (.N - 1)) %>% 10^.]


phase_4 <- data.table(
  datum = seq(from = min(limits_phase4), to = max(limits_phase4), by = 1),
  phase = "SentiSurv phase 4",
  value = autumnwinterlevel
)


phase_5 <- data.table(
  datum = seq(from = min(limits_phase5), to = max(limits_phase5), by = 1),
  phase = "SentiSurv phase 5"
)

phase_5[, value := seq(last(phase_4$value) %>% log10(), summerlevel2 %>% log10(), by = (summerlevel2 %>% log10() - last(phase_4$value) %>% log10()) / (.N - 1)) %>% 10^.]

phase_6 <- data.table(
  datum = seq(from = max(limits_phase5), to = max(limits_phase6), by = 1),
  phase = "SentiSurv phase 5",
  value = summerlevel2
)
phase_6



dark_ratio_rpl <- rbind( phase_1, phase_2, phase_3, phase_4, phase_5, phase_6)
dark_ratio_rpl[, variable := "Estimated Dark ratio for Model"]



  p_rpl <- p9_2 +
    geom_point(data = dark_ratio_rpl, lwd = 2.5, alpha = 0.5) + geom_vline(xintercept = c(limits_phase1, limits_phase2, limits_phase3, limits_phase4, limits_phase5, limits_phase6), lty = 3) +  scale_x_date(date_labels = "%b-%y", # This gives format like "Dec-22"
                                                                                                                                                                                                              date_breaks = "1 month") 
  p_rpl

  p_rpl2 = p_rpl +   # Add better legend labels
    scale_color_discrete(labels = c(
      "RPL_SentiSurv_7d100k" = "Rhineland-Palatinate SentiSurv Incidence",
      "RPL_RKI_7d100k" = "Rhineland-Palatinate RKI Incidence",
      "DE_RKI_7d100k" = "Germany RKI Incidence",
      "ratio_rpl" = "Calculated Ratio Sentisurv / RKI",
      "RPL Dark figure from testpositivity" = "Test-positivity-based Dark Figure",
      "Estimated Dark ratio for Model" = "Phase-wise Ratio Sentisurv / RKI"
    )) + labs(col = "") + ggtitle("", subtitle = "")
  p_rpl2 
jpeg(here(paste0("results//SUPPLEMENT_FIG_estimated_darknumber_sentisurv_all_ages.jpeg")), width = 16+8, height = 8, units = "in", res = 300)
p_rpl2
dev.off()


# > AGE specific rpl -----
sentiage <- read_excel2(sentisurv_fn, "ages")
sentiage[, datum := as_date(datum)]


categ_matcher <- read_excel2(sentisurv_fn, "categ_matcher")
categ_matcher[, categ_match := paste(categ_sentisurv, categ_rki)]


sentiage[, categ_rki := categ_matcher[match_hk(sentiage$agegroup, categ_matcher$categ_sentisurv, makeunique = TRUE, importcol = categ_matcher$categ_rki), categ_rki]]
sentiage[, categ_match := paste(agegroup, categ_rki, sep = " vs. \n")]

sentiage[, id2 := paste(datum, categ_rki)]

agedata_rpl <- alldata[Bundesland == "Rheinland-Pfalz" & region_level == "Bundesland" & datatype == "as_registered"]

agedata_rpl[, id2 := paste(DateRep, agegroup)]


sentiage[, inz_rki := agedata_rpl[match_hk(sentiage$id2, agedata_rpl$id2), NewConfCases7d100k]]

sentiage[, ratio := Inz7d100k / inz_rki]

sentiagem <- melt(sentiage, id.vars = c("datum", "agegroup", "categ_match"), measure.vars = c("Inz7d100k", "inz_rki", "ratio"))
sentiagem[, datatype := ifelse(grepl("inz", variable, ignore.case = TRUE), "7-day-inz-100k", "ratio")]

sentiagem[, variable2 := ifelse(variable == "inz_rki", "RKI RPL",
                                ifelse(variable == "Inz7d100k", "SentiSurv RPL", "SentiSurv/RKI ratio")
)]

sentiagem[, variable2 := factor(variable2, levels = c("SentiSurv RPL", "RKI RPL", "SentiSurv/RKI ratio"))]

## add categs not included and all
sentisurvm_adder <- sentisurvm[datum %in% sentiagem$datum & variable != "DE_RKI_7d100k"]
sentisurvm_adder[, datatype := ifelse(grepl("7d100", variable, ignore.case = TRUE), "7-day-inz-100k", "ratio")]

sentisurvm_adder$variable %>% table()
sentisurvm_adder[, variable2 := ifelse(variable == "RPL_RKI_7d100k", "RKI RPL",
                                       ifelse(variable == "RPL_SentiSurv_7d100k", "SentiSurv RPL", ifelse(variable == "ratio_rpl", "SentiSurv/RKI ratio", variable))
)]

sentisurvm_adder[, variable2 := factor(variable2, levels = c("SentiSurv RPL", "RKI RPL", "SentiSurv/RKI ratio"))]
sentisurvm_adder[, agegroup := "all"]
sentisurvm_adder[, categ_rki := "all"]

sentisurvm_adder[, categ_match := paste(agegroup, categ_rki, sep = " vs. \n")]

## and very young RKI
agedata_rpl$agegroup %>% unique()
agedata_rpl_adder <- agedata_rpl[
  DateRep %in% sentiagem$datum & agegroup %in% c("A00-A04", "A05-A14"),
  .(
    datum = DateRep %>% as_date(),
    categ_rki = agegroup,
    agegroup = ".NA",
    variable = "NewConfCases7d100k",
    value = NewConfCases7d100k,
    variable2 = "RKI RPL",
    datatype = "7-day-inz-100k"
  )
]
agedata_rpl_adder[, categ_match := paste(agegroup, categ_rki, sep = " vs. \n")]

destatism <- fread(here("data/SUPPLEMENT_estimation_darknumber/s205_1_destatis_12411-0012_kreise_plus_categs.txt"))

destatism2 <- destatism[region_name == "Rheinland-Pfalz", .(bevoelkerung = sum(bevoelkerung)), .(rki_categ)]

destatism2
destatism2_bis4 <- destatism2[rki_categ %in% c("A00-A04"), bevoelkerung]
destatism2_5bis14 <- destatism2[rki_categ %in% c("A05-A14"), bevoelkerung]


agedata_rpl_adder2 <- dcast.data.table(agedata_rpl_adder, datum + agegroup + variable + variable2 + datatype ~ categ_rki, value.var = "value")

agedata_rpl_adder2[, value := (`A00-A04` * destatism2_bis4 + `A05-A14` * destatism2_5bis14) / (destatism2_bis4 + destatism2_5bis14)]

agedata_rpl_adder2[, categ_rki := "A00-A14"]
agedata_rpl_adder2[, categ_match := paste(agegroup, categ_rki, sep = " vs. \n")]
sentiagem2 <- rbind(
  sentiagem,
  sentisurvm_adder[, names(sentiagem), with = FALSE],
  agedata_rpl_adder[, names(sentiagem), with = FALSE],
  agedata_rpl_adder2[, names(sentiagem), with = FALSE]
)




library(ggthemes)

category_labels_dt <- data.table(
  categ_match = c(
    "25-34 vs. \nA15-A34",
    "35-44 vs. \nA35-A59", 
    "45-54 vs. \nA35-A59",
    "55-64 vs. \nA60-A79",
    "65-74 vs. \nA60-A79", 
    "75+ vs. \nA80+",
    "all vs. \nall",
    ".NA vs. \nA00-A04",
    ".NA vs. \nA05-A14",
    ".NA vs. \nA00-A14"
  ),
  new_label = c(
    "SentiSurv 25-34 yrs\nvs RKI 15-34 yrs",
    "SentiSurv 35-44 yrs\nvs RKI 35-59 yrs",
    "SentiSurv 45-54 yrs\nvs RKI 35-59 yrs", 
    "SentiSurv 55-64 yrs\nvs RKI 60-79 yrs",
    "SentiSurv 65-74 yrs\nvs RKI 60-79 yrs",
    "SentiSurv 75+ yrs\nvs RKI 80+ yrs",
    "All Ages\n(SentiSurv vs RKI)",
    "SentiSurv NA yrs\nvs RKI 0-4 yrs",
    "SentiSurv NA yrs\nvs RKI 5-14 yrs",
    "SentiSurv NA yrs\nvs RKI 0-14 yrs"
  )
)

category_labels_dt
# qlist55 = venn2(sentiagem2$categ_match, category_labels_dt$categ_match)
sentiagem2[, categ_match2 := category_labels_dt[match_hk(sentiagem2$categ_match, category_labels_dt$categ_match),new_label]]

sentiagem2[, datatype2 := fifelse(datatype == "7-day-inz-100k", "7-day-incidence per 100k", "SentiSurv/RKI ratio")]



p_age_senti <- ggplot(sentiagem2, aes(datum, value, col = agegroup, lty = variable2)) +
  geom_line(lwd = 2) +
  theme_minimal(base_size = 21) +
  # ggtitle("SentiSurv Rheinland-Pfalz") +
  scale_colour_viridis_d(direction = -1) +
  scale_y_log10(breaks = log_breaks(10), labels = label_comma(accuracy = 1), sec.axis = dup_axis()) +
  facet_grid(datatype2 ~ categ_match2, space = "free", scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "top", legend.key.width = unit(4, "line"), panel.grid.major = element_line(), panel.spacing.y = unit(2, "cm"), panel.background = element_rect(fill = alpha("grey90", 0.17), color = "white")) +
  labs(lty = "") +
  scale_x_date(date_labels = "%b-%y", # This gives format like "Dec-22"
               date_breaks = "1 month") +
  guides(col = "none") + xlab("")

p_age_senti


p_age_senti2 = p_age_senti + scale_linetype_discrete(labels = c(
  "SentiSurv RPL" = "SentiSurv Rhineland-Palatinate",
  "RKI RPL" = "RKI Rhineland-Palatinate",
  "SentiSurv/RKI ratio" = "SentiSurv to RKI Ratio Rhineland-Palatinate"
)) 
p_age_senti2

jpeg(here(paste0("results//SUPPLEMENT_FIG_estimated_darknumber_sentisurv_agespecific_ages.jpeg")),  width = 65, height = 28, units = "cm", res = 300)
# jpeg(here(paste0("zeitreihen/results/datapipeline/s240_3_sentisurv_agespecific_rpl_",maxplotdata,".jpg")), width = 55, height = 28, units = "cm", res = 300)
p_age_senti2
dev.off()
 



# ratios agespecific ----------------------------------------------------------

sentiagem2[, categ_rki := str_split(categ_match, "\\\n") %>% sapply(., "[", 2)]
sentiagem2[, .N, variable]
sentiagem3 <- sentiagem2[datatype == "ratio", .(
  mean = mean(value),
  variable2 = unique(variable2)
), categ_rki]

sentiagem3[, categ_type := ifelse(grepl("all", categ_rki), "all", "agegroup")]
sentiagem3[, RelRatio := mean / mean[categ_rki == "all"]]

adder_0bis14 <- data.table(
  categ_rki = "A00-A14",
  RelRatio = 1.5 * sentiagem3[categ_rki == "A15-A34", RelRatio]
) # siehe inzidenz. ist bei A0 bis A05 gleich hoch, aber bei a05 bis A14 deutlich kleiner

relratios_age <- rbind(
  adder_0bis14,
  sentiagem3[, .(categ_rki, RelRatio)]
)

relratios_age[, dark_ratio_autumn := autumnwinterlevel * RelRatio]

dark_ratio_rpl[, categ_rki := "all"]

dark_ratio_rpl2b <- lapply(relratios_age$categ_rki %>% unique(), function(mycateg) {
  print(mycateg)
  resi <- dark_ratio_rpl[grepl("Senti", phase)][, RelRatio := relratios_age[categ_rki == mycateg, RelRatio]][, value2 := value * RelRatio]
  resi[, categ_rki := mycateg]
}) %>%
  rbindlist()

dark_ratio_rpl2b[, .N, categ_rki]
dark_ratio_rpl2b[, value := NULL]
setnames(dark_ratio_rpl2b, "value2", "dark_ratio")

dark_ratio_rpl2b[, weekday := weekdays(datum)]
dark_ratio_rpl2b2 <- dark_ratio_rpl2b[weekday == "Wednesday"]


dark_ratio_rpl2a <- melt(old_dn[date_of_wednesday < min(phase_1$datum)], id.vars = c("Bundesland", "date_of_wednesday", "DatumDesMittwochs"))
dark_ratio_rpl2a[, dark_ratio := value + 1]



# update neue per RPL sentisurv
dark_ratio_rpl2b3 <- lapply(dark_ratio_rpl2a[Bundesland %in% c('Deutschland'), unique(Bundesland)], function(my_bl) {
  print(my_bl)
  dark_ratio_rpl2b2[, Bundesland := my_bl]
  copy(dark_ratio_rpl2b2)
}) %>% rbindlist()

dark_ratio_rpl2b3[, .N, Bundesland]
dark_ratio_rpl2b3[, .N, categ_rki]

modelcateg_renamer <- fread(here("data/SUPPLEMENT_estimation_darknumber/categs_destatis_epi_icu_etc.csv"))[, .(model_categ, dunkelziffer_categ)] %>% unique()
modelcateg_renamer


dark_ratio_rpl2b3[, dunkelziffer_categ := modelcateg_renamer[match_hk(dark_ratio_rpl2b3$categ_rki, modelcateg_renamer$model_categ), dunkelziffer_categ]]

dark_ratio_rpl2b3[, .N, .(categ_rki, dunkelziffer_categ)]

## zusammenfuegen----

dark_ratio_rpl2a[, DatumDesMittwochs := as_date(DatumDesMittwochs)]

dark_ratio_all <- rbind(
  # dark_ratio_rpl2a[, phase := "Dark ratio from testpositivity"],
  dark_ratio_rpl2b3[, .(Bundesland,
                        date_of_wednesday = datum,
                        DatumDesMittwochs  = as_date(datum),
                        variable = dunkelziffer_categ,
                        value = dark_ratio - 1,
                        dark_ratio,
                        phase
  )]
)


dark_ratio_all[value < 0, value := 0]
dark_ratio_all[dark_ratio < 1, dark_ratio := 1]

dark_ratio_all$phase %>% unique()

# dark_ratio_all[, value := ifelse(phase == "SentiSurv phase 1" & value < last(value[phase == "Dark ratio from testpositivity"]), last(value[phase == "Dark ratio from testpositivity"]), value), .(Bundesland, variable)]

# dark_ratio_all[, dark_ratio := ifelse(phase == "SentiSurv phase 1" & dark_ratio < last(dark_ratio[phase == "Dark ratio from testpositivity"]), last(dark_ratio[phase == "Dark ratio from testpositivity"]), dark_ratio), .(Bundesland, variable)]


p_check <- ggplot(dark_ratio_all[variable!="DatumDesMittwochs"], aes(date_of_wednesday, value, group = paste(Bundesland, variable), color = variable)) +

  geom_line(lwd = 2) +
  theme_minimal(base_size = 21) +
  scale_color_viridis_d(
    name = "",  # Optional: adds a title to the legend
    labels = c(
      "Dunkelziffer_>=80" = "Age 80+",
      "Dunkelziffer_0-14" = "Age 0-14",
      "Dunkelziffer_15-34" = "Age 15-34",
      "Dunkelziffer_35-59" = "Age 35-59",
      "Dunkelziffer_60-79" = "Age 60-79",
      "Dunkelziffer_all" = "All Ages"
    ),
    direction = -1
  ) +
  
  
  scale_y_continuous(breaks = pretty_breaks(10), labels = label_comma(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "top", legend.key.width = unit(4, "line"), panel.grid.major = element_line(), panel.spacing.y = unit(2, "cm"), panel.background = element_rect(fill = alpha("grey90", 0.17), color = "white")) +
  labs(lty = "") +
  scale_x_date(date_labels = "%b-%y", # This gives format like "Dec-22"
               date_breaks = "1 month") +
  labs(col = "") + xlab("") + ylab("Estimated Dark Figure\nGermany") +
  scale_linewidth_manual(values = c(1.5, 2.5)) 




p_check

jpeg(here("results//SUPPLEMENT_FIG_estimated_darknumber_sentisurv_agespecific_resultGER.jpeg"), width = 14, height = 7, units = "in", res = 300)
p_check 
dev.off()


# finalize ----
sessionInfo()

# finalize ----
sessionInfo()
