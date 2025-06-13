jpeg2 = function(...) {jpeg(..., units = "in", quality = 100, res = 600)}

extract_timeinvariant_parameter <- function(parameters_together_pre, indiv_age_independentparams_pre, fixed_general_parameter,federal_countryinfo,parameter_explanation) {
  ###############################
  ## Data wrangling and naming  ###############################
  
  stopifnot(all(indiv_age_independentparams_pre$Bundesland %in% federal_countryinfo$modelname))
  indiv_age_independentparams_pre[ ,region := federal_countryinfo[match_hk(indiv_age_independentparams_pre$Bundesland, federal_countryinfo$modelname), region] ]
  indiv_age_independentparams_pre$Bundesland = NULL
  
  stopifnot(identical(length(parameters_together_pre), nrow(federal_countryinfo[region != "unknown"])))
  names(parameters_together_pre) = federal_countryinfo[region != "unknown"][order(ID), region]
  
  # Age specific ###############################
  
  age_specific_params_pre = lapply(names(parameters_together_pre), function(myname) {
    # myname = names(parameters_together_pre)[1]
    mysub = parameters_together_pre[[myname]]
    cbind(region = myname, mysub$age_spec_param %>% as.data.table(keep.rownames = T))
  }) %>% rbindlist(use.names = T)
  
  age_specific_params = melt(age_specific_params_pre, id.vars = c("region", "rn"))
  setnames(age_specific_params, c("region","agegroup", "parameter",  "value"))
  age_specific_params$source = "parameters_together.RData, slot age_specific_params"
  age_specific_params$parameter %>% unique()
  
  age_specific_params[, parameter := as.character(parameter)]
  
  agerename = data.table(old = age_specific_params$agegroup %>% unique(),
                         new = c("A00-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
  )
  stopifnot(all(age_specific_params$agegroup %in% agerename$old))
  age_specific_params[, agegroup := agerename[match_hk(age_specific_params$agegroup, agerename$old),new]]
  age_specific_params[, specificity := "age, region"]
  
  
  ### Age independent ###############################
  
  # "age_spec_param"  "transformed_par" "estimated_dates"
  transformed_params_pre = lapply(names(parameters_together_pre), function(myname) {
    # myname = names(parameters_together_pre)[1]
    mysub = parameters_together_pre[[myname]]
    cbind(region = myname, (mysub$transformed_par %>% t()) %>% as.data.table(keep.rownames = T))
  }) %>% rbindlist(use.names = T)
  
  transformed_params = melt(transformed_params_pre, id.vars = c("region"))
  setnames(transformed_params, c("region",  "parameter",  "value"))
  
  transformed_params$parameter %>% unique()
  transformed_params$source = "parameters_together.RData, slot transformed parameters"
  transformed_params[, specificity := "region"]
  
  
  estimated_dates_pre = lapply(names(parameters_together_pre), function(myname) {
    # myname = names(parameters_together_pre)[1]
    mysub = parameters_together_pre[[myname]]
    cbind(region = myname, (mysub$estimated_dates  %>% t() %>% as.data.table(keep.rownames = T)))
  }) %>% rbindlist(use.names = T)
  
  estimated_dates = melt(estimated_dates_pre, id.vars = c("region"))
  setnames(estimated_dates, c("region",  "parameter",  "value"))
  
  estimated_dates$parameter %>% unique()
  estimated_dates$source = "parameters_together.RData, slot estimated dates"
  estimated_dates[, specificity := "region"]
  
  
  indiv_age_independentparams =  melt(indiv_age_independentparams_pre, id.vars = c("region"))
  setnames(indiv_age_independentparams, c("region",  "parameter",  "value"))
  
  indiv_age_independentparams$parameter %>% unique()
  indiv_age_independentparams$source = "indiv_age_independentparams.xlsx"
  indiv_age_independentparams[, specificity := "region"]
  
  # add region - unspecific and age-unsprecific parameters to region-specific parameters
  fixed_general_parameter2 = data.table(region = indiv_age_independentparams_pre$region)
  for(i in (fixed_general_parameter$parameter)) {
    fixed_general_parameter2[ ,(i) := fixed_general_parameter[parameter ==i, value] ]
  }
  
  preset_params =  melt(fixed_general_parameter2, id.vars = c("region"))
  setnames(preset_params, c("region",  "parameter",  "value"))
  
  preset_params$parameter %>% unique()
  preset_params$source = "fixed_general_parameter.xlsx"
  preset_params[, specificity := "none"]
  
  
  # combine without doublicates ###############################
  allparam = rbind(transformed_params[parameter %nin% c(indiv_age_independentparams$parameter)], 
                   estimated_dates[parameter %nin% c(indiv_age_independentparams$parameter)],
                   indiv_age_independentparams,
                   preset_params)
  allparam[, parameter := as.character(parameter)]
  
  stopifnot(nrow(allparam[allDuplicatedEntries(paste(parameter, region))])==0) 
  
  # add agegroup 
  allagegroups = data.table(mergecol = 1, agegroup = unique(age_specific_params$agegroup))
  allagegroups
  
  allparam$mergecol = 1
  allparam2 = merge(allparam, allagegroups, by = "mergecol", all = T, sort = F, allow.cartesian=TRUE)
  allparam2[,mergecol := NULL]
  
  # merge all ###############################
  
  allparam = rbind(allparam2, age_specific_params)
  allparam
  
  
  
  # Annotate and Check  ###############################
  allparam[, parameter := as.character(parameter)]
  allparam[, parameter2 := parameter_explanation[match_hk(allparam$parameter, parameter_explanation$Parameter),Description2]]
  
  
  parameter_explanation[, inData := Parameter    %in% allparam$parameter   ]
  parameter_explanation[,.N, .(version, inData)]
  sm = parameter_explanation[inData ==F & grepl("depricated", version)==F & grepl("_treat|^treat", Parameter)==F]
  if(nrow(sm)>0) print(sm)
  stopifnot(nrow(sm)==0)
  
  
  # add  demography and model ID and long parametername
  for (i in c("ew2021", "ID")) {
    allparam[ ,(i) := federal_countryinfo[match_hk(allparam$region, federal_countryinfo$region), get(i)] ]
  }
  
  allparam[, parameter2 := parameter_explanation[match_hk(allparam$parameter, parameter_explanation$Parameter),Description]]
  allparam[, parameter2 := factor(parameter2, levels = unique(parameter_explanation$Description) %>% rev())]
  
  allparam[, is_Germany := grepl("Germany", region)]
  
  allparam
}

extractTensordata <- function(tensor, firstdate, scenarioname = NULL, extractOutputlayer=T, showplots=F, maxdate = as_date("2024-03-31")) {
  message('starting extractTensordata from functions_model_241106.R') 
  # tensor = copy(tensor_DE);scenarioname = "tensor_DE"; firstdate = as_date("2020-03-04")
  
  # tensor=resi$tensor
  # tensor=mytensor
  # firstdate = as_date("2020-03-04")
  # scenarioname = "Germany"
  # extractOutputlayer=T;showplots=T
  message("Tensor has time-steps from ", tensor$start_step, " to ", tensor$end_step)
  #################################################.
  # >extract info about compartments----
  # early _e_ compartments
  sim_arri_e_pre = tensor$sim_arri_e # 3-dimensionaler Tensor von " Infectable " Kompartimenten
  dimnames(sim_arri_e_pre)[[1]] = 1:length(dimnames(sim_arri_e_pre)[[1]])
  
  sim_arri_e = reshape2::melt(sim_arri_e_pre) %>% data.table(keep.rownames = T)
  setnames(sim_arri_e, c("rn","time","compartment", "agegroup", "value"))
  stopifnot(all(c("S", "Vac0", "Vac1", "Vac2", "Vac3") %in% sim_arri_e$compartment))
  sim_arri_e[,compartment := factor(compartment, levels =c( "Vac1", "Vac2","Vac3", "Vac0","S", "R1", "R2", "R3"))]
  sim_arri_e[, .N, compartment]
  if(showplots==TRUE) sim_arri_e[,boxplot(value~compartment)$x]
  
  stopifnot(sim_arri_e[grepl("^R", compartment ), sum(value, na.rm = T)]==0)
  sim_arri_e = sim_arri_e[grepl("^R", compartment )==F] # kann raus, weil 0
  
  # unique(sim_arri_e$time)
  sim_arri_e[, time := as.numeric(time)]                  
  sim_arri_e=sim_arri_e[order(time)]
  sim_arri_e[, datum := firstdate-1 + time]
  
  stopifnot(all(paste0("age_", 1:5) %in% sim_arri_e$agegroup))
  
  sim_arri_e[, agegroup2:= ifelse(agegroup =="age_1", "A00-A14",
                                  ifelse(agegroup =="age_2", "A15-A34",
                                         ifelse(agegroup =="age_3", "A35-A59",
                                                ifelse(agegroup =="age_4", "A60-A79",
                                                       ifelse(agegroup =="age_5", "A80+",agegroup)))))]                  
  
  unique(sim_arri_e$compartment)
  
  sim_arri_e[,compartmentGrob := str_replace_all(compartment, "[0-9]", "")]
  unique(sim_arri_e$compartmentGrob)
  
  
  
  sim_arri_e[,compartmentGrober := ifelse(grepl("^E|^I|^C|^StCare",compartment), "Infected",
                                          ifelse(compartment %in% c("S","Vac0") , "S/Vac0",
                                                 
                                                 compartment %>% as.character()))]
  unique(sim_arri_e$compartmentGrober)
  
  
  unique(sim_arri_e$compartmentGrober)
  sim_arri_e[,.N,.(compartment, compartmentGrob, compartmentGrober)][order(compartmentGrober)] %>% data.frame()
  
  
  
  sim_arri_e[, variant:= "uninfected"]
  sim_arri_e[, variant2:= "uninfected"]
  
   # ### infected compartment----
  sim_arri_l_pre = tensor$sim_arri_l # 5-dimensionaler Tensor von " Infected " Kompartimenten
  dimnames(sim_arri_l_pre)[[1]] = 1:length(dimnames(sim_arri_l_pre)[[1]])
  
  
  sim_arri_l = reshape2::melt(sim_arri_l_pre) %>% data.table(keep.rownames = T)
  setnames(sim_arri_l, c("rn","time","compartment","immunstate","variant", "agegroup", "value"))
  
  sim_arri_l[, time := as.numeric(time)]
  unique(sim_arri_l$time)
  sim_arri_l=sim_arri_l[order(time)]
  sim_arri_l[, datum := firstdate-1 + time]
  
  stopifnot(all(paste0("age_", 1:5) %in% sim_arri_l$agegroup))

  sim_arri_l$compartment %>% unique()
  stopifnot(all(c("E","IS1","IS2","IS3","IS4","StCare","C1","C2","C3", "D", "R1","R2","R3") %in% sim_arri_l$compartment))
  
  
  
  sim_arri_l[, agegroup2:= ifelse(agegroup =="age_1", "A00-A14",
                                  ifelse(agegroup =="age_2", "A15-A34",
                                         ifelse(agegroup =="age_3", "A35-A59",
                                                ifelse(agegroup =="age_4", "A60-A79",
                                                       ifelse(agegroup =="age_5", "A80+",agegroup)))))]
  
  # sim_arri_l[, variant2 := NA_character_]  
  sim_arri_l[, variant2 := ifelse(variant=="alpha" & "engl" %in% variant, "WT",
                                  ifelse(variant=="Alpha", "alpha",
                                         ifelse(variant=="engl", "alpha",
                                                ifelse(variant=="Delta", "delta",
                                  ifelse(variant=="omicron", "BA1",
                                         ifelse(variant=="BA.1", "BA1",
                                                ifelse(variant=="BA.2", "BA2",
                                                       ifelse(variant=="BA5", "BA4+5",
                                                              ifelse(variant=="BQ1__BA2_75", "BQ1+BA2.75",
                                                                     ifelse(variant=="XBB_", "XBB*",
                                                                            ifelse(variant=="BA2_86", "BA2.86",
                                                                                   as.character(variant))))))))))))]
  recodervariant = sim_arri_l[, .N, .(variant,variant2)]
  
  message("--------------------------------------------\nRecoding variant names\n",paste(recodervariant$variant, collapse = ", ") ,"\nto \n", paste(recodervariant$variant2, collapse = ", "))
  factorlevels_variants =  sim_arri_l[, unique(variant2)]
  sim_arri_l[, variant2:= factor(variant2, levels = factorlevels_variants)]
  
  
  sim_arri_l[,compartmentGrob := str_replace_all(compartment, "[0-9]", "")]
  unique(sim_arri_l$compartmentGrob)
  # allcompartmentGrob

  sim_arri_l[,compartmentGrober := ifelse(grepl("^E|^I|^C|^StCare",compartment), "Infected",
                                          ifelse(compartment %in% c("S","Vac0") , "S/Vac0",
                                                 
                                                 compartment %>% as.character()))]

  
  unique(sim_arri_l$ageg)
  sim_arri_l[compartmentGrob  == "E", sum(value)]
  
  sim_arri_l[,.N,.(compartment, compartmentGrob, compartmentGrober)][order(compartmentGrober)] %>% data.frame()
  
  
  
  # merge sim_arri_e and sim_arri_l
  
  
  qlist5 = venn2(sim_arri_e$compartment, sim_arri_l$compartment, plotte = showplots)
  stopifnot(length(qlist5$q1)==0)
  
  
  sim_arri_e[, variant:= "uninfected"]
  sim_arri_e[, variant2:= "uninfected"]
  sim_arri_e[, immunstate2:= ifelse(compartment %in% c("S", "Vac0"), "Unvacc.",
                                    ifelse(compartment %in% c("Vac1", "Vac2"), "Vac1+2 (some waning)",
                                           ifelse(compartment %in% c("Vac3"), "Vac3 (more waning)",compartment)))]
  
  
  immutable = sim_arri_e[, .N, .(compartment, immunstate2)]
  immutable[, immunestate_yuri:= ifelse(compartment %in% c("S", "Vac0"), "Unv",
                                        ifelse(compartment %in% c("Vac1", "Vac2"), "Vac",
                                               ifelse(compartment %in% c("Vac3"), "Wan",compartment)))]
  
  
  sim_arri_l[, immunstate2:= immutable[match_hk(sim_arri_l$immunstate, immutable$immunestate_yuri, makeunique = T, importcol = immutable$immunstate2),immunstate2]]
  
  sim_arri_l[,.N, .(compartment,immunstate2, immunstate)] %>% data.frame()
  
  venn2(names(sim_arri_e), names(sim_arri_l), plotte = showplots)
  
  sim_arri_e[, immunstate:= immutable[match_hk(sim_arri_e$immunstate2, immutable$immunstate2, makeunique = T, importcol = immutable$immunestate_yuri),immunestate_yuri]]
 
  sim_arri_pre = rbind(sim_arri_e, sim_arri_l)
 
  
  # add all class
  stopifnot(all(grepl("all", sim_arri_pre$agegroup))==F)
  stopifnot(all(grepl("all", sim_arri_pre$agegroup2))==F)
  
  sim_arri_toadd = sim_arri_pre[,.(value = sum(value),
                                   agegroup   = "all",
                                   agegroup2   = "all"), .(time, datum,  compartment, variant, variant2 ,immunstate ,immunstate2 ,compartmentGrob ,compartmentGrober)]
  
  
  sim_arri = rbind(sim_arri_pre[, -'rn'],
                   sim_arri_toadd)
  setorder(sim_arri, datum)
  sim_arri[, compartmentGrober := factor(compartmentGrober, levels = unique(compartmentGrober))]
  levels(sim_arri$compartmentGrober)
  
  sim_arri[, outcome := ifelse(compartmentGrob %in% c("E", "IA", "IS", "StCare"), "infiziert",
                               ifelse(compartmentGrob=="C", "ICU",
                                      ifelse(compartmentGrob=="D", "verstorben",as.character(NA))))]
  
  
  
  sim_arri[, .N, .(immunstate,immunstate2)]
  
  
  allcompartment = c("S",
                     "Vac0", "E", "IA1", "IA2",
                     "IA3", "IS1", "IS2", "IS3",  "C1", "C2", "C3", "C4", "StCare",
                     "D","Vac3", "R3","Vac2", "R2",  "Vac1", "R1")
  sim_arri[,compartment := factor(compartment, allcompartment)]
  
  allcompartmentGrob = c("S", "E","I", "IA", "IS","StCare", "C", "D","Vac",  "R")
  sim_arri[, compartmentGrob := factor(compartmentGrob, levels = allcompartmentGrob)]
  
  allcompartmentGrober = c("S/Vac0", "Infected", "D",paste0("Vac", 1:3),  paste0("R", 1:3))
  sim_arri[, compartmentGrober := factor(compartmentGrober, levels = allcompartmentGrober)]
  
  
  if(is.null(scenarioname)==F ){
    sim_arri$scenario = scenarioname
    compartmentdata = moveColFront(sim_arri, 'scenario')[datum <= maxdate]
  } else compartmentdata = sim_arri[datum <= maxdate]
  
  
  ##################################################.
  # >extract info about inzident cases IN MODEL----
  # load reported data 
  simu_reported_pre =  tensor$sum_increas_IS %>% as.data.table(keep.rownames = T) # YURI sum_increas_IS ist zwei dimensionale Tensor für kumulative registrierte neue Fälle (ohne Meldenverzug, nach Zeit und Altersgruppe) #  sum_increas_IS muss durch procent_meas (% von gemessenen) multipliziert, um den Output zu bekommen. procent_meas ist gleich 1/(1+Dunkelziffer).  sum_increas_IS ist eine Counter von neuen Fällern.
  
  setnames(simu_reported_pre, c("num","immunstate","variant", "agegroup", "cumvalue"))
  
  simu_reported_pre[, immunstate2 := immutable[match_hk(simu_reported_pre$immunstate, immutable$immunestate_yuri, makeunique = T, importcol = immutable$immunstate2),immunstate2]]
  
  simu_reported_pre[, datum := firstdate-1 + as.numeric(num)]
  
  stopifnot(all(paste0("age_", 1:5) %in% simu_reported_pre$agegroup))
  
  
  simu_reported_pre[, agegroup2:= ifelse(agegroup =="age_1", "A00-A14",
                                         ifelse(agegroup =="age_2", "A15-A34",
                                                ifelse(agegroup =="age_3", "A35-A59",
                                                       ifelse(agegroup =="age_4", "A60-A79",
                                                              ifelse(agegroup =="age_5", "A80+",agegroup)))))] 
  
  
  simu_reported_pre[,  variant2 := ifelse(variant=="alpha" & "engl" %in% variant, "WT",
                                          ifelse(variant=="Alpha", "alpha",
                                                 ifelse(variant=="engl", "alpha",
                                                        ifelse(variant=="Delta", "delta",
                                                               ifelse(variant=="omicron", "BA1",
                                                                      ifelse(variant=="BA.1", "BA1",
                                                                             ifelse(variant=="BA.2", "BA2",
                                                                                    ifelse(variant=="BA5", "BA4+5",
                                                                                           ifelse(variant=="BQ1__BA2_75", "BQ1+BA2.75",
                                                                                                  ifelse(variant=="XBB_", "XBB*",
                                                                                                         ifelse(variant=="BA2_86", "BA2.86",
                                                                                                                as.character(variant))))))))))))]
  
  
  
  message("--------------------------------------------\nRecoding variant names in simu_reported_pre\n",paste(simu_reported_pre$variant %>% unique(), collapse = ", ") ,"\nto \n", paste(simu_reported_pre$variant2 %>% unique(), collapse = ", ") )
  
  recodervariant_simu_reported = simu_reported_pre[, .N, .(variant,variant2)]
  stopifnot(identical(recodervariant_simu_reported[,paste(variant, variant2)] %>% sort(),
                      recodervariant[,paste(variant, variant2)] %>% sort())) # check for same encoding
  
  factorlevels_variants 
  stopifnot(all(simu_reported_pre$variant2 %in% factorlevels_variants  ))
  simu_reported_pre[, variant2:= factor(variant2, levels = factorlevels_variants)]
  
  simu_reported_pre[, outcome := "incidence modelintern"] # 
  
  
  
  # add all agegroup
  
  simu_reported_toadd = simu_reported_pre[, .(cumvalue = sum(cumvalue, na.rm = T), 
                                              agegroup = "all",
                                              agegroup2 = "all"), .(num,datum, immunstate,immunstate2, variant, variant2, outcome) ]
  
  simu_reported = rbind(simu_reported_pre, simu_reported_toadd)[datum <= maxdate]
  setorder(simu_reported, agegroup2, immunstate, variant, variant2, datum,outcome)
  # simu_reported[immunstate ==  immunstate[1] & agegroup2 == agegroup2[3] &variant == variant[3]]
  simu_reported[, value :=  cumvalue-c(0, cumvalue[1:(.N-1)]), .(outcome, agegroup2, immunstate, variant, variant2, outcome)]
  
  
if(showplots==TRUE) print(ggplot(simu_reported[value>=1], aes(datum, value, col = immunstate2)) + geom_line() + facet_grid(agegroup2~variant2, scales = "free") + scale_y_log10(label= label_comma(accuracy = 1), sec.axis = dup_axis()) )
  
  
  plot_kumulInzByImmune = ggplot(simu_reported[value>0], aes(datum, value, col  = immunstate2))+ theme_minimal(base_size = 14) + theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0)) + geom_line(lwd = 1) + facet_grid(agegroup2 ~ variant2, scales = "free", space = "free")  + ylab(unique(simu_reported$outcome)) + ggtitle(scenarioname)
  if(showplots==TRUE) print(plot_kumulInzByImmune)
  
  if(is.null(scenarioname)==F ){
    simu_reported$scenario = scenarioname
    
  } else simu_reported$scenario = "scenario" 
  
  variantdata = moveColFront(simu_reported, 'scenario')[datum <= maxdate]
  
  ##################################################.
  # >extrct data for outputlayer-----
  # outputlayerdata
  # 
  tensor$datum = unique(simu_reported_pre$datum) %>% sort()
  
  if(extractOutputlayer) { 
    sm = tensor$res_daily
    tail(tensor$res_dailyoutputs$all$simulated_daily$Total)
    tail(tensor$outputs$`0_14`$simulated_daily$Total)
    tail(tensor$outputs$`35_59`$simulated_daily$Total)
    
    agestodo = names(tensor$outputs)
    agestodo
    
    if(is.null(agestodo)==F){
      outputs2 = lapply(agestodo, function(myage) {
        # myage= agestodo[3]
        output_myage  = tensor$outputs[[myage]]
        output_myage2 = data.table(time  = output_myage$time,
                                   datum = as_date(output_myage$time))
        
        
        for(my_daily_outcome in names(output_myage$simulated_daily)) {
          # my_daily_outcome =names(output_myage$simulated_daily)[1]
          output_myage2[[paste0('daily_', my_daily_outcome)]] = output_myage$simulated_daily[[my_daily_outcome]]
        }
        output_myage2
        
        for(my_cumul_outcome in names(output_myage$simulated_cumul)) {
          # my_cumul_outcome =names(output_myage$simulated_cumul)[1]
          output_myage2[[paste0('cumul_', my_cumul_outcome)]] = output_myage$simulated_cumul[[my_cumul_outcome]]
        }
        
        if(output_myage2[time ==max(time), daily_Death==0 & is.na(daily_Death)==F]) { # bug laut Yuri tel 7.9.22, hier fix dafuer
          output_myage2[time ==max(time), daily_Death :=NA]
          output_myage2[time ==max(time), cumul_Death :=NA]
        }
        
        output_myage2[, agegroup := myage]
        output_myage2
      }) %>% rbindlist()
    } else {
      stopifnot(identical(ncol(tensor$res_daily$Total), length(tensor$datum) ))
      ouputs1a = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Total",
                            (tensor$res_daily$Total %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'),  variable.name="agegroup")
      
      ouputs1b = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Death",
                            (tensor$res_daily$Death %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'),  variable.name="agegroup")
      
      
      ouputs1c = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Critical",
                            (tensor$res_daily$Critical %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'), variable.name="agegroup")
      
      
      ouputs1d = data.table(datum=tensor$datum %>% as_date(),
                            type = "daily_Normal",
                            (tensor$res_daily$Normal %>% t()) %>% 
                              as.data.table()) %>% 
        melt.data.table(id.vars = c("datum",'type'),  variable.name="agegroup")
      
      outputs2 = rbind(ouputs1a, 
                       ouputs1b, 
                       ouputs1c, 
                       ouputs1d)
      
      unique(outputs2$agegroup)
      
      outputs2[, agegroup2:= ifelse(agegroup =="0_14", "A00-A14",
                                    ifelse(agegroup =="15_34", "A15-A34",
                                           ifelse(agegroup =="35_59", "A35-A59",
                                                  ifelse(agegroup =="60_79", "A60-A79",
                                                         ifelse(agegroup =="80", "A80+",
                                                                ifelse(agegroup =="all", "all",agegroup))))))] 
      
      
      
      
      outputs2[,.N, .(agegroup, agegroup2)]
      # ggplot(outputs2, aes(datum, value, col = agegroup)) + geom_line() + facet_wrap(~type, scales = "free")
      
      
      outputs3 = dcast.data.table(outputs2, datum + agegroup+agegroup2~type, value.var = "value")
      setorder(outputs3, agegroup,datum)
      
      outputs3[, cumul_Total := cumsum(daily_Total), agegroup2]
      outputs3[, cumul_Critical := cumsum(daily_Critical), agegroup2]
      outputs3[, cumul_Death := cumsum(daily_Death), agegroup2]
      outputs3[, cumul_Normal := cumsum(daily_Normal), agegroup2]
      
      
      outputs3m = melt(outputs3, id.vars = c("datum", "agegroup2"), measure.vars = c("daily_Total","daily_Death", "daily_Critical", "daily_Normal","cumul_Total", "cumul_Death", "cumul_Critical", "cumul_Normal"))[datum <= maxdate]
      outputs3m[, datatype := str_split(variable, "_") %>% sapply(., "[", 1)]
      outputs3m[, outcome_yuri := str_split(variable, "_") %>% sapply(., "[", 2)]
      outputs3m$outcome_yuri %>% unique()
      outputs3m[, outcome :=ifelse(outcome_yuri =="Total", "reported",
                                   ifelse(outcome_yuri =="Critical", "ICU",
                                          ifelse(outcome_yuri =="Death", "died",
                                                 ifelse(outcome_yuri =="Normal", "Hospital",
                                                        outcome_yuri))))]
      
      
      outputs3m[,.N, .(datatype, outcome, outcome_yuri,variable)]
      
      
      plot_outputlayer = ggplot(outputs3m, aes(datum, value, col = outcome)) + geom_line() + facet_grid(datatype~agegroup2, scales = "free") + scale_y_log10()
      if(showplots==TRUE) print(plot_outputlayer)
      
      
      
      if(is.null(scenarioname)==F ){
        outputs3m$scenario = scenarioname
        outputs3m = moveColFront(outputs3m, 'scenario')
      }
      
    }
  }
  ##################################################.
  # # >extract info about variant frequencies currently in the model----
  variantcompartments = sim_arri_l[grepl("^D$|^R", compartment)==F, unique(compartment)]
  novariantcompartments = sim_arri_l[grepl("^D$|^R", compartment)==T, unique(compartment)]
  message("--------------------------------------------\nCalculating virus variant frequencies over time from compartments\n", paste(variantcompartments, collapse = ", "), "\n (and not from compartments ", paste(novariantcompartments, collapse = ", "), ")")
  novariantcompartments = sim_arri_l[grepl("^D$|^R", compartment)==T, unique(compartment)]
  
  dataVariant = sim_arri_l[grepl("^D$|^R", compartment)==F, .(value = sum(value)), .(datum, variant2)]
  p1 = ggplot(dataVariant, aes(datum, value, col = variant2)) + geom_line()
  
  dataVariant[, dataVariant_anteil :=  value / sum(value), datum]
  
  p2 = ggplot(dataVariant, aes(datum, dataVariant_anteil, col = variant2)) + geom_line()
  require(patchwork)
  variantplot = (p1+p2+ plot_layout(guides = "collect") + plot_annotation(title = scenarioname))
  if(showplots==TRUE) print(variantplot)
  
  if(is.null(scenarioname)==F ){
    dataVariant$scenario = scenarioname
    variantdata = moveColFront(dataVariant, 'scenario')
  }
  
  
  ### extract variants as measured in the outputlayer
  ##considering only tested individuals
  message("Now calculating virus variant frequencies in the output layer")
  mutant_outputlayer = tensor$res_daily$allele_frac %>% as.data.table(keep.rownames = T)
  
  mutant_outputlayerm = melt(mutant_outputlayer, id.vars = c("rn", "dates", "numdates"))
  ggplot(mutant_outputlayerm, aes(as_date(dates) ,value,  col = variable     )) + geom_line(alpha=0.5)
  
  
  mutant_outputlayerm[, variant := str_replace_all(variable, "_simulated|_measured", "")]
  if(any(grepl("measured$", mutant_outputlayerm$variable))) mutant_outputlayerm[, type := str_split(variable, "_") %>% sapply(., "[", 2)] else mutant_outputlayerm[, type := ifelse(grepl("_simulated$", variable),"simulated", "measured")]
  mutant_outputlayerm[,.N,.(variable, variant, type)] %>% data.frame()
  
  
  stopifnot(all(mutant_outputlayerm$type %in% c("measured", "simulated")))
  
  stopifnot(all (c("alpha", "engl", "delta","omicron","BA2","BA5") %in% mutant_outputlayerm$variant))
  
  
  mutant_outputlayerm[, variant2 :=  ifelse(variant=="alpha" & "engl" %in% variant, "WT",
                                                        ifelse(variant=="Alpha", "alpha",
                                                               ifelse(variant=="engl", "alpha",
                                                                      ifelse(variant=="Delta", "delta",
                                                                             ifelse(variant=="omicron", "BA1",
                                                                                    ifelse(variant=="BA.1", "BA1",
                                                                                           ifelse(variant=="BA.2", "BA2",
                                                                                                  ifelse(variant=="BA5", "BA4+5",
                                                                                                         ifelse(variant=="BQ1__BA2_75", "BQ1+BA2.75",
                                                                                                                ifelse(variant=="BQ1", "BQ1+BA2.75",
                                                                                                                ifelse(variant=="XBB_", "XBB*",
                                                                                                                       ifelse(variant=="XBB", "XBB*",
                                                                                                                       ifelse(variant=="BA2_86", "BA2.86",
                                                                                                                              as.character(variant))))))))))))))]
  
  message("--------------------------------------------\nRecoding variant names in mutant_outputlayerm\n",paste(mutant_outputlayerm$variant %>% unique(), collapse = ", ") ,"\nto \n", paste(mutant_outputlayerm$variant2 %>% unique(), collapse = ", "))
  
  
  mutant_outputlayerm[, .N,.(variant, variant2)]
  
  factorlevels_variants 
  stopifnot(all(mutant_outputlayerm$variant2 %in% factorlevels_variants  ))
  mutant_outputlayerm[, variant2:= factor(variant2, levels = factorlevels_variants)]
  
  
  #renaming
  mutant_outputlayerm2 = mutant_outputlayerm[, .(datum  = dates %>% as_date(), variant, variant2, dataVariant_anteil=value, type )][datum <= maxdate]
  
  if(is.null(scenarioname)==F ){
    mutant_outputlayerm2$scenario = scenarioname
    mutant_outputlayerm2 = moveColFront(mutant_outputlayerm2, 'scenario')
  }
  
  
  mutant_outputlayerm2[, type := ifelse(type=="measured", "reported",
                                        ifelse(type =="simulated", "modelled", as.character(type)))]
  
  # mutant_outputlayerm2[,.N,.(type, type2)]
  
  p_mutant_outputlayerm = ggplot(mutant_outputlayerm2, aes(datum,dataVariant_anteil, lty = type, col = variant , group = paste(type, variant))) + geom_line(alpha=0.5)
  
  p_mutant_outputlayerm
  # plotly::ggplotly(p_mutant_outputlayerm)
  
  # ## compare them
  # variantdata
  # mutant_outputlayerm$type
  # comparetab = rbind(variantdata[, .(dates = datum %>% as_date(), variant2, value=dataVariant_anteil, type ="oldmodel")],
  #                    mutant_outputlayerm[, .(dates = dates %>% as_date(), variant2, value, type )])
  # 
  # comparetab[,.N, .(type)]
  # p_comparetabfreq = ggplot(comparetab, aes(dates,value, lty = type, col = variant2 , group = paste(type, variant2))) + geom_line(size = 1)
  # p_comparetabfreq
  
  ##################################################.
  # # >extract info about infectivity ----
  # 
  
  infectivitydata = tensor$treat_arr %>% as.data.table(keep.rownames = T)
  infectivitydata[, time := as.numeric(rn)]                  
  infectivitydata=infectivitydata[order(time)]
  infectivitydata[, datum := firstdate-1 + time]
  
  infectivitydatam = melt(infectivitydata, id.vars = c("rn","datum", "time"))[datum <= maxdate]
  
  infectivitydatam[, agegroup2:= ifelse(variable =="age_1", "A00-A14",
                                        ifelse(variable =="age_2", "A15-A34",
                                               ifelse(variable =="age_3", "A35-A59",
                                                      ifelse(variable =="age_4", "A60-A79",
                                                             ifelse(variable =="age_5", "A80+",variable)))))] 
  
  
  
  infectivityplot =ggplot(infectivitydatam, aes(datum, value, col = agegroup2 )) + 
    geom_line(lwd=1) + 
    theme_minimal(base_size = 14) +
    scale_y_continuous(breaks = pretty_breaks(10), labels = label_comma(accuracy = 0.1), sec.axis = dup_axis()) +
    ylab("Infectivity b1") +
    scale_x_date( breaks = date_breaks(width = "1 months"))+ #limits = range(vaccdata$DateRep), 
    theme(axis.title.x = element_blank(),
          legend.position = c(0.21, 0.7),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0)) + 
    labs(lty = "")+
    scale_color_viridis_d(direction = -1) +
    # geom_vline( xintercept = datesDominance$date, lty = 2, alpha = 0.5) +
    
    # guides(col = "none") +
    # geom_vline( xintercept = as_date(c("2020-03-22", "2020-12-14", "2021-11-22")), lty = 2, alpha = 0.4, col = "red", lwd = 1) +
    coord_cartesian(ylim = c(0, 1))
  infectivityplot
  
  
  # >zusammenbauen ----
  res = c()
  res$scenario = ifelse(is.null(scenarioname), "", scenarioname)
  res$incidencedata = simu_reported
  res$incidenceplot = plot_kumulInzByImmune
  res$compartmentdata = compartmentdata
  res$variantplot = variantplot
  res$variantdata = variantdata
  
  res$variantplot_outputlayer = p_mutant_outputlayerm
  res$variantdata_outputlayer = mutant_outputlayerm2
  
  if(extractOutputlayer==T) res$outputlayerdata = outputs3m
  if(extractOutputlayer==T) res$outputlayerplot = plot_outputlayer
  res$infectivitydata = infectivitydatam
  res$infectivityplot = infectivityplot
  
  res
}
