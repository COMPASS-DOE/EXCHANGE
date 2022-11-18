## EXCHANGE-FTICR
##
## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports and processes FTICR data obtained from Formularity 
## 
## Created: November 2022
## Kaizad F. Patel

## These functions have been modified from the `fticrrr` package and workflow: 
## https://github.com/kaizadp/fticrrr

################################################## #

# `fticrrr-functions-initial-processing.R`

################################################## #

## this script will load functions for:
## (a) processing FTICR reports generated in Formularity
## -- (a1) filtering peaks 
## -- (a2) computing indices, element composition, class assignment for metadata 
## -- (a3) blank-correcting the data


## NOTE: These data contain intensities, which are not recommended when analyzing the data
## We recommend converting these intensities to presence/absence data (0/1) before further analysis.
## Refer to the functions in `b-fticrrr-functions-advanced-processing.R` for these functions.
## See these papers for more information on processing and analyzing FTICR data:
#### 1.
#### 2.
#### 3.

# INSTRUCTIONS:
## source this file in the `water_fticr.R` file, do not run the script here.
## This script can (generally) be used as is for most data that follow this format. No modifications needed 

################################################## #
################################################## #

# 1. PROCESSING FUNCTIONS -------------------------------------------------
## LEVEL I FUNCTIONS -------------------------------------------------------
## for metadata file
apply_filter_report = function(report){
  report %>% 
    # filter appropriate mass range
    filter(Mass>200 & Mass<800) %>% 
    # remove isotopes
    filter(C13==0) %>% 
    # remove peaks without C assignment
    filter(C>0)
}
compute_indices = function(dat){
  dat %>% 
    dplyr::select(Mass, C:P) %>% 
    dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                  NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                  HC = round(H/C,2),
                  OC = round(O/C,2),
                  DBE_AI = 1+C-O-S-0.5*(N+P+H),
                  DBE =  1 + ((2*C-H + N + P))/2,
                  DBE_C = round(DBE_AI/C,4)) %>% 
    dplyr::select(-c(C:P))
}
compute_mol_formula = function(dat){
  dat %>% 
    dplyr::select(Mass, C:P) %>% 
    dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                  formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                  formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                  formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                  formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                  formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                  formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                  formula = str_replace_all(formula,"NA","")) %>% 
    dplyr::select(Mass, formula)
}
assign_class_seidel = function(meta_clean, meta_indices){
  meta_clean %>%
    left_join(meta_indices, by = "Mass") %>% 
    mutate(Class = case_when(AImod>0.66 ~ "condensed aromatic",
                             AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                             AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                             HC >= 1.5 ~ "aliphatic"),
           Class = replace_na(Class, "other"),
           Class_detailed = case_when(AImod>0.66 ~ "condensed aromatic",
                                      AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                                      AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                                      HC >= 2.0 & OC >= 0.9 ~ "carbohydrate",
                                      HC >= 2.0 & OC < 0.9 ~ "lipid",
                                      HC < 2.0 & HC >= 1.5 & N==0 ~ "aliphatic",
                                      HC < 2.0 & HC >= 1.5 & N > 0 ~ "aliphatic+N")) %>% 
    dplyr::select(Mass, Class, Class_detailed)
}

## LEVEL II FUNCTIONS ------------------------------------------------------

make_fticr_meta = function(report){
  # filter peaks
  fticr_report = (apply_filter_report(report))
  
  meta_clean = 
    fticr_report %>% 
    # select only the relevant columns for the formula assignments
    dplyr::select(Mass, C, H, O, N, S, P, El_comp)
  
  # calculate indices and assign classes - Seidel 2014 and Seidel 2017
  meta_indices = compute_indices(meta_clean)
  meta_formula = compute_mol_formula(meta_clean)
  meta_class = assign_class_seidel(meta_clean, meta_indices)
  
  # output
  meta2 = meta_formula %>% 
    left_join(meta_class, by = "Mass") %>% 
    left_join(meta_indices, by = "Mass") %>% dplyr::select(-Mass) %>% distinct(.)
  
  list(meta2 = meta2,
       meta_formula = meta_formula)
}
make_fticr_data_intensities = function(report, sample_key){
  fticr_report = (apply_filter_report(report))
  mass_to_formula = make_fticr_meta(report)$meta_formula
  
  data_columns = fticr_report %>% dplyr::select(Mass, starts_with(c("Blank", "K")))
  
  data_samples = 
    fticr_report %>% 
    dplyr::select(Mass, starts_with("K"))
  
  data_blanks  = 
    data_columns %>% 
    dplyr::select(Mass, ends_with("Eluate")) %>% 
    mutate(summary = rowSums(select(., starts_with("Blank"))),
           BLANK = summary > 0) %>% 
    dplyr::select(Mass, BLANK) %>% 
    filter(BLANK)
  
  data_samples_blank_corrected = 
    data_samples %>% 
    left_join(data_blanks) %>% 
    filter(is.na(BLANK))
  
  list(data_samples_blank_corrected = data_samples_blank_corrected,
       data_blanks = data_blanks)
}
