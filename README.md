# Masoko_lateral_line_2023
Code and data for Edgley et al. Lateral line diversification during the early stages of ecological speciation in cichlid fish

ASSETS:

pore_areas_masoko.csv - dataframe with pore areas for all cranial canal pores for Masoko specimens
pore_areas_masoko_malawi.csv - dataframe with pore areas for cranial canal pores for both Masoko and Malawi specimens (latter from Scott et al. 2023)
neuromasts_fieldwork.csv - dataframe with neuromast count data for Masoko and Malawi specimens
Masoko_VH_scaled_converted.TPS - tps file containing coordinate data for pore landmarks (ventral perspective, mandibular & preopercular canals)
Masoko_IOC_L_scaled_converted.TPS - tps file containing coordinate data for pore landmarks (ventral perspective, infraorbital canal left side)
Masoko_IOC_R_scaled_converted.TPS - tps file containing coordinate data for pore landmarks (ventral perspective, infraorbital canal right side)
Masoko_LH_scaled_converted.TPS - tps file containing coordinate data for pore landmarks (lateral perspective, preopercular & otic canals)
classifiers.csv - dataframe with supporting information to merge with pore areas after calculation


SCRIPTS:

Masoko_pore_area_calculation.R - R code for calculating lateral line canal pore areas from landmarked TPS files (Masoko samples)
Masoko_statistical_models.R  - R code for statistical models comparing lateral line morphologies between populations
Masoko_malawi_paper_plots.R - R code for producing figures - conducted in same workspace as "Masoko_statistical_models.R" (above)
