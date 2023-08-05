#### PACKAGES ####
library(ggplot2)
library(ggsignif)
library(multcomp)
library(cowplot)
library(jtools)
library(emmeans)
library(sjPlot)
library(effects)
library(cowplot)

#### READING IN DATA & SUMMARY STATS ####

#CT data
pore_areas_merged_corrected <- read.csv("pore_areas_malawi.csv",
                                        fileEncoding = "UTF-8-BOM")

standardised_mean_mandibular_pore_area <- rstandard(lm(log10(mean_mandibular_pore_area) ~ log10(SL), data=pore_areas_merged_corrected, na.action=na.exclude))
standardised_mean_preopercular_pore_area <- rstandard(lm(log10(mean_preopercular_pore_area) ~ log10(SL), data=pore_areas_merged_corrected, na.action=na.exclude))
standardised_mean_orbital_pore_area <- rstandard(lm(log10(mean_orbital_pore_area) ~ log10(SL), data=pore_areas_merged_corrected, na.action=na.exclude))
standardised_mean_otic_pore_area <- rstandard(lm(log10(mean_otic_pore_area) ~ log10(SL), data=pore_areas_merged_corrected, na.action=na.exclude))

#NM data
neuromast_data <- read.csv("neuromasts_fieldwork.csv",
                           fileEncoding = "UTF-8-BOM")
neuromast_data$ECOMORPH <- as.factor(neuromast_data$ECOMORPH)

# counting number of samples

nrow(pore_areas_merged_corrected[which(pore_areas_merged_corrected$Depth_Cat=="Shallow"),])
nrow(pore_areas_merged_corrected[which(pore_areas_merged_corrected$Depth_Cat=="Deep"),])
nrow(pore_areas_merged_corrected[which(pore_areas_merged_corrected$Depth_Cat=="Intermediate"),])

nrow(pore_areas_merged_corrected[which(pore_areas_merged_corrected$subpop=="benthic"),])
nrow(pore_areas_merged_corrected[which(pore_areas_merged_corrected$subpop=="middle"),])
nrow(pore_areas_merged_corrected[which(pore_areas_merged_corrected$subpop=="littoral"),])

# plotting histograms of data
hist(pore_areas_merged_corrected$mean_mandibular_pore_area)
hist(pore_areas_merged_corrected$mean_preopercular_pore_area)
hist(pore_areas_merged_corrected$mean_orbital_pore_area)
hist(pore_areas_merged_corrected$mean_otic_pore_area)

hist(neuromast_data$TOTAL_HEAD_SN)
hist(neuromast_data$TOTAL_TRUNK_SN)
hist(neuromast_data$TOTAL_TRUNK_CN)
hist(neuromast_data$AVERAGE_TRUNK_SNS_PER_CN)


#### PORE MORPHOLOGY PCA ####

data_ventral <- read.csv("Masoko_VH_combined.TPS")
procrustes_ventral <- gpagen(data_ventral)
pca_ventral <- gm.prcomp(procrustes_ventral$coords)
classifiers <- read.csv("classifiers.csv")
plot(pca_ventral, col=as.factor(classifiers$subpop))

ventral_plot_data <- cbind(pca_ventral$x, classifiers)

data_lateral <- readland.tps("Masoko_LH_combined.TPS")
procrustes_lateral <- gpagen(data_lateral)
pca_lateral <- gm.prcomp(procrustes_lateral$coords)
plot(pca_lateral, col=as.factor(classifiers$subpop))

lateral_plot_data <- cbind(pca_lateral$x, classifiers)


#### PORE SIZE ANALYSIS ####

### grouped by subpopulation
pore_areas_merged_corrected$subpop <- as.factor(pore_areas_merged_corrected$subpop)
pore_areas_merged_corrected$Sex_Value <- as.factor(pore_areas_merged_corrected$Sex_Value)

##mandibular
# glm by subpop, including standard length as a covariate
mandibular_model <- glm(log10(mean_mandibular_pore_area) ~ subpop + log10(SL) + Sex_Value, data=pore_areas_merged_corrected,
                        family="gaussian")
summary(mandibular_model)

#checking model validity
hist(resid(mandibular_model))
par(mfrow=c(2,2))
plot(mandibular_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_mandibular <- glht(mandibular_model, mcp(subpop="Tukey"))
summary(post_hoc_mandibular, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_mandibular <- data.frame(partialize(mandibular_model, vars="subpop"))

## preopercular
# glm by subpop, including standard length as a covariate
preopercular_model <- glm(log10(mean_preopercular_pore_area) ~ subpop + log10(SL) + Sex_Value, data=pore_areas_merged_corrected, 
                          family="gaussian")
summary(preopercular_model)

#checking model validity
hist(resid(preopercular_model))
par(mfrow=c(2,2))
plot(preopercular_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_preopercular <- glht(preopercular_model, mcp(subpop="Tukey"))
summary(post_hoc_preopercular, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_preopercular <- data.frame(partialize(preopercular_model, vars="subpop"))

## orbital
# glm by subpop, including standard length as a covariate
orbital_model_excluded_data <- pore_areas_merged_corrected[-c(194,195,199),]
orbital_model <- glm(log10(mean_orbital_pore_area) ~ subpop + log10(SL) + Sex_Value, data=orbital_model_excluded_data,
                     family="gaussian")
summary(orbital_model)

#checking model validity
hist(resid(orbital_model))
par(mfrow=c(2,2))
plot(orbital_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_orbital <- glht(orbital_model, mcp(subpop="Tukey"))
summary(post_hoc_orbital, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_orbital <- data.frame(partialize(orbital_model, vars="subpop"))

## otic
# glm by subpop, including standard length as a covariate
otic_model <- glm(log10(mean_otic_pore_area) ~ subpop + log10(SL) + Sex_Value, data=pore_areas_merged_corrected,
                  family="gaussian")
summary(otic_model)

#checking model validity
hist(resid(otic_model))
par(mfrow=c(2,2))
plot(otic_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_otic <- glht(otic_model, mcp(subpop="Tukey"))
summary(post_hoc_otic, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_otic <- data.frame(partialize(otic_model, vars="subpop"))

### grouped by depth
## mandibular
# glm by depth cat, including standard length as a covariate
pore_areas_merged_corrected$Depth_Cat <- as.factor(pore_areas_merged_corrected$Depth_Cat)

mandibular_model_depth <- glm(log10(mean_mandibular_pore_area) ~ Depth_Cat + log10(SL) + Sex_Value, data=pore_areas_merged_corrected,
                              family="gaussian")
summary(mandibular_model_depth)

#checking model validity
hist(resid(mandibular_model_depth))
par(mfrow=c(2,2))
plot(mandibular_model_depth)
par(mfrow=c(1,1))

#post hoc
post_hoc_mandibular_depth <- glht(mandibular_model_depth, mcp(Depth_Cat="Tukey"))
summary(post_hoc_mandibular_depth, test=adjusted("bonferroni"))

# partial residuals from model 

## preopercular
# glm by depth cat, including standard length as a covariate
preopercular_model_depth <- glm(log10(mean_preopercular_pore_area) ~ Depth_Cat + log10(SL) + Sex_Value, data=pore_areas_merged_corrected, 
                                family="gaussian")
summary(preopercular_model_depth)

#checking model validity
hist(resid(preopercular_model_depth))
par(mfrow=c(2,2))
plot(preopercular_model_depth)
par(mfrow=c(1,1))

#post hoc
post_hoc_preopercular_depth <- glht(preopercular_model_depth, mcp(Depth_Cat="Tukey"))
summary(post_hoc_preopercular_depth, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_preopercular_depth <- data.frame(partialize(preopercular_model_depth, vars="Depth_Cat"))
partial_residuals_preopercular_depth <- cbind(partial_residuals_preopercular_depth,
                                              pore_areas_merged_corrected$subpop)

## orbital
# anova by depth cat, including standard length as a covariate
orbital_model_depth <- glm(log10(mean_orbital_pore_area) ~ Depth_Cat + log10(SL) + Sex_Value, data=pore_areas_merged_corrected,
                           family="gaussian")
summary(orbital_model_depth)

#checking model validity
hist(resid(orbital_model_depth))
par(mfrow=c(2,2))
plot(orbital_model_depth)
par(mfrow=c(1,1))

#post hoc
post_hoc_orbital_depth <- glht(orbital_model_depth, mcp(Depth_Cat="Tukey"))
summary(post_hoc_orbital_depth, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_orbital_depth <- data.frame(partialize(orbital_model_depth, vars="Depth_Cat"))
partial_residuals_orbital_depth <- cbind(partial_residuals_orbital_depth,
                                         pore_areas_merged_corrected$subpop)

## otic
# anova by depth cat, including standard length as a covariate

otic_model_depth <- glm(log10(mean_otic_pore_area) ~ Depth_Cat + log10(SL) + Sex_Value, data=pore_areas_merged_corrected,
                        family="gaussian")
summary(otic_model_depth)

#checking model validity
hist(resid(otic_model_depth))
par(mfrow=c(2,2))
plot(otic_model_depth)
par(mfrow=c(1,1))

#post hoc
post_hoc_otic_depth <- glht(otic_model_depth, mcp(Depth_Cat="Tukey"))
summary(post_hoc_otic_depth, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_otic_depth <- data.frame(partialize(otic_model_depth, vars="Depth_Cat"))
partial_residuals_otic_depth <- cbind(partial_residuals_otic_depth,
                                      pore_areas_merged_corrected$subpop)




#### NEUROMAST COUNT ANALYSIS ####
neuromast_data_masoko <- neuromast_data[which(neuromast_data$sampling_location=="masoko"),]
neuromast_data_masoko$sex <- as.factor(neuromast_data_masoko$sex)

## head SN model
head_sn_model <- glm(TOTAL_HEAD_SN ~ ECOMORPH + log10(STANDARD_LENGTH_MM) + sex, data=neuromast_data_masoko, 
                     na.action="na.omit",
                     family="poisson")
summary(head_sn_model)

#checking model validity
hist(resid(head_sn_model))
par(mfrow=c(2,2))
plot(head_sn_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_head_sn_model <- glht(head_sn_model, mcp(ECOMORPH="Tukey"))
summary(post_hoc_head_sn_model, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_head_sns <- data.frame(partialize(head_sn_model, vars="ECOMORPH"))

## trunk SN model
trunk_sn_model <- glm(TOTAL_TRUNK_SN ~ ECOMORPH + log10(STANDARD_LENGTH_MM) + sex, data=neuromast_data_masoko, 
                      na.action="na.omit",
                      family="poisson")
summary(trunk_sn_model)

#checking model validity
hist(resid(trunk_sn_model))
par(mfrow=c(2,2))
plot(trunk_sn_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_trunk_sn_model <- glht(trunk_sn_model, mcp(ECOMORPH="Tukey"))
summary(post_hoc_trunk_sn_model, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_trunk_sns <- data.frame(partialize(trunk_sn_model, vars="ECOMORPH"))

## trunk CN model
trunk_cn_model <- glm(TOTAL_TRUNK_CN ~ ECOMORPH + log10(STANDARD_LENGTH_MM) + sex, data=neuromast_data_masoko, 
                      na.action="na.omit",
                      family="poisson")
summary(trunk_cn_model)

#checking model validity
hist(resid(trunk_cn_model))
par(mfrow=c(2,2))
plot(trunk_cn_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_trunk_cn_model <- glht(trunk_cn_model, mcp(ECOMORPH="Tukey"))
summary(post_hoc_trunk_cn_model, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_trunk_cns <- data.frame(partialize(trunk_cn_model, vars="ECOMORPH"))

## sns per cn model
sns_per_cn_model <- glm(AVERAGE_TRUNK_SNS_PER_CN ~ ECOMORPH + log10(STANDARD_LENGTH_MM) + sex, data=neuromast_data_masoko, 
                        na.action="na.omit",
                        family="poisson")
summary(sns_per_cn_model)

#checking model validity
hist(resid(sns_per_cn_model))
par(mfrow=c(2,2))
plot(sns_per_cn_model)
par(mfrow=c(1,1))

#post hoc
post_hoc_sns_per_cn_model <- glht(sns_per_cn_model, mcp(ECOMORPH="Tukey"))
summary(post_hoc_sns_per_cn_model, test=adjusted("bonferroni"))

# partial residuals from model 
partial_residuals_sns_per_cn <- data.frame(partialize(sns_per_cn_model, vars="ECOMORPH"))

#### MALAWI ANALYSIS ####

# reading data and prep
data_malawi <- read.csv("pore_areas_merged_corrected_with_malawi.csv")
data_malawi$standardised_mean_mandibular_pore_area <- rstandard(lm(log10(malawi_mandibular_mean_pore_area) ~ log10(SL), data=data_malawi, na.action=na.exclude))
data_malawi$standardised_mean_preopercular_pore_area <- rstandard(lm(log10(malawi_preopercular_mean_pore_area) ~ log10(SL), data=data_malawi, na.action=na.exclude))

# get partialized residual values from models - pore areas
model_mandibular_pores_malawi <- glm(log10(malawi_mandibular_mean_pore_area)~ subpop_for_graph + log10(SL), data=data_malawi, family="gaussian")
partial_residuals_mandibular_malawi <- data.frame(partialize(model_mandibular_pores_malawi, vars="subpop_for_graph"))
summary(aov(model_mandibular_pores_malawi))

model_preopercular_pores_malawi <- glm(log10(malawi_preopercular_mean_pore_area)~ subpop_for_graph + log10(SL), data=data_malawi, family="gaussian")
partial_residuals_preopercular_malawi <- data.frame(partialize(model_preopercular_pores_malawi, vars="subpop_for_graph"))
summary(aov(model_preopercular_pores_malawi))

model_orbital_pores_malawi <- glm(log10(malawi_orbital_mean_pore_area)~ subpop_for_graph + log10(SL), data=data_malawi, family="gaussian")
partial_residuals_orbital_malawi <- data.frame(partialize(model_orbital_pores_malawi, vars="subpop_for_graph"))
summary(aov(model_orbital_pores_malawi))

model_otic_pores_malawi <- glm(log10(malawi_otic_mean_pore_area)~ subpop_for_graph + log10(SL), data=data_malawi, family="gaussian")
partial_residuals_otic_malawi <- data.frame(partialize(model_otic_pores_malawi, vars="subpop_for_graph"))
summary(aov(model_otic_pores_malawi))

# get partialized residual values from models - neuromasts
neuromast_data_nona <- subset(neuromast_data, !is.na(TOTAL_TRUNK_CN))
model_neuromasts_malawi <- glm(log10(TOTAL_TRUNK_CN)~ subpop_for_graph + log10(SL), data=neuromast_data_nona, family="poisson")
partial_residuals_neuromasts_trunk_CN_malawi <- data.frame(partialize(model_neuromasts_malawi, vars="subpop_for_graph"))
summary(aov(model_neuromasts_malawi))


neuromast_data_nona <- subset(neuromast_data, !is.na(TOTAL_TRUNK_SN))
model_neuromasts_malawi <- glm(log10(TOTAL_TRUNK_SN)~ subpop_for_graph + log10(SL), data=neuromast_data_nona, family="poisson")
partial_residuals_neuromasts_trunk_SN_malawi <- data.frame(partialize(model_neuromasts_malawi, vars="subpop_for_graph"))
summary(aov(model_neuromasts_malawi))

neuromast_data_nona <- subset(neuromast_data, !is.na(TOTAL_HEAD_SN))
model_neuromasts_malawi <- glm(log10(TOTAL_HEAD_SN)~ subpop_for_graph + log10(SL), data=neuromast_data_nona, family="poisson")
partial_residuals_neuromasts_head_SN_malawi <- data.frame(partialize(model_neuromasts_malawi, vars="subpop_for_graph"))
summary(aov(model_neuromasts_malawi))

neuromast_data_nona <- subset(neuromast_data, !is.na(AVERAGE_TRUNK_SNS_PER_CN))
model_neuromasts_malawi <- glm(AVERAGE_TRUNK_SNS_PER_CN~ subpop_for_graph + log10(SL), data=neuromast_data_nona, family="poisson")
partial_residuals_neuromasts_trunk_sns_per_cn_malawi <- data.frame(partialize(model_neuromasts_malawi, vars="subpop_for_graph"))
summary(aov(model_neuromasts_malawi))
