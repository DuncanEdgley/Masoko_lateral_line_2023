## IMPORTANT NOTE: This script uses data and objects produced in the script "Masoko_statistical_models.R".
## Ensure that that code has been run correctly. The plots below rely on being in the same working environment.

#### PACKAGES ####
library(ggplot2)
library(cowplot)
library(sjPlot)
library(tidyverse)
library(plyr)
source("R_rainclouds.R")

#### Figure 1 ####
## Gross lateral head morphology PCA

lateral_gross_PCA_plot <- ggplot(data=pore_areas_merged_corrected, aes(x=gross_lateral_PC1, y=gross_lateral_PC2, color=subpop)) + 
  geom_point() + 
  stat_ellipse() +
  theme_classic() + 
  scale_color_manual(values=c("#4477AA", "#DDAA33", "gray")) + 
  xlab("PC1 (36.22%)") + 
  ylab("PC2 (17.64%)") + 
  theme(legend.position="none")
lateral_gross_PCA_plot

## Gross ventral head morphology PCA
ventral_gross_PCA_plot <- ggplot(data=pore_areas_merged_corrected, aes(x=gross_ventral_PC1, y=gross_ventral_PC2, color=subpop)) + 
  geom_point() + 
  stat_ellipse() +
  theme_classic() + 
  scale_color_manual(values=c("#4477AA", "#DDAA33", "gray")) + 
  xlab("PC1 (53.69%)") + 
  ylab("PC2 (11.29%)") + 
  theme(legend.position="none")
ventral_gross_PCA_plot

## Warped mesh shape changes
# lateral head
msh <- mshape(procrustes_gross_lateral$coords)
plotRefToTarget(pca_gross_lateral$shapes$shapes.comp1$max, msh)
plotRefToTarget(pca_gross_lateral$shapes$shapes.comp1$min, msh)
plotRefToTarget(pca_gross_lateral$shapes$shapes.comp2$max, msh)
plotRefToTarget(pca_gross_lateral$shapes$shapes.comp2$min, msh)

# ventral head
msh <- mshape(procrustes_gross_ventral$coords)
plotRefToTarget(pca_gross_ventral$shapes$shapes.comp1$max, msh)
plotRefToTarget(pca_gross_ventral$shapes$shapes.comp1$min, msh)
plotRefToTarget(pca_gross_ventral$shapes$shapes.comp2$max, msh)
plotRefToTarget(pca_gross_ventral$shapes$shapes.comp2$min, msh)

## associations between morphology and pore area

ventral_size_association <- ggplot(data=pore_areas_merged_corrected, aes(y=log10(mean_mandibular_pore_area),
                                                                         x=gross_ventral_PC1)) + 
  geom_point(aes(color=pore_areas_merged_corrected$subpop)) + 
  geom_smooth(method="lm", color="black") + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA", "#DDAA33", "gray")) + 
  xlab("Gross ventral morphology PC1 (53.69%)") + 
  ylab(expression(paste("Mean mandibular pore area"~ (mm^2)))) +
  theme(legend.position="none")

ventral_size_association

lateral_size_association <- ggplot(data=pore_areas_merged_corrected, aes(y=log10(mean_orbital_pore_area),
                                                                         x=gross_lateral_PC1)) + 
  geom_point(aes(color=pore_areas_merged_corrected$subpop)) + 
  geom_smooth(method="lm", color="black") + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA", "#DDAA33", "gray")) + 
  xlab("Gross lateral morphology PC1 (36.22%)") + 
  ylab(expression(paste("Mean orbital pore area"~ (mm^2)))) + 
  theme(legend.position="none")

lateral_size_association

#### Figure 2 ####
n/a

#### Figure 3 ####

##geometric morphometrics PCAs
#ventral
ventral_pores <- ggplot(data=subset(ventral_plot_data, !is.na(subpop)), 
                        aes(x=Comp1, y=Comp2, colour=subpop)) + 
  geom_point() + 
  stat_ellipse() + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA", "#DDAA33", "gray")) + 
  xlab("PC1 (60.55%)") + 
  ylab("PC2 (12.01%)") + 
  theme(legend.position="none")

ventral_pores 

msh <- mshape(procrustes_ventral$coords)
plotRefToTarget(pca_ventral$shapes$shapes.comp1$min, msh)
plotRefToTarget(pca_ventral$shapes$shapes.comp1$max, msh)

plotRefToTarget(pca_ventral$shapes$shapes.comp2$min, msh)
plotRefToTarget(pca_ventral$shapes$shapes.comp2$max, msh)

#lateral
lateral_pores <- ggplot(data=subset(lateral_plot_data, !is.na(subpop)), 
                        aes(x=Comp1, y=Comp2, colour=subpop)) + 
  geom_point() + 
  stat_ellipse() + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA", "#DDAA33", "gray")) + 
  ylim(-0.06, 0.06) + 
  xlab("PC1 (37.10%)") + 
  ylab("PC2 (27.33%)") + 
  theme(legend.position="none")

lateral_pores

msh <- mshape(procrustes_lateral$coords)
plotRefToTarget(pca_lateral$shapes$shapes.comp1$min, msh)
plotRefToTarget(pca_lateral$shapes$shapes.comp1$max, msh)

plotRefToTarget(pca_lateral$shapes$shapes.comp2$min, msh)
plotRefToTarget(pca_lateral$shapes$shapes.comp2$max, msh)

##mandibular canal
#subpop
subpop_plot_mandibular <- ggplot(data=partial_residuals_mandibular, 
                                 aes(x=factor(subpop, levels=c("benthic", "middle", "littoral")), 
                                     y=log10.mean_mandibular_pore_area., 
                                     fill=subpop))+ 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_mandibular, 
             aes(x=subpop, y=log10.mean_mandibular_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop, y=log10.mean_mandibular_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black")  + 
  theme_classic() + 
  scale_fill_manual(values=c("#4477AA", "#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Benthic", "Intermediate", "Littoral")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean mandibular pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("littoral", "benthic")), y_position=-0.4, annotations="**") +
  geom_signif(comparison=list(c("middle", "benthic")), y_position=-0.5, annotations="NS") +
  geom_signif(comparison=list(c("littoral", "middle")), y_position=-0.6, annotations="NS") 
subpop_plot_mandibular

#depth cat
depth_plot_mandibular <- ggplot(data=partial_residuals_mandibular_depth, 
                                aes(x=factor(Depth_Cat), 
                                    y=log10.mean_mandibular_pore_area.)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8, fill="gray") + 
  geom_point(data=partial_residuals_mandibular_depth, 
             aes(x=Depth_Cat, y=log10.mean_mandibular_pore_area., color=pore_areas_merged_corrected$subpop), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=Depth_Cat, y=log10.mean_mandibular_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black")  + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA","#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Deep", "Midwater", "Shallow")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean mandibular pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("Shallow", "Deep")), y_position=-0.4, annotations="NS") +
  geom_signif(comparison=list(c("Intermediate", "Deep")), y_position=-0.5, annotations="**") +
  geom_signif(comparison=list(c("Shallow", "Intermediate")), y_position=-0.6, annotations="NS")
depth_plot_mandibular

##preopercular canal
#subpop
subpop_plot_preopercular <- ggplot(data=partial_residuals_preopercular, 
                                   aes(x=factor(subpop, levels=c("benthic", "middle", "littoral")), 
                                       y=log10.mean_preopercular_pore_area., 
                                       fill=subpop))+ 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_preopercular, 
             aes(x=subpop, y=log10.mean_preopercular_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop, y=log10.mean_preopercular_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  scale_fill_manual(values=c("#4477AA", "#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Benthic", "Intermediate", "Littoral")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean preopercular pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("littoral", "benthic")), y_position=-0.2, annotations="***") +
  geom_signif(comparison=list(c("middle", "benthic")), y_position=-0.3, annotations="NS") +
  geom_signif(comparison=list(c("littoral", "middle")), y_position=-0.4, annotations="*") 
subpop_plot_preopercular

#depth cat
depth_plot_preopercular <- ggplot(data=partial_residuals_preopercular_depth, 
                                  aes(x=factor(Depth_Cat), 
                                      y=log10.mean_preopercular_pore_area.)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8, fill="gray") + 
  geom_point(data=partial_residuals_preopercular_depth, 
             aes(x=Depth_Cat, y=log10.mean_preopercular_pore_area., color=pore_areas_merged_corrected$subpop), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=Depth_Cat, y=log10.mean_preopercular_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA","#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Deep", "Midwater", "Shallow")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean preopercular pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("Shallow", "Deep")), y_position=-0.2, annotations="***") +
  geom_signif(comparison=list(c("Intermediate", "Deep")), y_position=-0.3, annotations="***") +
  geom_signif(comparison=list(c("Shallow", "Intermediate")), y_position=-0.4, annotations="NS")
depth_plot_preopercular

##orbital canal
#subpop
subpop_plot_orbital <- ggplot(data=partial_residuals_orbital, 
                              aes(x=factor(subpop, levels=c("benthic", "middle", "littoral")), 
                                  y=log10.mean_orbital_pore_area., 
                                  fill=subpop))+ 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_orbital, 
             aes(x=subpop, y=log10.mean_orbital_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop, y=log10.mean_orbital_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  scale_fill_manual(values=c("#4477AA", "#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Benthic", "Intermediate", "Littoral")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean orbital pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("littoral", "benthic")), y_position=-0.3, annotations="***") +
  geom_signif(comparison=list(c("middle", "benthic")), y_position=-0.4, annotations="***") +
  geom_signif(comparison=list(c("littoral", "middle")), y_position=-0.5, annotations="*") 
subpop_plot_orbital


#depth cat
depth_plot_orbital <- ggplot(data=partial_residuals_orbital_depth, 
                             aes(x=factor(Depth_Cat), 
                                 y=log10.mean_orbital_pore_area.)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8, fill="gray") + 
  geom_point(data=partial_residuals_orbital_depth, 
             aes(x=Depth_Cat, y=log10.mean_orbital_pore_area., color=pore_areas_merged_corrected$subpop), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=Depth_Cat, y=log10.mean_orbital_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA","#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Deep", "Midwater", "Shallow")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean orbital pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("Shallow", "Deep")), y_position=-0.3, annotations="***") +
  geom_signif(comparison=list(c("Intermediate", "Deep")), y_position=-0.4, annotations="***") +
  geom_signif(comparison=list(c("Shallow", "Intermediate")), y_position=-0.5, annotations="***")
depth_plot_orbital


##otic canal
#subpop
subpop_plot_otic <- ggplot(data=partial_residuals_otic, 
                           aes(x=factor(subpop, levels=c("benthic", "middle", "littoral")), 
                               y=log10.mean_otic_pore_area., 
                               fill=subpop)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_otic, 
             aes(x=subpop, y=log10.mean_otic_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop, y=log10.mean_otic_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  scale_fill_manual(values=c("#4477AA", "#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Benthic", "Intermediate", "Littoral")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean otic pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("littoral", "benthic")), y_position=-0.3, annotations="***") +
  geom_signif(comparison=list(c("middle", "benthic")), y_position=-0.4, annotations="*") +
  geom_signif(comparison=list(c("littoral", "middle")), y_position=-0.5, annotations="NS") 
subpop_plot_otic


#depth cat
depth_plot_otic <- ggplot(data=partial_residuals_otic_depth, 
                          aes(x=factor(Depth_Cat), 
                              y=log10.mean_otic_pore_area.)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8, fill="gray") + 
  geom_point(data=partial_residuals_otic_depth, 
             aes(x=Depth_Cat, y=log10.mean_otic_pore_area., color=pore_areas_merged_corrected$subpop), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=Depth_Cat, y=log10.mean_otic_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  scale_color_manual(values=c("#4477AA","#DDAA33","gray")) + 
  scale_x_discrete(labels=c("Deep", "Midwater", "Shallow")) + 
  coord_flip() + 
  ylab(expression(paste("Log mean otic pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") +
  geom_signif(comparison=list(c("Shallow", "Deep")), y_position=-0.3, annotations="***") +
  geom_signif(comparison=list(c("Intermediate", "Deep")), y_position=-0.4, annotations="***") +
  geom_signif(comparison=list(c("Shallow", "Intermediate")), y_position=-0.5, annotations="NS")
depth_plot_otic

## combined plot
library(cowplot)
combined_pore_plot_raincloud <- cowplot::plot_grid(subpop_plot_mandibular,
                                                   subpop_plot_preopercular,
                                                   subpop_plot_orbital,
                                                   subpop_plot_otic,
                                                   depth_plot_mandibular,
                                                   depth_plot_preopercular,
                                                   depth_plot_orbital, 
                                                   depth_plot_otic, 
                                         nrow=2, align="vh")
combined_pore_plot_raincloud

png(file="combined_pore_plot_raincloud.png",
    units="in",
    width=16, 
    height=5, 
    res=300, 
    type="cairo-png")
combined_pore_plot_raincloud
dev.off()

pdf(file="combined_pore_plot_raincloud.pdf",
    width=16, 
    height=5)
combined_pore_plot_raincloud
dev.off()

#### Figure 4 ####

# head sns
head_sns_plot <- ggplot(data=partial_residuals_head_sns, aes(x=ECOMORPH, y=TOTAL_HEAD_SN)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, fill="gray", color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_head_sns, aes(x=ECOMORPH, y=TOTAL_HEAD_SN), position=position_jitter(width=.15), size=.75) + 
  geom_boxplot(aes(x=ECOMORPH, y=TOTAL_HEAD_SN), position=position_nudge(x=.25,y=0),outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  ylab("Standardised total number of head superifical neuromasts") + 
  theme(axis.title.y = element_blank(), legend.position="none") + 
  scale_x_discrete(labels=c("Deep", "Shallow"))
head_sns_plot 

# trunk sns
trunk_sns_plot <- ggplot(data=partial_residuals_trunk_sns, aes(x=ECOMORPH, y=TOTAL_TRUNK_SN))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, fill="gray", color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_trunk_sns, aes(x=ECOMORPH, y=TOTAL_TRUNK_SN), position=position_jitter(width=.15), size=.75) + 
  geom_boxplot(aes(x=ECOMORPH, y=TOTAL_TRUNK_SN), position=position_nudge(x=.25,y=0),outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  ylab("Standardised total number of trunk superifical neuromasts") + 
  theme(axis.title.y = element_blank(), legend.position="none") + 
  scale_x_discrete(labels=c("Deep", "Shallow"))
trunk_sns_plot 

# trunk cns
trunk_cns_plot <- ggplot(data=partial_residuals_trunk_cns, aes(x=ECOMORPH, y=TOTAL_TRUNK_CN))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, fill="gray", color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_trunk_cns, aes(x=ECOMORPH, y=TOTAL_TRUNK_CN), position=position_jitter(width=.15), size=.75) + 
  geom_boxplot(aes(x=ECOMORPH, y=TOTAL_TRUNK_CN), position=position_nudge(x=.25,y=0),outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  ylab("Standardised total number of trunk canal neuromasts") + 
  theme(axis.title.y = element_blank(), legend.position="none") + 
  scale_x_discrete(labels=c("Deep", "Shallow"))
trunk_cns_plot

# sns_per_cn
sns_per_cn_plot <- ggplot(data=partial_residuals_sns_per_cn, aes(x=ECOMORPH, y=AVERAGE_TRUNK_SNS_PER_CN)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, fill="gray", color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_sns_per_cn, aes(x=ECOMORPH, y=AVERAGE_TRUNK_SNS_PER_CN), position=position_jitter(width=.15), size=.75) + 
  geom_boxplot(aes(x=ECOMORPH, y=AVERAGE_TRUNK_SNS_PER_CN), position=position_nudge(x=.25,y=0),outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  ylab("Standardised mean number of superficial neuromasts per trunk canal neuromast") + 
  theme(axis.title.y = element_blank(), legend.position="none") + 
  scale_x_discrete(labels=c("Deep", "Shallow"))
sns_per_cn_plot

# composite
combined_neuromast_plot_rainclouds <- cowplot::plot_grid(head_sns_plot, trunk_sns_plot,
                                                         trunk_cns_plot, sns_per_cn_plot, nrow=4)
combined_neuromast_plot_rainclouds


#### Figure 5 ####

## pore areas
#mandibular
malawi_mandibular_plot <- ggplot(data=partial_residuals_mandibular_malawi, 
                                 aes(x=factor(subpop_for_graph, 
                                              levels=c("deep_benthic", 
                                                       "shallow_benthic", 
                                                       "diplotaxodon", 
                                                       "rhamphochromis", 
                                                       "utaka", 
                                                       "mbuna", 
                                                       "benthic", "middle","littoral")),
                                     y=log10.malawi_mandibular_mean_pore_area., 
                                     fill=subpop_for_graph)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_mandibular_malawi, 
             aes(x=subpop_for_graph, y=log10.malawi_mandibular_mean_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.malawi_mandibular_mean_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon", 
                            "Rhamphochromis", "utaka", "mbuna", 
                            "Masoko benthic", "Masoko intermediate", "Masoko littoral")) +
  scale_fill_manual(values=c("#4477AA", "#CCDDAA","#CCDDAA", 
                             "#DDAA33", "#CCDDAA", "gray", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab(expression(paste("Mean mandibular pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")
malawi_mandibular_plot

#preopercular
malawi_preopercular_plot <- ggplot(data=partial_residuals_preopercular_malawi, 
                                   aes(x=factor(subpop_for_graph, 
                                                levels=c("deep_benthic", 
                                                         "shallow_benthic", 
                                                         "diplotaxodon", 
                                                         "rhamphochromis", 
                                                         "utaka", 
                                                         "mbuna", 
                                                         "benthic", "middle","littoral")),
                                       y=log10.malawi_preopercular_mean_pore_area., 
                                       fill=subpop_for_graph)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=4, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_preopercular_malawi, 
             aes(x=subpop_for_graph, y=log10.malawi_preopercular_mean_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.malawi_preopercular_mean_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon", 
                            "Rhamphochromis", "utaka", "mbuna", 
                            "Masoko benthic", "Masoko intermediate", "Masoko littoral")) +
  scale_fill_manual(values=c("#4477AA", "#CCDDAA","#CCDDAA", 
                             "#DDAA33", "#CCDDAA", "gray", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab(expression(paste("Mean preopercular pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_preopercular_plot

#orbital
malawi_orbital_plot <- ggplot(data=partial_residuals_orbital_malawi, 
                              aes(x=factor(subpop_for_graph, 
                                           levels=c("deep_benthic", 
                                                    "shallow_benthic", 
                                                    "diplotaxodon", 
                                                    "rhamphochromis", 
                                                    "utaka", 
                                                    "mbuna", 
                                                    "benthic", "middle","littoral")),
                                  y=log10.malawi_orbital_mean_pore_area., 
                                  fill=subpop_for_graph))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_orbital_malawi, 
             aes(x=subpop_for_graph, y=log10.malawi_orbital_mean_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.malawi_orbital_mean_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon", 
                            "Rhamphochromis", "utaka", "mbuna", 
                            "Masoko benthic", "Masoko intermediate", "Masoko littoral")) +
  scale_fill_manual(values=c("#4477AA", "#CCDDAA","#CCDDAA", 
                             "#DDAA33", "#CCDDAA", "gray", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab(expression(paste("Mean infraorbital pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_orbital_plot


#otic
malawi_otic_plot <- ggplot(data=partial_residuals_otic_malawi, 
                           aes(x=factor(subpop_for_graph, 
                                        levels=c("deep_benthic", 
                                                 "shallow_benthic", 
                                                 "diplotaxodon", 
                                                 "rhamphochromis", 
                                                 "utaka", 
                                                 "mbuna", 
                                                 "benthic", "middle","littoral")),
                               y=log10.malawi_otic_mean_pore_area., 
                               fill=subpop_for_graph)) + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_otic_malawi, 
             aes(x=subpop_for_graph, y=log10.malawi_otic_mean_pore_area.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.malawi_otic_mean_pore_area.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon", 
                            "Rhamphochromis", "utaka", "mbuna", 
                            "Masoko benthic", "Masoko intermediate", "Masoko littoral")) +
  scale_fill_manual(values=c("#4477AA", "#CCDDAA","#CCDDAA", 
                             "#DDAA33", "#CCDDAA", "gray", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab(expression(paste("Mean otic pore area"~ (mm^2)))) + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_otic_plot


## neuromast count
#trunk CN
malawi_trunk_CN_plot <- ggplot(data=partial_residuals_neuromasts_trunk_CN_malawi, 
                               aes(x=factor(subpop_for_graph, levels=c("deep benthic", "shallow benthic",
                                                                       "diplotaxodon", "rhamphochromis",
                                                                       "mbuna", 
                                                                       "calliptera salima", "masoko benthic", "masoko littoral")),
                                   y=log10.TOTAL_TRUNK_CN., 
                                   fill=subpop_for_graph))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_neuromasts_trunk_CN_malawi, 
             aes(x=subpop_for_graph, y=log10.TOTAL_TRUNK_CN.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.TOTAL_TRUNK_CN.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon",
                            "Rhamphochromis",  "mbuna", 
                            "A. calliptera salima", "Masoko deep", "Masoko shallow")) +
  scale_fill_manual(values=c("#CCDDAA", "#CCDDAA","#CCDDAA", 
                             "#4477AA", "#DDAA33", "#CCDDAA", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab("log total number of trunk canal neuromasts") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_trunk_CN_plot

#trunk SN
malawi_trunk_SN_plot <- ggplot(data=partial_residuals_neuromasts_trunk_SN_malawi, 
                               aes(x=factor(subpop_for_graph, levels=c("deep benthic", "shallow benthic",
                                                                       "diplotaxodon", "rhamphochromis",
                                                                       "mbuna", 
                                                                       "calliptera salima", "masoko benthic", "masoko littoral")),
                                   y=log10.TOTAL_TRUNK_SN., 
                                   fill=subpop_for_graph))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_neuromasts_trunk_SN_malawi, 
             aes(x=subpop_for_graph, y=log10.TOTAL_TRUNK_SN.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.TOTAL_TRUNK_SN.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon",
                            "Rhamphochromis", "mbuna", 
                            "A. calliptera salima", "Masoko deep", "Masoko shallow")) +
  scale_fill_manual(values=c("#CCDDAA", "#CCDDAA","#CCDDAA", 
                             "#4477AA", "#DDAA33", "#CCDDAA", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab("log total number of trunk superficial neuromasts") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_trunk_SN_plot


#head SN
malawi_head_SN_plot <- ggplot(data=partial_residuals_neuromasts_head_SN_malawi, 
                              aes(x=factor(subpop_for_graph, levels=c("deep benthic", "shallow benthic",
                                                                      "diplotaxodon", "rhamphochromis",
                                                                      "mbuna", 
                                                                      "calliptera salima", "masoko benthic", "masoko littoral")),
                                  y=log10.TOTAL_HEAD_SN., 
                                  fill=subpop_for_graph))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_neuromasts_head_SN_malawi, 
             aes(x=subpop_for_graph, y=log10.TOTAL_HEAD_SN.), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=log10.TOTAL_HEAD_SN.), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon",
                            "Rhamphochromis",  "mbuna", 
                            "A. calliptera salima", "Masoko deep", "Masoko shallow")) +
  scale_fill_manual(values=c("#CCDDAA", "#CCDDAA","#CCDDAA", 
                             "#4477AA", "#DDAA33", "#CCDDAA", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab("log total number of head superficial neuromasts") + 
  theme(axis.title.y = element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_head_SN_plot

#SNS per cn
malawi_average_sns_per_cn_plot <- ggplot(data=partial_residuals_neuromasts_trunk_sns_per_cn_malawi, 
                                         aes(x=factor(subpop_for_graph, levels=c("deep benthic", "shallow benthic",
                                                                                 "diplotaxodon", "rhamphochromis",
                                                                                 "mbuna", 
                                                                                 "calliptera salima", "masoko benthic", "masoko littoral")),
                                             y=AVERAGE_TRUNK_SNS_PER_CN, 
                                             fill=subpop_for_graph))  + 
  geom_flat_violin(position=position_nudge(x=.25, y=0), adjust=2, color=NA, alpha=0.8) + 
  geom_point(data=partial_residuals_neuromasts_trunk_sns_per_cn_malawi, 
             aes(x=subpop_for_graph, y=AVERAGE_TRUNK_SNS_PER_CN), 
             position=position_jitter(width=.15), 
             size=.75) + 
  geom_boxplot(aes(x=subpop_for_graph, y=AVERAGE_TRUNK_SNS_PER_CN), 
               position=position_nudge(x=.25,y=0), 
               outlier.shape=NA, alpha=0.3, width=.1, colour="black") + 
  theme_classic() + 
  coord_flip() + 
  scale_x_discrete(labels=c("Deep benthic", "Shallow benthic", "Diplotaxodon",
                            "Rhamphochromis",  "mbuna", 
                            "A. calliptera salima", "Masoko deep", "Masoko shallow")) +
  scale_fill_manual(values=c("#CCDDAA", "#CCDDAA","#CCDDAA", 
                             "#4477AA", "#DDAA33", "#CCDDAA", 
                             "#CCDDAA", "#CCDDAA", "#CCDDAA")) + 
  ylab("log average superficial neuromasts per canal cluster") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="none") + 
  geom_vline(xintercept=6.5, linetype="dashed")

malawi_average_sns_per_cn_plot

#composite
composite_malawi_plot <- cowplot::plot_grid(malawi_mandibular_plot, malawi_preopercular_plot,
                                                          malawi_orbital_plot, malawi_otic_plot,
                                                          malawi_head_SN_plot, malawi_trunk_SN_plot,
                                                          malawi_trunk_CN_plot, malawi_average_sns_per_cn_plot,
                                                          nrow=2, align="vh") 
composite_malawi_plot
