# Goff-et-al_GLORIA_Manuscript
Repository for code, data, and figures related to Goff et al. manuscript: "Stability of alpine plant communities over time in western North America"

## File Manifest:

### code:

01 - 03B.R (a total of 12 scripts) are used to pull the GLORIA Great Basin summit data, filter this to the summit-area-section data type, and complete a large number of cleaning steps. The outcome of these steps is a clean data file (SAS_clean_data.csv). The clean summit-area-section data is included in this repository, but the preceding data files are not because they are very large.

04_richness_model_figure2.R - Performs species richness analysis, including statistical analysis and creating manuscript Figure 2..

05_turnover_binomial_model_figure3_figureS1_figureS3.R - Performs turnover analysis, using a modified function "turnover"" in the package codyn to compute turnover between pairs of sequential surveys on the same peak, and feeding this data into statistical analysis. These results are used to create Figure 3 in the manuscript, as well as supplemental Figure S1 and S3.

05A_modify_codyn_turnover.R - makes one small modification to the source code of codyn (Hallet et al. 2013), so that gains and losses are not proportional to the total species richness of a given replicate. Also creates a function to calculate total richness separately.

06_func-gr_gain-loss_binomial_figure4.R - Performs another set of turnover analyses for each functional group (forb, cushion, graminoid, shrub/tree), again uses this for statistical analysis. Outputs are Figure 4.

07_abundance_models_table1_figure5_figureS2_tableS4.R - Statistical analysis of change in abundance ranks (r!, r, s, c, d) of the four functional groups using cumulative link models. This script creates Table 1, Figure 5, and supplementary materials Figure S2, and Table S4.

08_climateNA_and_logger_data_fig1bcd.R - This script visualizes the mean annual temperature, mean annual precipitation and climatic moisture deficit for the 8 target regions included in this study, which is used to create panels b and c in Figure 1. This script also conducts a statistical analysis of changes in the three climate variables of interest from 2004-2022. Next, we create a model for soil temp change over time from the logger data, make figure 1d, showing logger data over time for each tr.

08A_climate_and_elevation_setup.R - Imports and preps GLORIA summit data for Climate NA, including downloading elevations using the elevatr package.

09_turnover_null_model.R - This script is used to create a null turnover model to use as a basis for comparison for the turnover model created in script 05_. 

### data-raw:

plot_elevation_information.csv - Elevation, latitude and longitude for all 29 peaks included in this study.

GGB_summit_survey_acknowledgments_raw_manual_cleaned_kag.csv - List of volunteers who assisted with the field component of this study over nearly 20 years!

### data-derived:

SAS_clean_data.csv - This is the clean summit-area-section data used for the analyses in this manuscript.

functional_group_sheet_V2.csv - Functional group assignments (forb, graminoid, cushion, shrub/tree) for all species included in this study.

ggb_geo_elev_forClimNA_1901-2022MSY.csv - Raw output of climate data for each peak from ClimateNA.

### figures:

Figure1 - study system map, macroclimate data and soil temperature data

Figure2 - Species richness change over time

Figure3 - global turnover

Figure4 - turnover by functional group

Figure5 - abundance change over type by functional group

FigureS1 - comparison between null turnover and observed turnover

FigureS2 - shrub/tree functional group by target region interaction

FigureS3 - proportional gain plotted against proportional loss for each target region
