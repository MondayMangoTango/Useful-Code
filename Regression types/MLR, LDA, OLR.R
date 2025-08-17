setwd("/Users/dgg/Desktop")

#Packages (core functions)
library(data.table)
library(dplyr)
library(ggplot2)
library(rfishbase)
library(patchwork)

#Packages (regressions)
library(bestNormalize)
library(effects)
library(ggeffects) 
library(MASS)
library(nnet)
library(tibble)
library(tidyr)

'#### Database loading'
#Swimways
Swimways_raw <- fread("Swimways_Database.csv")
  colnames(Swimways_raw) <- make.names(Swimways_raw[2, ], unique = TRUE)
  Swimways_raw <- Swimways_raw[-(1:2), ]
Swimways_FishBase_Eschmeyer_Comparison <- fread("Fishbase - Eschmeyer's Catalogue comparison.csv")

#Fishmorph
FISHMORPH_raw <- fread("FISHMORPH_Database.csv")
ReadMe <- fread("FISHMORPH_Readme.csv")
#acquired from https://rodare.hzdr.de/record/1316, full paper here: https://www.researchgate.net/publication/354654635_FISHMORPH_A_global_database_on_morphological_traits_of_freshwater_fishes
 
'##### Code Appendix'
#1. Fishbase data - line 60
#2. Merging fishbase data with abbreviated swimways - line 76
#3. Merging Fishmorph with Swimways_FishBase_Merged - line 83
#4. Multinomial logistic regression (MLR) & linear discriminant analysis (LDA)for migration category - line 87
#5. Ordinal logistic regression (OLR) for migration distance - line 256

'##### Variable interpretation for FishMorph:'
# [1] "SpecCode"             "Suporder"             "Order"                "Family"               "Genus species"   

        'Variable (calculation)                                                 Hypothetical relevance'
# [6] "MBl" = maximum body length                                             - metabolism, trophic impacts, locomotion ability, nutrient cycling            
# [7] "BEl" = body elongation (body length / body depth)                      - hydrodynamism      
# [8] "VEp" = vertical eye position (eye position / body depth).              - position of fish and/or prey in water column             
# [9] "REs" = relative eye size (eye diameter / head depth)                   - visual accuity      
# [10] "OGp" = oral gape position (mouth height / body depth)                 - feeding position in the water column 
# [11] "RMl" = relative maxillary length (maxillary jaw length / head depth)  - mouth size, jaw strength
# [12] "BLs" = body lateral shape (head depth / body depth)                   - hydrodynamism and head size
# [13] "PFv" = pectoral fin vertical position (p. fin position / body depth)  - pectoral fin use for swimming
# [14] "PFs" = pectoral fin size (p. fin length / body length)                - pectoral fin use for swimming
# [15] "CPt" = caudal peduncle throttling (caudal fin depth / caudal p.depth) - caudal propulsion efficiency through reduction of drag
        
# [16] "Type of illustration" "Reference"    
        
        
'#### Pairing Fishmorph with Swimways'
#note - I did this with fishbase species codes initially; if the theory holds water, we can do it properly with ECOF names/codes. 


#1. Fishbase data
options(FISHBASE_VERSION="23.05")

species_list <- as.vector(Swimways_raw$Catalogue.of.Fishes.name)
fields <- c("Species", "SpecCode", "Genus", "SpeciesRefNo", "FBname", "Subfamily", "BodyShapeI", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "Importance",
                    "PriceCateg", "MainCatchingMethod", "UsedforAquaculture", "UsedasBait", "GameFish")

Swimways_FBdata <- species(
  species_list = species_list,
  fields = fields,
  server = getOption("FISHBASE_API"))

Swimways_FBdata <- Swimways_FBdata %>% rename(Species_Binomial = Species) #just for clarity between scientific and common name columns 
Swimways_FBdata <- Swimways_FBdata %>% rename(Species_CommonName = FBname)


#2. Merging fishbase data with abbreviated swimways
Swimways_raw_abbreviated <- Swimways_raw[ ,c(1,19:28)] #note - about 188 species dropped off here, lowering total from 2445 in Swimways to 2257 w/ paired FB info. 
Swimways_raw_abbreviated <- Swimways_raw_abbreviated %>% rename(Species_Binomial = Catalogue.of.Fishes.name) #for merging, both this and Swimways_FBdata need the same column name.

Swimways_FishBase_Merged <- merge(Swimways_FBdata, Swimways_raw_abbreviated, by = "Species_Binomial")


#3. Merging Fishmorph with Swimways_FishBase_Merged
Fishmorph_Swimways_Merged <- merge(Swimways_FishBase_Merged, FISHMORPH_raw, by = "SpecCode") #merge will only keep species in both databases (n = 1,600)


#4. Multinomial logistic regression (MLR) & linear discriminant analysis (LDA)for migration category. 
   #MLR estimates individiaul coefficients for each trait → 
        #each coefficient is the effect of that trait holding others constant.
   #LDA creates linear combinations of traits that maximize group separation → 
        #importance reflects how much each trait contributes to the discriminant axes, not direct outcome probability.

    #trimming of low-confidence, coastal (as not a migration category)
Fishmorph_Swimways_Merged_ConfidentMigrationDistances <- Fishmorph_Swimways_Merged[Fishmorph_Swimways_Merged$Migration_Length != "little information", ] #taking out unknowns
Fishmorph_Swimways_Merged_ConfidentMigrationDistances <- 
  Fishmorph_Swimways_Merged_ConfidentMigrationDistances[!Fishmorph_Swimways_Merged_ConfidentMigrationDistances$Confidence_Migration_Length %in% c(0, 1),]#taking out low confidences
Fishmorph_Swimways_Merged_ConfidentMigrationDistances <- 
  Fishmorph_Swimways_Merged_ConfidentMigrationDistances[!Fishmorph_Swimways_Merged_ConfidentMigrationDistances$Overall_Migration_Class %in% c("coastal"),]

Fishmorph_Swimways_Merged_MigCat <- Fishmorph_Swimways_Merged_ConfidentMigrationDistances[,c(17,31:40)]

    #renaming for legibility
Fishmorph_Swimways_Merged_MigCat <- Fishmorph_Swimways_Merged_MigCat %>% rename(
  Maximum_body_length = MBl,
  Body_elongation = BEl,
  Vertical_eye_position = VEp,
  Relative_eye_size = REs, 
  Oral_gape_position = OGp,
  Relative_maxillary_length = RMl,
  Body_lateral_shape = BLs, 
  Pectoral_fin_vertical_position = PFv,
  Pectoral_fin_size = PFs,
  Caudal_peduncle_throttling = CPt)
  
    #standardization & normalization of variables
traits <- c("Maximum_body_length","Body_elongation","Vertical_eye_position",
            "Relative_eye_size", "Oral_gape_position", "Relative_maxillary_length",
            "Body_lateral_shape", "Pectoral_fin_vertical_position", "Pectoral_fin_size", "Caudal_peduncle_throttling") # replace with your trait names

Fishmorph_Swimways_Merged_MigCat_norm <- Fishmorph_Swimways_Merged_MigCat

for (tr in traits) {
  norm_obj <- bestNormalize(Fishmorph_Swimways_Merged_MigCat_norm[[tr]], standardize = FALSE)
  Fishmorph_Swimways_Merged_MigCat_norm[[tr]] <- predict(norm_obj)}

Fishmorph_Swimways_Merged_MigCat_scaled <- Fishmorph_Swimways_Merged_MigCat_norm
Fishmorph_Swimways_Merged_MigCat_scaled[, traits] <- scale(Fishmorph_Swimways_Merged_MigCat_norm[, traits])

    #checking for normalicy (regression assumption)
boxplot(
  Fishmorph_Swimways_Merged_MigCat_scaled$Maximum_body_length,
  Fishmorph_Swimways_Merged_MigCat_scaled$Body_elongation,
  Fishmorph_Swimways_Merged_MigCat_scaled$Vertical_eye_position,
  Fishmorph_Swimways_Merged_MigCat_scaled$Relative_eye_size,
  Fishmorph_Swimways_Merged_MigCat_scaled$Oral_gape_position,
  Fishmorph_Swimways_Merged_MigCat_scaled$Relative_maxillary_length,
  Fishmorph_Swimways_Merged_MigCat_scaled$Body_lateral_shape,
  Fishmorph_Swimways_Merged_MigCat_scaled$Pectoral_fin_vertical_position,
  Fishmorph_Swimways_Merged_MigCat_scaled$Pectoral_fin_size,
  Fishmorph_Swimways_Merged_MigCat_scaled$Caudal_peduncle_throttling,
  names = c(
    "Max body length", "Body elongation", "Vertical eye pos", 
    "Rel. eye size", "Oral gape pos", "Rel. maxillary length", 
    "Body lateral shape", "Pectoral fin vert pos", "Pectoral fin size", 
    "Caudal peduncle throttling"),
  las = 2,           # rotate x-axis labels
  main = "Morphological Traits",
  col = "lightblue") #body length, body elongation, maxillary length, throttling strongly skewed high

    #multinominal logistic regression (MLR)
Fishmorph_Swimways_Merged_MigCat_scaled$Overall_Migration_Class <- as.factor(Fishmorph_Swimways_Merged_MigCat_scaled$Overall_Migration_Class) #standardization moves the mean to 0, with variance up to 1.
mlr_model <- multinom(Overall_Migration_Class ~ ., data = Fishmorph_Swimways_Merged_MigCat_scaled[, c("Overall_Migration_Class", traits)])

    #summary stats, significance (MLR)
summary_mlr <- summary(mlr_model)
z <- summary_mlr$coefficients / summary_mlr$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))

    #plot generation (MLR)
effs <- allEffects(mlr_model)  
for(tr in traits){
  plot(effs[tr], main = paste("Predicted probabilities by", tr))
}
    
    #linear discriminate analysis (LDA)
    #trimming Fishmorph_Swimways_Merged_MigCat_scaled to match LDA rows for compatibility (LDA)
lda_cols <- c("Overall_Migration_Class", traits)
complete_idx <- complete.cases(Fishmorph_Swimways_Merged_MigCat_scaled[, lda_cols])
lda_df <- Fishmorph_Swimways_Merged_MigCat_scaled[complete_idx, lda_cols]
lda_model <- lda(Overall_Migration_Class ~ ., data = lda_df)
lda_pred <- predict(lda_model)

    #plot creation (LDA)
lda_vis_df <- data.frame(lda_pred$x, MigrationType = lda_df$Overall_Migration_Class)

ggplot(lda_vis_df, aes(x = LD1, y = LD2, color = MigrationType)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "norm", level = 0.95) +
  theme_minimal() +
  labs(title = "LDA: Migration Type Separation")

     #ranking trait importance (LDA)
mlr_z <- apply(z, 2, function(x) mean(abs(x))) #average absolute z-scores across categories (MLR)
mlr_imp <- data.frame(Trait = names(mlr_z), Importance = mlr_z)

lda_imp <- data.frame(Trait = rownames(lda_model$scaling),
                      Importance = apply(abs(lda_model$scaling), 1, mean)) #absolute discriminant loadings

importance_traits <- merge(mlr_imp, lda_imp, by = "Trait", suffixes = c("_MLR", "_LDA")) #merging & ranking
importance_traits_long <- importance_traits %>% tidyr::pivot_longer(-Trait, names_to = "Method", values_to = "Importance")

    #plot creation (LDA scatterplot)
lda_vis_df <- data.frame(
  LD1 = lda_pred$x[, 1],
  LD2 = lda_pred$x[, 2],
  MigrationType = lda_df$Overall_Migration_Class)

    #2D scatter plot
ggplot(lda_vis_df, aes(x = LD1, y = LD2, color = MigrationType)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(
    title = "LDA: Migration Type Separation",
    x = "LD1",
    y = "LD2",
    color = "Migration Type"
  ) +
  scale_color_manual(values = c(
    "anadromous" = "#ff7f0e",
    "catadromous" = "#2ca02c",
    "non-migratory" = "#d62728",
    "potamodromous, longitudinal" = "#9467bd",
    "potamodromous, lateral" = "#000099",
    "amphidromous" = "#1f77b4"))


lda_loadings <- as.data.frame(lda_model$scaling) %>%
  rownames_to_column("Trait") %>%
  pivot_longer(-Trait, names_to = "LD", values_to = "Loading")
lda_loadings <- lda_loadings %>% filter(LD %in% c("LD1", "LD2"))

ggplot(lda_loadings, aes(x = reorder(Trait, abs(Loading)), y = Loading, fill = LD)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "LDA Trait Loadings", y = "Coefficient (loading)", x = "Trait")

     #plot creation (LDA + MLR)
mlr_df <- importance_traits_long %>% filter(Method == "Importance_MLR") %>%
  arrange(Importance) %>%
  mutate(Trait_ordered = factor(Trait, levels = Trait))  # sort descending

lda_df <- importance_traits_long %>% filter(Method == "Importance_LDA") %>%
  arrange(Importance) %>%
  mutate(Trait_ordered = factor(Trait, levels = Trait))  # sort descending

     #plot MLR + LDA graph
MLR_importance_graph <- ggplot(mlr_df, aes(x = Trait_ordered, y = Importance, fill = Method)) +
  geom_col(fill = "#6600CC") +
  coord_flip() +
  theme_minimal() +
  labs(title = "MLR Trait Importance \n (individual effect on mig. class)", x = "Trait", y = "Importance") +
  theme(legend.position = "none")

LDA_importance_graph <- ggplot(lda_df, aes(x = Trait_ordered, y = Importance, fill = Method)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  theme_minimal() +
  labs(title = "LDA Trait Importance \n (contribution to mig. class distinction)", x = "Trait", y = "Importance") +
  theme(legend.position = "none")

MLR_importance_graph + LDA_importance_graph



#5. Ordinal logistic regression (OLR) for migration distance. Regression selected for its recognition of ranked (i.e., ascending) categorical variables (0-100 km, 100-500 km, 500-3000km, 3000+ km)
     #cleaning for a signal
Fishmorph_Swimways_Merged_ConfidentMigrationDistances <- Fishmorph_Swimways_Merged[Fishmorph_Swimways_Merged$Migration_Length != "little information", ] #taking out unknowns
Fishmorph_Swimways_Merged_ConfidentMigrationDistances <- 
  Fishmorph_Swimways_Merged_ConfidentMigrationDistances[!Fishmorph_Swimways_Merged_ConfidentMigrationDistances$Confidence_Migration_Length %in% c(0, 1),]#taking out low confidences

Fishmorph_Swimways_Merged_ConfidentMigrationDistances$Migration_Length <- ordered(Fishmorph_Swimways_Merged_ConfidentMigrationDistances$Migration_Length,
                                         levels = c("non-migratory", "0-100(km)", "100-500(km)", "500-3000(km)", "3000 + (km)"))
    #renaming for legibility
Fishmorph_Swimways_Merged_ConfidentMigrationDistances <- Fishmorph_Swimways_Merged_ConfidentMigrationDistances %>% rename(
  Maximum_body_length = MBl,
  Body_elongation = BEl,
  Vertical_eye_position = VEp,
  Relative_eye_size = REs, 
  Oral_gape_position = OGp,
  Relative_maxillary_length = RMl,
  Body_lateral_shape = BLs, 
  Pectoral_fin_vertical_position = PFv,
  Pectoral_fin_size = PFs,
  Caudal_peduncle_throttling = CPt)

    #fitting ordinal logistic regression
ord_model_MigDistances <- polr(Migration_Length ~ Maximum_body_length + Body_elongation + Vertical_eye_position + 
                                 Relative_eye_size + Oral_gape_position + Relative_maxillary_length + 
                                 Body_lateral_shape + Pectoral_fin_vertical_position + Pectoral_fin_size + Caudal_peduncle_throttling,
                  data = Fishmorph_Swimways_Merged_ConfidentMigrationDistances, Hess = TRUE)
summary(ord_model_MigDistances)

ctable <- coef(summary(ord_model_MigDistances))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_values)

plot(allEffects(ord_model_MigDistances))
 ##Note to interpretation:
    #the x axis is the range of values included in the database. for instance, body length (MBl) scales from 0-800cm, whereas the other values are ratios. 
    #the blue line signifies predicted probability of being in each migration distance category, holding all other predictors at their average.

