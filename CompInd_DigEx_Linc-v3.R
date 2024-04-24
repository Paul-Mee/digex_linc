############
### Composite Index formation 
### Digital Exclusion Index - Following Severity Index vignette
### https://humanitarian-user-group.github.io/post/compositeindicator/
############

### To Do 
### Sort out renames
### Include uniqueness and communality
### Produce map in a function
### Do Low/Low High/High analysis

# Clear any existing data from the data set
rm(list = ls())

### Load packages and libraries

# pacman only needs to be installed once
# install.packages("pacman")

# Define vector of package names
# NB Some of these packages may have been used in the development 
# But are no longer needed in this version of the code

package_names <- c("readr","readxl","dplyr","ggplot2","corrplot","psych",
                   "scales","ggiraph","ggrepel","Compind","ggcorrplot","kableExtra",
                   "reshape2","qgraph", "sf","cartography",
                   "raster", "dismo", "rgdal", "SpatialPosition","ggpubr",
                   "devtools","PerformanceAnalytics","hrbrthemes",
                   "Hmisc","PerformanceAnalytics","psych","Rcsdp","GPArotation",
                   "BBmisc","rgdal", "leaflet","sp","plotly","htmlwidgets","mapview",
                   "forcats","htmltools","Matrix","schoRsch")


# This code installs all the other required packages if they are not currently installed and load all the libraries
pacman::p_load(char=package_names)
rm(package_names)





#### Defining functions which will be used in the code
### Functions 


# This small function is used to have nicely left align text within 
# charts produced with ggplot2
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}
# 

### This makes a few tweaks to Chart Correlation
Chart.Correlation_PM <- function (R, histogram = TRUE, method_corr ,cex,sym_cex, ...) 
{
  x = checkData(R, method = "matrix")

  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = method_corr, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel, ...)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,cex=sym_cex, ...)
}

# ind_vec <- (c('la_name','lsoa_name','lsoa_code','gp_pt15_pc','gp_pt30_pc','gp_car15_pc','pharm_car_15_pc'))
# ind_df <- DER_raw_norm.df
# pk_n <- 3
# corr_method = "pearson"
# dim_factor <- 3

### This function generates the correlation plot and network diagram for the input variables 

corr_gen <- function(ind_df,ind_vec,pk_n,corr_method,out_dir,corr_plot_name,net_plot_name){
  ### Create a dataframe with just the required variables
  tmp.df <- data.frame(ind_df[ind_vec])
  tmp1.df <- data.frame(tmp.df [-(1:pk_n)])
  corr.matrix1 <- cor(tmp1.df, method = "spearman",  use = "pairwise.complete.obs")
  ### Check correlation
  ### Produces a plot which can be printed from R Studio
  jpeg(paste0(out_dir,corr_plot_name,".jpg"), res = 300, width = 1000, height = 1000, pointsize = 9)
  Chart.Correlation_PM(tmp1.df , method = c(corr_method)
                    ,histogram=FALSE, pch=20, cex=1.5,sym_cex = 0.001)
  dev.off()
  this.indicators.label <- rownames(corr.matrix1)
  #### Plotting network diagram
  qgraph(cor(tmp1.df),
         # shape = "circle",
         # posCol = "darkgreen",
         # negCol = "darkred",
         # threshold = "bonferroni", #The threshold argument can be used to remove edges that are not significant.
         # sampleSize = nrow(scores.this.norm),
         # graph = "glasso",
         esize = 50, ## Size of node
         vsize = 6,
         vTrans = 500,
         posCol = "#003399", ## Color positive correlation Dark powder blue
         negCol = "#FF9933", ## Color negative correlation Deep Saffron
         alpha = 0.05,
         cut = 0.4, ## cut off value for correlation
         maximum = 1, ## cut off value for correlation
         palette = 'pastel', # adjusting colors
         borders = TRUE,
         details = FALSE,
         layout = "spring",
         nodeNames = this.indicators.label ,
         legend.cex = 0.4,
         title = "Correlations Network for Digital Exclusion Indicators",
         line = -2,
         #cex.main = 1.5,
         filetype = "jpg",
         filename = paste0(out_dir,net_plot_name))
}

#### This function generated the composite index from the input variables

# ind_df=DER_raw_norm.df
# ind_vec=int_ind_vec
# pk_n=3
# corr_method=c("pearson")
# dim_factor=2

# ggplot(tmp.df, aes(x=lt10mb_pc)) +
#   geom_histogram()

comp_ind_gen <- function(ind_df,ind_vec,pk_n,corr_method,dim_factor){
  ### Create a dataframe with just the required variables
  tmp.df <- data.frame(ind_df[ind_vec])
  tmp1.df <- data.frame(tmp.df [-(1:pk_n)])
  
  index_1 <- pk_n+1
  index_2 <- as.integer(ncol(tmp.df))
  
  #### Creating Composite Indicator
  tmp3 <- data.matrix(tmp.df, rownames.force = NA)
  CI_Factor_estimated <-  Compind::ci_factor(x=tmp.df,
                                             indic_col = (index_1:index_2),
                                             method = "CH",  # if method = "CH" it can be choose the number of the component to take into account.
                                             dim = dim_factor)
  
  ci_factor_est <- data.frame( CI_Factor_estimated$ci_factor_est)
  names(ci_factor_est) <- "Factor_Score"
  
  ci_factor_est$Factor_Rank <- rank(ci_factor_est$Factor_Score,
                                    ties.method= "min",na.last="keep")
  
  Final_CI.df <- cbind(ind_df,ci_factor_est)
  Final_CI.df
  
}

##### This function generates the required maps

# ind_data=Final_CompI
# geo_dir=geo_dir
# layer="Lincolnshire LSOA"
# poly_name = "LSOA.name"
# rank_var="Factor_Rank"
# label_header ="Factor Rank: "


map_ind_linc <-  function(ind_data,geo_dir,layer,label_name,rank_var,label_header){

  ## Read Lincolnshire data 
  lsoa_data <-  readOGR(dsn = geo_dir,
                        layer = layer)
  ### Project to Lat Long
  lsoa_data_ll <- spTransform(lsoa_data, CRS("+init=epsg:4326"))
  
  ## merge factor data 
  lsoa_data_ll_CI <- sp::merge(lsoa_data_ll,ind_data,
                               by.x='AreaCode',by.y='lsoa_code',all.x=TRUE)

  ## Define Polygon Colours from red to white based on ranking variable
  pal <- colorNumeric(
    palette = colorRampPalette(c('red', 'white'))(length(lsoa_data_ll_CI[[rank_var]])), 
    domain = lsoa_data_ll_CI[[rank_var]])

  

  
  ### labels 
  labs <- lapply(seq(nrow(lsoa_data_ll_CI@data)), function(i) {
      paste0( '<p>', lsoa_data_ll_CI@data[i, label_name], '<p></p>',
              label_header,
              lsoa_data_ll_CI@data[i, rank_var], '</p>' )
    })

 ### Produce the map
 ### NB need to work out how to pass the reference to Factor_Rank to the function 
 
 lsoa_linc_map <- leaflet(options = leafletOptions(minZoom = 9)) %>%
   addMapPane(name = "fac_poly", zIndex = 410) %>%
   addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
   addProviderTiles("CartoDB.PositronNoLabels") %>%
   addProviderTiles("CartoDB.PositronOnlyLabels",
                    options = leafletOptions(pane = "maplabels"),
                    group = "map labels") %>%
   addPolygons(data = lsoa_data_ll_CI, stroke = TRUE,
               fillOpacity = 0.5, smoothFactor = 0.5, weight=1.0,
               fillColor = ~ pal(x=Factor_Rank),
               label = lapply(labs, htmltools::HTML),
               group = "fac_poly",
               options = leafletOptions(pane = "fac_poly")) %>%
   addLegend(data = lsoa_data_ll_CI,
             "topright", pal = pal, values = ~Factor_Rank, title = "Rank")
lsoa_linc_map
  
}  








### Load data 
#### 

### Directory for the raw data files
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Data/'
### Directory for the GIS data
geo_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/gis/'
### Output directory 
out_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Figures/'



### Load raw data - sources of data used in the rankings 

data_raw <- 'Source_data_Lincoln-v3_March_2023.xlsx'
DER_raw.df <- readxl::read_xlsx(paste0(data_dir,data_raw),range = "A3:T423",col_names = TRUE )


### Rename columns 
#########
### Better to use dplyr rename function to avoid any ambiguity with column positions 
#########

DER_raw.df <- dplyr::rename(DER_raw.df, "la_name" = "Local authority name")
DER_raw.df <- dplyr::rename(DER_raw.df, "lsoa_name" = "LSOA name")
DER_raw.df <- dplyr::rename(DER_raw.df, "lsoa_code" = "LSOA Code")
DER_raw.df <- dplyr::rename(DER_raw.df, "exp_group" = "Experian Group")
DER_raw.df <- dplyr::rename(DER_raw.df, "nqr_pc" = "Percentage of residents aged 16+ with no qualifications")
DER_raw.df <- dplyr::rename(DER_raw.df, "gpc_rate" = "Guaranteed pension credit (rate per 1,000 aged 65+)")
DER_raw.df <- dplyr::rename(DER_raw.df, "unp_rate" = "Unemployment rate")
DER_raw.df <- dplyr::rename(DER_raw.df, "de_pc" = "Percentage of population in social grade DE")
DER_raw.df <- dplyr::rename(DER_raw.df, "lac_pc" = "Day-to-day activities limited a lot %")
DER_raw.df <- dplyr::rename(DER_raw.df, "dwnld_speed" = "Average download speed (Mbit/s)")
DER_raw.df <- dplyr::rename(DER_raw.df, "lt10mb_pc" = "Percentage of connections receiving less than 10Mbit/s broadband")
DER_raw.df <- dplyr::rename(DER_raw.df, "gp_pt15_pc" = "GP PT 15 minutes %")
DER_raw.df <- dplyr::rename(DER_raw.df, "gp_pt30_pc" = "GP PT 30 minutes %")
DER_raw.df <- dplyr::rename(DER_raw.df, "gp_car15_pc" = "GP Car 15 minutes %")
DER_raw.df <- dplyr::rename(DER_raw.df, "pharm_car_15_pc" = "Pharmacy Car 15 minutes %")
DER_raw.df <- dplyr::rename(DER_raw.df, "exp_smph" = "Smartphone")
DER_raw.df <- dplyr::rename(DER_raw.df, "exp_ns" = "Not savvy at all")
DER_raw.df <- dplyr::rename(DER_raw.df, "exp_email" = "Email")
DER_raw.df <- dplyr::rename(DER_raw.df, "exp_im" = "Instant messaging")
DER_raw.df <- dplyr::rename(DER_raw.df, "exp_sn" = "Social networking")

ggplot(DER_raw.df, aes(x=dwnld_speed)) +
  geom_histogram()

## STEP 1 Inverting order of variables so that lowest score = biggest problem

DER_raw.df$de_pc <- DER_raw.df$de_pc*-1
DER_raw.df$gpc_rate <- DER_raw.df$gpc_rate*-1
DER_raw.df$lac_pc <- DER_raw.df$lac_pc*-1
DER_raw.df$nqr_pc <- DER_raw.df$nqr_pc*-1
DER_raw.df$unp_rate <- DER_raw.df$unp_rate*-1
DER_raw.df$lt10mb_pc <- DER_raw.df$lt10mb_pc*-1
DER_raw.df$exp_ns <- DER_raw.df$exp_ns*-1

### STEP 2 - Data normalisation 
### Normalise the data so all variables in the same range
### To enable appropriate comparison 

### Applying z score normalisation - mean = 0 sd = 1 
DER_raw_norm.df <- DER_raw.df
DER_raw_norm.df[-(1:4)] <- lapply(DER_raw_norm.df[-(1:4)], BBmisc::normalize)


### Generate Factor Scores for subsets of variables and then combine in one composite

### Transport Variables 
trans_ind_vec <- (c('la_name','lsoa_name','lsoa_code','gp_pt15_pc','gp_pt30_pc','gp_car15_pc','pharm_car_15_pc'))



Final_CI_transport <- comp_ind_gen(ind_df=DER_raw_norm.df
                       ,ind_vec=trans_ind_vec
                       ,pk_n=3
                       ,corr_method=c("pearson")
                       ,dim_factor=3)

### Generate a normalised combined indicator
trans_score <- select(Final_CI_transport, c('lsoa_code','Factor_Score'))
trans_score[-(1:1)] <- lapply(trans_score[-(1:1)], BBmisc::normalize)
trans_score <- dplyr::rename(trans_score, "TRP" = "Factor_Score")

### Internet variables
# int_ind_vec <- (c('la_name','lsoa_name','lsoa_code','dwnld_speed','lt10mb_pc'))
# 
# Final_CI_int <- comp_ind_gen(ind_df=DER_raw_norm.df
#                                    ,ind_vec=int_ind_vec
#                                    ,pk_n=3
#                                    ,corr_method=c("pearson")
#                                    ,dim_factor=2)
# ### Generate a normalised combined indicator
# int_score <- select(Final_CI_int, c('lsoa_code','Factor_Score'))
# int_score[-(1:1)] <- lapply(int_score[-(1:1)], BBmisc::normalize)
# int_score <- dplyr::rename(int_score, "INT" = "Factor_Score")

### Experian variables
exp_ind_vec <- (c('la_name','lsoa_name','lsoa_code','exp_smph','exp_ns','exp_email','exp_im','exp_sn'))

Final_CI_exp <- comp_ind_gen(ind_df=DER_raw_norm.df
                                   ,ind_vec=exp_ind_vec
                                   ,pk_n=3
                                   ,corr_method=c("pearson")
                                   ,dim_factor=2)

### Generate a normalised combined indicator
exp_score <- select(Final_CI_exp, c('lsoa_code','Factor_Score'))
exp_score[-(1:1)] <- lapply(exp_score[-(1:1)], BBmisc::normalize)
exp_score <- dplyr::rename(exp_score, "EXP" = "Factor_Score")

#### Create a dataframe with all indicators for final Composite

main_ind <- select(DER_raw_norm.df, c('lsoa_code','nqr_pc','gpc_rate','unp_rate','de_pc','lac_pc'))

Final_CI_transport <- dplyr::rename(Final_CI_transport, "G15" = "gp_pt15_pc")
Final_CI_transport <- dplyr::rename(Final_CI_transport, "G30" = "gp_pt30_pc")
Final_CI_transport <- dplyr::rename(Final_CI_transport, "C15" = "gp_car15_pc")
Final_CI_transport <- dplyr::rename(Final_CI_transport, "P15" = "pharm_car_15_pc")

main_ind <- dplyr::rename(main_ind, "NQR" = "nqr_pc")
main_ind <- dplyr::rename(main_ind, "GPC" = "gpc_rate")
main_ind <- dplyr::rename(main_ind, "UNP" = "unp_rate")
main_ind <- dplyr::rename(main_ind, "DER" = "de_pc")
main_ind <- dplyr::rename(main_ind, "LAC" = "lac_pc")


#### Alternative to combined internent variable just use download speed

dwnld_ind <- select(DER_raw_norm.df, c('lsoa_code','dwnld_speed'))
dwnld_ind <- dplyr::rename(dwnld_ind, "INT" = "dwnld_speed")

### Combine into one dataframe

#all_ind <- merge(main_ind,int_score,by='lsoa_code')
all_ind <- merge(main_ind,dwnld_ind,by='lsoa_code')
all_ind <- merge(all_ind,exp_score,by='lsoa_code')


#### Finally create a single composite

final_comp_vec <- (c('lsoa_code','NQR','GPC','UNP','DER','LAC','INT','EXP'))

### Correlation Checking and Composite indicator generation


### Transport

trans_new_vec <- (c('lsoa_code','G15','G30','C15','P15'))


corr_gen(ind_df = Final_CI_transport,
         ind_vec = trans_new_vec,
         pk_n =1,
         corr_method = "pearson",
         out_dir = out_dir,
         corr_plot_name = 'final_corr_trans',
         net_plot_name = 'final_network_trans')


corr_gen(ind_df = all_ind,
         ind_vec = final_comp_vec,
         pk_n =1,
         corr_method = "pearson",
         out_dir = out_dir,
         corr_plot_name = 'final_corr_all',
         net_plot_name = 'final_network_all')

Final_CompI <- comp_ind_gen(ind_df=all_ind
                             ,ind_vec=final_comp_vec
                             ,pk_n=1
                             ,corr_method=c("pearson")
                             ,dim_factor=3)

all_CompI <- merge(DER_raw.df,Final_CompI,by='lsoa_code')

### interpretation of factor analysis 
#https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html 
digex.fa <- factanal(all_ind[,c(2:ncol(all_ind))], factors = 3)
digex.fa

digex.fa$uniquenesses

digex.fa$loadings


### Output to a file as Communality and Uniqueness
# Communality 
fname = paste0(out_dir,'tmp.csv')
tmp.df <- data.frame(apply(digex.fa$loadings^2,1,sum)) # communality
names(tmp.df)[1] <- "comm"
tmp.df$comm <- as.numeric(tmp.df$comm)
# Uniqueness - check
# tmp.df$unique <- 1 - apply(digex.fa$uniquenesses^2,1,sum) # uniqueness
# write.csv(tmp.df,fname)

#### Mapping
#### Currently the rank variable has to be 'Factor_Rank"

Main_Map <- map_ind_linc(ind_data=all_CompI,
             geo_dir=geo_dir,
             layer="Lincolnshire LSOA",
             label_name = "lsoa_name",
             rank_var="Factor_Rank",
             label_header ="Factor Rank: ")
  
setView(Main_Map, lng=-0.1999702,lat=53.1178821,zoom = 9.4)
# Save as png
map_file <- 'linc_main_map_1_5.jpg'
mapview::mapshot(Main_Map, file = paste0(out_dir,map_file),zoom=1.5)

Trans_Map <- map_ind_linc(ind_data=Final_CI_transport,
                         geo_dir=geo_dir,
                         layer="Lincolnshire LSOA",
                         label_name = "lsoa_name",
                         rank_var="Factor_Rank",
                         label_header ="Factor Rank: ")

setView(Trans_Map, lng=-0.1999702,lat=53.1178821,zoom = 9.4)
# Save as png
map_file <- 'linc_trans_map.jpg'
mapview::mapshot(Trans_Map, file = paste0(out_dir,map_file,zoom=15.0))
  
  
#### Rank LSOA's into quartiles and identify those with high/high high/low etc. for multiple composites
n_quant=2

main_quant <- select(Final_CompI, c('lsoa_code','Factor_Rank'))
main_quant$main_quant <-  schoRsch::ntiles(main_quant, dv = "Factor_Rank", bins=n_quant)
main_quant <- dplyr::rename(main_quant, "Factor_Rank_main" = "Factor_Rank")

trans_quant <- select(Final_CI_transport, c('lsoa_code','Factor_Rank'))
trans_quant$trans_quant <-  schoRsch::ntiles(trans_quant, dv = "Factor_Rank", bins=n_quant)
trans_quant <- dplyr::rename(trans_quant, "Factor_Rank_trans" = "Factor_Rank")

#### Merge DataFrames

main_trans_quant <- merge(main_quant,trans_quant,by='lsoa_code')

### Add LSOA names 

all_trans_quant <- merge(DER_raw.df,main_trans_quant,by='lsoa_code')

#### Create a Map of the combined data 

main_trans_quant$Overall <- ""
main_trans_quant[main_trans_quant$main_quant == 1 & 
                main_trans_quant$trans_quant == 1, "Overall"] <- "High:High"
main_trans_quant[main_trans_quant$main_quant == 1 & 
                   main_trans_quant$trans_quant == 2, "Overall"] <- "High:Low"
main_trans_quant[main_trans_quant$main_quant == 2 & 
                   main_trans_quant$trans_quant == 1, "Overall"] <- "Low:High"
main_trans_quant[main_trans_quant$main_quant == 2 & 
                   main_trans_quant$trans_quant == 2, "Overall"] <- "Low:Low"


## Read Lincolnshire data 
lsoa_data <-  readOGR(dsn = geo_dir,
                      layer = "Lincolnshire LSOA")
### Project to Lat Long
lsoa_data_ll <- spTransform(lsoa_data, CRS("+init=epsg:4326"))

## merge factor data 
lsoa_data_ll_CI <- sp::merge(lsoa_data_ll,main_trans_quant,
                             by.x='AreaCode',by.y='lsoa_code',all.x=TRUE)

## Create a colour palette
pal <- colorFactor(
  palette = 'Set1',
  domain = lsoa_data_ll_CI$Overall
)

### labels 
labs <- lapply(seq(nrow(lsoa_data_ll_CI@data)), function(i) {
  paste0( '<p>', lsoa_data_ll_CI@data[i, "lsoa_name"], '<p></p>',
          "Factor Rank: ",
          lsoa_data_ll_CI@data[i, "Overall"], '</p>' )
})

### Produce the map
### NB need to work out how to pass the reference to Factor_Rank to the function 

lsoa_digex_trans_map <- leaflet(options = leafletOptions(minZoom = 9)) %>%
  addMapPane(name = "fac_poly", zIndex = 410) %>%
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addPolygons(data = lsoa_data_ll_CI, stroke = TRUE,
              fillOpacity = 0.5, smoothFactor = 0.5, weight=1.0,
              fillColor = ~ pal(x=Overall),
              label = lapply(labs, htmltools::HTML),
              group = "fac_poly",
              options = leafletOptions(pane = "fac_poly")) %>%
  addLegend(data = lsoa_data_ll_CI,
            "topright", pal = pal, values = ~Overall, title = "Digital Exclusion:Transport Barrier")
setView(lsoa_digex_trans_map, lng=-0.1999702,lat=53.1178821,zoom = 9.4)

## Save as png
map_file <- 'linc_main_trans_map.jpg'
mapview::mapshot(lsoa_digex_trans_map, file = paste0(out_dir,map_file))