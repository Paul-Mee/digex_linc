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
                   "forcats","htmltools","Matrix")


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

chart.Correlation <- function (R, histogram = TRUE, method_corr ,cex,sym_cex, ...) 
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

# ind_vec <- (c('la_name','lsoa_name','lsoa_code','gp_pt15','gp_pt30','gp_car15','pharm_car_15'))
# ind_df <- DER_raw_norm.df
# pk_n <- 3
# dim_factor <- 3

corr_gen <- function(ind_df,ind_vec,pk_n,corr_method,out_dir,plot_name){
  ### Create a dataframe with just the required variables
  tmp.df <- data.frame(ind_df[ind_vec])
  tmp1.df <- data.frame(tmp.df [-(1:pk_n)])
  ### Check correlation
  ### Produces a plot which can be printed from R Studio
  chart.Correlation(tmp1.df , method = c(corr_method)
                    ,histogram=FALSE, pch=20, cex=2.5,sym_cex = 0.001)
  corr.matrix1 <- cor(tmp1.df, method = corr_method,  use = "pairwise.complete.obs")
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
         cex.main = 1.5,
         filetype = "png",
         filename = paste0(out_dir,plot_name))
}


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



### Load data 
#### 

### Directory for the raw data files
data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Data/'
### Directory for the GIS data
geo_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/gis/'
### Output directory 
out_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Figures/'

### Original equal weight rankings 
data_file <- 'Digital Exclusion Rankings.csv'
## Ranked data 
### Loading orginal rankings and overall equal weight rankings 
### Rankings from 1 most deprived to n least deprived
DER.df <- read.csv2(file=paste0(data_dir,data_file),header = TRUE,sep = ",")

### Rename columns for consistency with other dataframes

names(DER.df)[6] <- "NQU"
names(DER.df)[7] <- "GPC"
names(DER.df)[8] <- "UNP"
names(DER.df)[9] <- "INT"
names(DER.df)[10] <- "DER"
names(DER.df)[11] <- "LAC"
names(DER.df)[12] <- "TRP"
names(DER.df)[13] <- "EXP"


### Load raw data - sources of data used in the rankings 

data_raw <- 'Source_data_Lincoln-v3_March_2023.xlsx'
DER_linc.df <- readxl::read_xlsx(paste0(data_dir,data_raw),range = "A3:T423",col_names = TRUE )

DER_raw.df <- DER_linc.df 

### Rename columns 
#########
### Better to use dplyr rename function to avoid any ambiguity with column positions 
#########

names(DER_raw.df)[1] <- "la_name"
names(DER_raw.df)[2] <- "lsoa_name"
names(DER_raw.df)[3] <- "lsoa_code"
names(DER_raw.df)[4] <- "exp_group"
names(DER_raw.df)[5] <- "nqr_pc"
names(DER_raw.df)[6] <- "gpc_rate"
names(DER_raw.df)[7] <- "unp_rate"
names(DER_raw.df)[8] <- "de_pc"
names(DER_raw.df)[9] <- "lac_pc"
names(DER_raw.df)[10] <- "dwnld_speed"
names(DER_raw.df)[11] <- "lt10mb_pc"
names(DER_raw.df)[12] <- "gp_pt15"
names(DER_raw.df)[13] <- "gp_pt30"
names(DER_raw.df)[14] <- "gp_car15"
names(DER_raw.df)[15] <- "pharm_car_15"
names(DER_raw.df)[16] <- "exp_smph"
names(DER_raw.df)[17] <- "exp_ns"
names(DER_raw.df)[18] <- "exp_email"
names(DER_raw.df)[19] <- "exp_im"
names(DER_raw.df)[20] <- "exp_sn"

### STEP 1 - Data normalisation 
### Normalise the data so all variables in the same range
### To enable appropriate comparison 

### Applying z score normalisation - mean = 0 sd = 1 
DER_raw_norm.df <- DER_raw.df
DER_raw_norm.df[-(1:4)] <- lapply(DER_raw_norm.df[-(1:4)], BBmisc::normalize)

### Raw data 
p1 <- ggplot(DER_raw.df, aes(x=unp_rate)) + 
  geom_histogram()
p1
### Normalised data
p2 <- ggplot(DER_raw_norm.df, aes(x=unp_rate)) + 
  geom_histogram()
p2


# ### Checking effect
# 
# DER_check.df <- merge(DER.df,DER_raw_norm.df,by.x="LSOA.Code",by.y="lsoa_code")
# 
# ggplot(DER_check.df, aes(x=gpc_rate, y=GPC)) + 
#   geom_point()


## Inverting order of variables so that lowest score = biggest problem

DER_raw_norm.df$de_pc <- DER_raw_norm.df$de_pc*-1
DER_raw_norm.df$gpc_rate <- DER_raw_norm.df$gpc_rate*-1
DER_raw_norm.df$lac_pc <- DER_raw_norm.df$lac_pc*-1
DER_raw_norm.df$nqr_pc <- DER_raw_norm.df$nqr_pc*-1
DER_raw_norm.df$unp_rate <- DER_raw_norm.df$unp_rate*-1
DER_raw_norm.df$lt10mb_pc <- DER_raw_norm.df$lt10mb_pc*-1


## Experian
## Indicator equals proportion using each technology or for ns (proportion not savvy).
## Therefore for ns change sign 
DER_raw_norm.df$exp_ns <- DER_raw_norm.df$exp_ns*-1

################
### This Version pass sets of variables to a function which calculates combined index
### and generates the other figures
################

trans_ind_vec <- (c('la_name','lsoa_name','lsoa_code','gp_pt15','gp_pt30','gp_car15','pharm_car_15'))

Final_CI_transport <- comp_ind_gen(ind_df=DER_raw_norm.df
                       ,ind_vec=trans_ind_vec
                       ,pk_n=3
                       ,corr_method=c("spearman")
                       ,dim_factor=3)

other_ind_vec <- (c('la_name','lsoa_name','lsoa_code','nqr_pc','gpc_rate','unp_rate','de_pc','lac_pc',
                    'dwnld_speed','lt10mb_pc','exp_smph','exp_ns','exp_email','exp_im','exp_sn'))

Final_CI_main <- comp_ind_gen(ind_df=DER_raw_norm.df
                                   ,ind_vec=other_ind_vec
                                   ,pk_n=3
                                   ,corr_method=c("pearson")
                                   ,dim_factor=3)

### Output for online tool

DER_fact_summary.df <- DER_norm_CI.df[,c('la_name','lsoa_name','lsoa_code','Factor_Rank')]

write.csv(DER_fact_summary.df,paste0(out_dir,'factor_rank.csv'))


### interpretation of factor analysis 
#https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html 
digex.fa <- factanal(DER_norm_tmp.df[,c(4:ncol(DER_norm_tmp.df))], factors = 3)
digex.fa

digex.fa$uniquenesses

digex.fa$loadings

# Communality 
fname = paste0(out_dir,'tmp.csv')


tmp.df <- data.frame(apply(digex.fa$loadings^2,1,sum)) # communality

names(tmp.df)[1] <- "comm"

tmp.df$comm <- as.numeric(tmp.df$comm)


# Uniqueness
tmp.df$unique <- 1 - apply(digex.fa$loadings^2,1,sum) # uniqueness

write.csv(tmp.df,fname)

#### Mapping


### Display on a map
### Only needed if mapping in R 


### Read shapefile data 
# lsoa_uk <-  readOGR(dsn = geo_dir,
#                  layer = "LSOA_2021_EW_BSC")

## Lincolnshire data 
lsoa_linc <-  readOGR(dsn = geo_dir,
                 layer = "Lincolnshire LSOA")

### Project to Lat Long
lsoa_linc_ll <- spTransform(lsoa_linc, CRS("+init=epsg:4326"))

## merge factor data 

lsoa_linc_ll_CI <- sp::merge(lsoa_linc_ll,Final_CI_transport,
                         by.x='AreaCode',by.y='lsoa_code',all.x=TRUE)

## Polygon Colours

pal <- colorNumeric(
  palette = colorRampPalette(c('red', 'white'))(length(lsoa_linc_ll_CI$Factor_Rank)), 
  domain = lsoa_linc_ll_CI$Factor_Rank)


### Maps



tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
  }
"))




### Display Factor analysis in map
labs <- lapply(seq(nrow(lsoa_linc_ll_CI@data)), function(i) {
  paste0( '<p>', lsoa_linc_ll_CI@data[i, "LSOA.name"], '<p></p>',
          "Factor Rank: ",
          lsoa_linc_ll_CI@data[i, "Factor_Rank"], '</p>' )
})

title <- tags$div(
  tag.map.title, HTML("Lincolnshire LSOA Digital Exclusion Ranks   Factors")
)  

lsoa_linc_map <- leaflet(options = leafletOptions(minZoom = 9)) %>%
  # Choose here whether to use Open Street Map or Satellite imagery data in the background
  # addProviderTiles("Esri.WorldImagery") %>%
  addMapPane(name = "fac_poly", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addPolygons(data = lsoa_linc_ll_CI, stroke = TRUE, 
              fillOpacity = 0.5, smoothFactor = 0.5, weight=1.0,
              fillColor = ~ pal(Factor_Rank),
              label = lapply(labs, htmltools::HTML),
              group = "fac_poly",
              options = leafletOptions(pane = "fac_poly")) %>%
  #addProviderTiles("OpenStreetMap.Mapnik") %>%
 
  addLegend(data = lsoa_linc_ll_CI, 
            "topright", pal = pal, values = ~Factor_Rank, title = "Rank") 
  #addControl(title, position = "topleft", className="map-title") 
  # addLayersControl(baseGroups = "CartoDB.PositronNoLabels",
  #                  overlayGroups = c("map labels",
  #                                    "fac_poly"))

setView(lsoa_linc_map, lng=-0.1999702,lat=53.1178821,zoom = 9.4)

## Save as png
# map_file <- 'linc_factor_map.png'
# mapview::mapshot(lsoa_linc_map, file = paste0(out_dir,map_file))




