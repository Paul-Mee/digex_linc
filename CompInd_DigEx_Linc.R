############
### Composite Index formation 
### Digital Exclusion Index - Following Severity Index vignette
### https://humanitarian-user-group.github.io/post/compositeindicator/
############

# Clear any existing data from the data set
rm(list = ls())

### Load packages and libraries

# pacman only needs to be installed once
# install.packages("pacman")

# Define vector of package names

package_names <- c("readr","readxl","dplyr","ggplot2","corrplot","psych",
                   "scales","ggiraph","ggrepel","Compind","ggcorrplot","kableExtra",
                   "reshape2","qgraph", "sf","cartography",
                   "raster", "dismo", "rgdal", "SpatialPosition","ggpubr",
                   "devtools","PerformanceAnalytics","hrbrthemes",
                   "Hmisc","PerformanceAnalytics","psych","Rcsdp","GPArotation",
                   "BBmisc","rgdal", "leaflet","sp","plotly","htmlwidgets","mapview",
                   "forcats","htmltools")



# This code installs all the other required packages if they are not currently installed and load all the libraries
pacman::p_load(char=package_names)
rm(package_names)






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
# unhcr_style <- function() {
#   font <- "Lato"
#   ggplot2::theme(
#     
#     #This sets the font, size, type and colour of text for the chart's title
#     plot.title = ggplot2::element_text(family = font, size = 20,
#                                        face = "bold", color = "#222222"),
#     
#     # This sets the font, size, type and colour of text for the chart's subtitle,
#     # as well as setting a margin between the title and the subtitle
#     plot.subtitle = ggplot2::element_text(family = font, size = 16, 
#                                           margin = ggplot2::margin(9,0,9,0)),
#     plot.caption = ggplot2::element_blank(),
#     
#     # This sets the position and alignment of the legend, removes a title 
#     # and backround for it and sets the requirements for any text within the legend.
#     # The legend may often need some more manual tweaking when it comes to its exact
#     # position based on the plot coordinates.
#     legend.position = "top",
#     legend.text.align = 0,
#     legend.background = ggplot2::element_blank(),
#     legend.title = ggplot2::element_blank(),
#     legend.key = ggplot2::element_blank(),
#     legend.text = ggplot2::element_text(family = font, size = 13, color = "#222222"),
#     
#     # This sets the text font, size and colour for the axis test, as well as setting
#     # the margins and removes lines and ticks. In some cases, axis lines and axis 
#     # ticks are things we would want to have in the chart
#     axis.title = ggplot2::element_blank(),
#     axis.text = ggplot2::element_text(family = font, size = 13, color = "#222222"),
#     axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
#     axis.ticks = ggplot2::element_blank(),
#     axis.line = ggplot2::element_blank(),
#     
#     # This removes all minor gridlines and adds major y gridlines. 
#     # In many cases you will want to change this to remove y gridlines and add x gridlines. 
#     panel.grid.minor = ggplot2::element_blank(),
#     panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
#     panel.grid.major.x = ggplot2::element_blank(),
#     
#     # This sets the panel background as blank, removing the standard grey
#     # ggplot background colour from the plot
#     panel.background = ggplot2::element_blank(),
#     
#     # This sets the panel background for facet-wrapped plots to white, 
#     # removing the standard grey ggplot background colour and sets the title size 
#     # of the facet-wrap title to font size 22
#     strip.background = ggplot2::element_rect(fill = "white"),
#     strip.text = ggplot2::element_text(size  = 13,  hjust = 0)
#   )
# }

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



### Load data 



data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Data/'
geo_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/gis/'
data_file <- 'Digital Exclusion Rankings.csv'
out_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Figures/'
ref_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/refs/'

## Ranked data 
DER.df <- read.csv2(file=paste0(data_dir,data_file),header = TRUE,sep = ",")

### Rename columns 

names(DER.df)[6] <- "NQU"
names(DER.df)[7] <- "GPC"
names(DER.df)[8] <- "UNP"
names(DER.df)[9] <- "INT"
names(DER.df)[10] <- "DER"
names(DER.df)[11] <- "LAC"
names(DER.df)[12] <- "TRP"
names(DER.df)[13] <- "EXP"

DER_cor.df <- DER.df[, c(6:13)]

### Load raw data 

data_raw <- 'Source_data_Lincoln.xlsx'
DER_raw.df <- readxl::read_xlsx(paste0(data_dir,data_raw),range = "A3:T423",col_names = TRUE )

### Rename columns 

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
names(DER_raw.df)[15] <- "gp_pharm15"
names(DER_raw.df)[16] <- "exp_smph"
names(DER_raw.df)[17] <- "exp_ns"
names(DER_raw.df)[18] <- "exp_email"
names(DER_raw.df)[19] <- "exp_im"
names(DER_raw.df)[20] <- "exp_sn"






### Data normalisation 
### Applying z score normalisation - mean = 0 sd = 1 
DER_raw_norm.df <- DER_raw.df
DER_raw_norm.df[-(1:4)] <- lapply(DER_raw_norm.df[-(1:4)], BBmisc::normalize)


### Checking effect

DER_check.df <- merge(DER.df,DER_raw_norm.df,by.x="LSOA.Code",by.y="lsoa_code")

ggplot(DER_check.df, aes(x=gpc_rate, y=GPC)) + 
  geom_point()


## Inverting order of variables so that lowest score = biggest problem
DER_raw_norm.df$de_pc <- DER_raw_norm.df$de_pc*-1
DER_raw_norm.df$gpc_rate <- DER_raw_norm.df$gpc_rate*-1
DER_raw_norm.df$lac_pc <- DER_raw_norm.df$lac_pc*-1
DER_raw_norm.df$nqr_pc <- DER_raw_norm.df$nqr_pc*-1
DER_raw_norm.df$unp_rate <- DER_raw_norm.df$unp_rate*-1




## Recalculate combined indices based on normalised scores
## Add normalised values and take average
## Internet 
## two internet variables scale in different directions
DER_raw_norm.df$int_comb <- (DER_raw_norm.df$dwnld_speed + -1*DER_raw_norm.df$lt10mb_pc)/2
## GP travel 
DER_raw_norm.df$tran_comb <- (DER_raw_norm.df$gp_pt15 + DER_raw_norm.df$gp_pt30 
                                                      + DER_raw_norm.df$gp_car15 + DER_raw_norm.df$gp_pharm15)/4

## Experian
## Indicator equals proportion using each technology or for ns (proportion not savvy).
## Therefore for ns change sign 
DER_raw_norm.df$exp_comb <- (DER_raw_norm.df$exp_smph + -1*DER_raw_norm.df$exp_ns
                             + DER_raw_norm.df$exp_email + DER_raw_norm.df$exp_im
                             + DER_raw_norm.df$exp_sn) /5




## Normalize combined variables 
DER_raw_norm.df[-(1:20)] <- lapply(DER_raw_norm.df[c('int_comb','tran_comb','exp_comb')], BBmisc::normalize)

## Create a matrix of normalised variables and rename for consistency



DER_norm_cor.df <- DER_raw_norm.df[ ,c('nqr_pc','gpc_rate','unp_rate','de_pc',
                                       'lac_pc','int_comb','tran_comb','exp_comb')] 


DER_norm_cor.df <- DER_norm_cor.df %>% dplyr::rename(NQR = nqr_pc,
                                                     GPC = gpc_rate,
                                                     UNP = unp_rate,
                                                     DER = de_pc,
                                                     LAC = lac_pc,
                                                     INT = int_comb,
                                                     TRP = tran_comb,
                                                     EXP = exp_comb)

### Correlation analysis for normalised data 

### Check correlation
corr.matrix1 <- cor(DER_norm_cor.df, method = "spearman",  use = "pairwise.complete.obs")

## Correlation with p values 

 chart.Correlation(DER_norm_cor.df , method = c("spearman")
                  ,histogram=FALSE, pch=20, cex=2.5,sym_cex = 0.001)

### Call to chart.Correlation does not produce a print object 
### Print interactively using R studio  

 
## Another approach to better visualize correlation between indicators is to 
## represent them through a network with the ggpraph package.

this.indicators.label <- rownames(corr.matrix1)

### for output
plot_name <- 'correlation_network' 
qgraph(cor(DER_norm_cor.df),
                # shape = "circle",
                # posCol = "darkgreen",
                # negCol = "darkred",
                # threshold = "bonferroni", #The threshold argument can be used to remove edges that are not significant.
                # sampleSize = nrow(scores.this.norm),
                # graph = "glasso",
                esize = 35, ## Size of node
                vsize = 6,
                vTrans = 600,
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
                cex.main = 2,
                filetype = "png",
                filename = paste0(out_dir,plot_name))


# Cronbach’s alpha, (or coefficient alpha), developed by Lee Cronbach in 1951, 
# measures reliability (i.e. how well a test measures what it should: 
# measure of the stability of test scores), or internal consistency.
# 
# As a rule of thumbs, a score of more than 0.7 indicates an acceptable level of consistency:
#   
# A high level for alpha may mean that all indicators are highly correlated 
# (meaning we have redundant indicators representing the same thing…).
# A low value for alpha may mean that there are not enough indicators or that the 
# indicators are poorly interrelated.

Cronbach.this <- psych::alpha(DER_norm_cor.df, check.keys = TRUE)

cat(paste0("The Cronbach Alpha measure of consistency for this combination of indicators is  "
           , round(Cronbach.this$total$std.alpha, 2), "\n." ) )


### Factor analysis with normalised vlaues 
##  Doing PCA with ci_factor.R
# If method = "ONE" (default) the composite indicator estimated values are equal to first component scores;
# if method = "ALL" the composite indicator estimated values are equal to component score multiplied by its proportion variance;
# if method = "CH" it can be choose the number of the component to take into account.


DER_norm_tmp.df <- cbind(DER_raw_norm.df[5:9],DER_raw_norm.df[21:23])
dimfactor <- ifelse(ncol(DER_norm_tmp.df) > 2, 3, ncol(DER_norm_tmp.df))

DER_norm_tmp.df <- DER_norm_tmp.df %>% dplyr::rename(NQR = nqr_pc,
                                                     GPC = gpc_rate,
                                                     UNP = unp_rate,
                                                     DER = de_pc,
                                                     LAC = lac_pc,
                                                     INT = int_comb,
                                                     TRP = tran_comb,
                                                     EXP = exp_comb)


### Factor Method

CI_Factor_estimated <-  Compind::ci_factor(DER_norm_tmp.df,
                                           indic_col = (1:ncol(DER_norm_tmp.df)),
                                           method = "CH",  # if method = "CH" it can be choose the number of the component to take into account.
                                           dim = dimfactor)


ci_factor_est <- data.frame( CI_Factor_estimated$ci_factor_est)
names(ci_factor_est) <- "Factor_Score"

ci_factor_est$Factor_Rank <- rank(ci_factor_est$Factor_Score,
                                       ties.method= "min",na.last="keep")

### interpretation of factor analysis 
#https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html 
digex.fa <- factanal(DER_norm_tmp.df, factors = 3)
digex.fa

digex.fa$uniquenesses

digex.fa$loadings

# Communality 
fname = paste0(out_dir,'tmp.csv')
tmp.df <- data.frame(apply(digex.fa$loadings^2,1,sum)) # communality

names(tmp.df)[1] <- "comm"

tmp.df$comm <- as.numeric(tmp.df$comm)
write.csv(tmp.df,fname)

# Uniqueness
1 - apply(digex.fa$loadings^2,1,sum) # uniqueness




### Add CI scores to orginal dataframe
DER_norm_CI.df <- cbind(DER_raw_norm.df
                            ,ci_factor_est)

### Output for online tool

DER_fact_summary.df <- DER_norm_CI.df[,c('la_name','lsoa_name','lsoa_code','Factor_Rank')]

write.csv2(DER_fact_summary.df,paste0(out_dir,'factor_rank.csv'))

## Comparing with orginal overall rank and factor score based on ranks
## Equal score share first rank i.e. joint 1st 

comb_factor.df  <- merge(DER.df,DER_norm_CI.df,
                         by.x="LSOA.Code",by.y="lsoa_code")

### Display on a map


### Read shapefile data 
# lsoa_uk <-  readOGR(dsn = geo_dir,
#                  layer = "LSOA_2021_EW_BSC")

## Lincolnshire data 
lsoa_linc <-  readOGR(dsn = geo_dir,
                 layer = "Lincolnshire LSOA")

### Project to Lat Long
lsoa_linc_ll <- spTransform(lsoa_linc, CRS("+init=epsg:4326"))

## merge factor data 

lsoa_linc_ll_CI <- sp::merge(lsoa_linc_ll,comb_factor.df,
                         by.x='AreaCode',by.y='LSOA.Code',all.x=TRUE)

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


### citation checking 

# bibfile = paste0(ref_dir,"leaf.bib")
# 
# cat(toBibtex(citation(package = "leaflet")),file= bibfile)

