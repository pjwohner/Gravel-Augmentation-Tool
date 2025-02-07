## Rogue Gravel Transport Tool Beta Version 2.0
## Created by Patricia Wohner with input from various stakeholders Oct 1 2024
## Updated Dec 18 2024 to version 2
## Using filtered Terrainworks data and 
## Connor Intrinsic Potential Habitat Classifications IP model (Connor et al. 2020)
## Salmon IP model based on #1 ranked Rogue and uses stream gradient, stream width, and mean basin elevation
## Oregon Land Ownership comes from Oregon GeoHub data updated July 2023
## https://geohub.oregon.gov/datasets/dfdbbd2e11a645d097f00f523c147204_0/explore?location=43.371964%2C-120.595509%2C10.25
## NorWeST database provides mean August stream temperatures
## description https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_NorWeST_StreamTemperatures_01/MapServer
##  https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=norwest_temperature
## See GRAVELtransportMakeFilesJan2_2025.R for more details on data file creation
######################################################################
  ##Follow the below steps for first time users. 
  ##Step 1a. Make sure to first install the necessary packages 

  ##Step 1b. run the below lines to call them 
  library(sf)
  library(terra)
  library(raster)
  library(sp)
  library(tidyverse)
  #########################################################################
  ##Step 2. set your working directory to where you saved the following files
  ## or open Rproj file in the folder
  #setwd("C:/newlaptop/FISH/GravelTransport/SpatialTool")
   
  #Step 3. Call the datafiles
  Roguecounties <- readRDS("Roguecounties.rds")
  Roguestreams <- readRDS("Roguestreams.rds")
  waterbodiesRogue <- readRDS("waterbodiesRogue.rds")
  mycities <- readRDS("mycities.rds")
  ROgueRds <- readRDS("ROgueRds2.rds")
  TruckRds2 <-readRDS("TruckRds2.rds")
  GravSources <- readRDS("GravSources.rds")
  Chinookspsurv.dat <- readRDS("ChinookspReaches1.rds")
  STsurv.dat <- readRDS("STsurv.dat4.rds")
  FinalReaches2 <- readRDS("FinalReaches2.rds")###LOAD the terrainworks netmap reach data

  ###############################################################################
  ##Step 4. Run the gravel transport function so R recognizes it and can use it
  ##p ut curser on name in next line and either run or press cntrl and enter at the same time
  ## or highlight entire function from here to line 108 and run
  Gravel_Transport_Tool <- function(CSIPcategory1,CSIPcategory2=NULL,CSIPcategory3=NULL,
                                    Rho1,Rho2=NULL,Rho3=NULL,
                                    Temp,
                                    ClimateChange,
                                    Land,
                                    IncludeBar,
                                    DistanceKm,
                                    DistAccessRd,
                                    HighSteel,
                                    ShowTruckRtes,
                                    zoom=NULL) {
    STreddbuff <- st_buffer(STsurv.dat, dist = 1000)#buffer Steelhead redds
    CSreddbuff<- st_buffer(Chinookspsurv.dat, dist = 1000)#buffer chinook redds
    
    CSReaches <-FinalReaches2%>%#filter out user choices
      filter(CS.IPrank %in% c(CSIPcategory1, CSIPcategory2, CSIPcategory3))%>%
      filter(Rhorank%in%c(Rho1,Rho2,Rho3))%>%
      mutate(ModelTemp = case_when(
        ClimateChange == "Yes" ~ S30_2040D,
        ClimateChange == "No" ~ S1_93_11))%>%
      filter(ModelTemp<=Temp)%>%
     # filter(S1_93_11<=Temp)%>%
       mutate(Closest1 = case_when(
        IncludeBar == "Yes" ~ CL.DISTkm,
        IncludeBar == "No" ~ CL.DISTkmpits))%>%
        filter(Closest1<=DistanceKm)%>%
        filter(nearest_dist <= DistAccessRd)%>%
      filter(case_when(
        Land == "Public" ~ Public == 1,
        Land == "Private" ~ Public == 0,
        Land == "Both" ~ Public %in% c(0, 1)
      ))
    
    STandCSReaches <-CSReaches%>%#create dataframe for when user interested in steelhead
      filter(ST.IPrank =="High")
    
    ##decide which Reaches dataframe to use based on user interest in Steelhead
      if (HighSteel == 1) { Reaches <- STandCSReaches
        } else { Reaches <- CSReaches }
    
    GravSources <- GravSources %>%#include gravel sources user specified
      filter(case_when(IncludeBar == "No" ~ Description != 0, TRUE ~ TRUE))
    
    ##decide if to plot truckroutes or not
    if (ShowTruckRtes == "Yes") {
      TruckRoutes <- geom_sf(data = TruckRds2, aes(color = "TruckRoads"), fill = "white", linewidth = 0.75)
    } else {
      TruckRoutes <- NULL
       }
    
    labels <- Roguestreams %>%##create labels to include in map to orient the user
        st_transform(st_crs(Reaches)) %>% ##Below need to be in alphabetical order
        filter(Name %in% c("Applegate River","Big Butte Creek","Elk Creek","Evans Creek",
                           "Rogue River", "South Fork Big Butte Creek"	,"South Fork Rogue River",
                           "Trail Creek","West Fork Evans Creek","West Fork Trail Creek")) %>%#"Skeeters Creek","North Fork Little Butte Creek", "Flat Creek"
            group_by(Name) %>%  # Group by stream name
            slice_head(n = 1) %>% ungroup() # Take first occurrence of each name if multiple
    ##Make the plot
    ReachesPlot <- ggplot() +
        geom_sf(data = Roguecounties, aes(color = "Counties"), fill = "white") + 
        geom_sf(data = ROgueRds, aes(color = "Roads"), fill = "white") +
        geom_sf(data = mycities, color = "darkgrey",aes(color = "Cities")) +
        geom_sf(data = Roguestreams, aes(color = "Streams"), fill = "white") +
        geom_sf(data = labels, aes(color = "Major rivers"), linewidth=0.75) +
        geom_sf_label(data = labels, label.padding = unit(0, "lines"),label.r = unit(0, "lines"), 
                    fill = NA, label.size = 0 ,color = "steelblue",  aes(label = Name),
                    nudge_x = c(-1500, -14500, -14500, -10500, 54000, -14500, 14500, -500,-13500,-500),
                    nudge_y = c(2050, -2050, -3050, 2050, 8050, -2050, 2050, 2050,2050,12550),
                    size = 3, fontface = "bold") +
        geom_sf_label(data = mycities, label.padding = unit(0, "lines"), label.r = unit(0, "lines"),
                    fill = NA, label.size = 0 ,color = "grey30", aes(label = Place_Name),
                    size = 3,fontface = "bold") +
        TruckRoutes +
        geom_sf(data = STreddbuff, aes(color = "Steelhead redds"),shape = 1, size = 0.5,fill = NA) +
        geom_sf(data = CSreddbuff,  aes(color = "Chinook redds"), shape = 1, fill = NA,  size = 0.5) + 
        geom_sf(data = GravSources, aes(color = "Gravel sources"), shape = 8, size = 3.5) +
        geom_sf(data = Reaches, aes(color = "Priority reaches"),linewidth=0.75,fill = "white") +
        geom_sf(data = waterbodiesRogue, aes(color = "Reservoir"), fill = "darkblue") +
        coord_sf(xlim = c(695000-zoom, 730000+zoom), ylim = c(240000-zoom, 280000+zoom)) +
        scale_color_manual(name = "Legend",
                         values = c(
                           "Roads" = "grey",
                           "Streams" = "lightblue",
                           "Reservoir" = "darkblue",
                           "Major rivers" = "dodgerblue",
                           "Priority reaches" = "red",
                           "Chinook redds" = "purple",
                           "Steelhead redds" = "black",
                           "Cities" = "black",
                           "Gravel sources" = "black")) +
        theme(legend.title = element_text(family = "Arial", size = 10, color = "black"),
            legend.background = element_rect(fill = "lightgray", color = "black"),
            legend.key = element_blank(),  # removes the box behind the legend key
            legend.margin = margin(10, 20, 30, 40, "mm")) +
        theme_bw()
    print(ReachesPlot)##prints out map to screen
    return(Reaches)##saves a dataframe of top reaches
  }
  
  ##Step 5. Specify arguments(inputs)  in the Tool
  # Inputs:
  # 1) Intrinsic Potential Category of Interest (higher is higher potential)
    # Can add up 3 IP categories of 5 = "VeryHigh", "High", "Moderate","Low","VeryLow"
    # Need to retain quotations for model to run!!!
  # 2) HighSteel = whether to only include reaches where salmon and High Steelhead occur, 0 or 1
  # 3) Rho1 (gravel transport potential) reach categories (lower means less gravel moved over time)
    # Can add up to 3 choices of 5 possible Rho categories = "VeryLow", "Low", "Medium", "High","VeryHigh"
    ## use Null for unused 2nd or 3rd categories
  # 4) Temp: Maximum stream temperature threshold of reach (any value)
  # 5) ClimateChange: Yes if want to use predicted temps for 2040s, no to use current predictions from NorWest
  # 6) Land: Land classification of reach: specify Public, or Private or Both
  # 7) IncludeBar: Gravel bar or not = IncludeBar = "Yes" if including Gravel bars and other not currently permitted
  # or "No" if not including gravel bars
  # 8) DistAccessRd = minimum distance from the reach to an access road in m
  # 9) DistanceKm: maximum distance of reaches to gravel pit in Km you want to consider =  any distance
  # 10) ShowTruckRtes: Yes if you want to see on the plot potential truck routes from pits to reaches
      #No if you don't want to see this
  # 11) zoom = how close to zoom in on output map: zoom = 40000 is drainage wide
    ##zoom = 4000 is smaller wide (max = 115000)
###########################################################################
    #Example Scenarios below used in MS
  ####Scenario 1: Climate Change Scenario 22 reaches
  Find_Reaches <- Gravel_Transport_Tool(
    CSIPcategory1="VeryHigh",CSIPcategory2="High",CSIPcategory3=NULL,
    HighSteel=0,
    Rho1="VeryLow",Rho2="Low",Rho3=NULL,
    Temp=18,
    ClimateChange="No",
    Land="Public",#"Both",#"Private", #
    IncludeBar="No", 
    DistAccessRd = 100,
    DistanceKm=25,
    ShowTruckRtes="No",
    zoom=115000)
  
#####Scenario 2 Steelhead 14 reaches
  Find_Reaches2 <- Gravel_Transport_Tool(
    CSIPcategory1="VeryHigh",CSIPcategory2="High",CSIPcategory3="Moderate",
    HighSteel=1,
    Rho1="VeryLow",Rho2="Low",Rho3=NULL,
    Temp=18,
    ClimateChange="No",
    Land="Public",#"Both",#"Private", #
    IncludeBar="No", 
    DistAccessRd = 100,
    DistanceKm=25,
    ShowTruckRtes="No",
    zoom=115000)

####Scenario 3 Climate Change and permit chaneg 100 reaches
  Find_Reaches3 <- Gravel_Transport_Tool(
    CSIPcategory1="VeryHigh",CSIPcategory2="High",CSIPcategory3=NULL,
    HighSteel=0,
    Rho1="VeryLow",Rho2="Low",Rho3=NULL,
    Temp=18,
    ClimateChange="Yes",
    Land="Public",#"Both",#"Private", #
    IncludeBar="Yes", 
    DistAccessRd = 100,
    DistanceKm=25,
    ShowTruckRtes="No",
    zoom=115000)

  