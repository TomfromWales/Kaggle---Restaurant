# Load relevant data
############################
  air_store_id_list <- readRDS(paste0(project_wd,"/data/air_store_id_list.rds"))
  air_store_info <- readRDS(paste0(project_wd,"/data/air_store_info.rds"))
  hpg_store_info <- readRDS(paste0(project_wd,"/data/hpg_store_info.rds"))

# Establish the extent of local competition
############################  
  # Get a list of all restaurants from the two datasources
  #=========================
    hpg_store_info2 <- hpg_store_info %>%
      dplyr::left_join(store_id_relation,by = "hpg_store_id")
    hpg_rests_in_air <- hpg_store_info2 %>%
      dplyr::filter(!is.na(air_store_id))%>%
      select(-latitude,-longitude)
    hpg_rests_NOT_in_air <- hpg_store_info2 %>%
      dplyr::filter(is.na(air_store_id))
    
    all_restaurants_temp <- air_store_info %>%
      dplyr::left_join(hpg_rests_in_air,by="air_store_id")
    
    all_restaurants <- bind_rows(all_restaurants_temp,hpg_rests_NOT_in_air)
    
    # Add less granular area info
    #-----------------------
      all_restaurants_DT <- as.data.table(all_restaurants)
      all_restaurants_DT[, air_area_name_part1 := tstrsplit(air_area_name, split = " ", keep = 1)]
      all_restaurants_DT[, hpg_area_name_part1 := tstrsplit(hpg_area_name, split = " ", keep = 1)]
      all_restaurants_DT[, air_area_name_part2 := tstrsplit(air_area_name, split = " ", keep = 2)]
      all_restaurants_DT[, hpg_area_name_part2 := tstrsplit(hpg_area_name, split = " ", keep = 2)]
   
      all_restaurants <- as.data.frame(all_restaurants_DT)%>%
        mutate(
          air_area_name_depth2 = paste(air_area_name_part1,air_area_name_part2,sep=" ")
          ,hpg_area_name_depth2 = paste(hpg_area_name_part1,hpg_area_name_part2,sep=" ")
        )

  # Map hpg restaurant genres to the air categories
  #=================================================
    # Specify mapping determined in "Genre_Analysis.xlsx"
    #--------------------------------
      genre_mapping <- cbind.data.frame(
        hpg_genre = c(
          "Amusement bar",
          "Cafe",
          "Creation",
          "Creative Japanese food",
          "Grilled meat",
          "International cuisine",
          "Italian",
          "Japanese cuisine/Kaiseki",
          "Japanese food in general",
          "Japanese style",
          "Karaoke",
          "Okonomiyaki/Monja/Teppanyaki",
          "Party",
          "Seafood",
          "Spain Bar/Italian Bar",
          "Steak/Hamburger/Curry",
          "Bar/Cocktail",
          "Bistro",
          "Cantonese food",
          "Chinese general",
          "Dim Sum/Dumplings",
          "French",
          "Korean cuisine",
          "Pasta/Pizza",
          "Shabu-shabu/Sukiyaki",
          "Shanghai food",
          "Sichuan food",
          "Spain/Mediterranean cuisine",
          "Sushi",
          "Sweets",
          "Taiwanese/Hong Kong cuisine",
          "Thai/Vietnamese food",
          "Udon/Soba",
          "Western food"
        )
        ,air_genre_lookup = c(
          "Bar/Cocktail",
          "Bar/Cocktail",
          "Izakaya",
          "Izakaya",
          "Yakiniku/Korean food",
          "Dining bar",
          "Italian/French",
          "Japanese food",
          "Japanese food",
          "Izakaya",
          "Bar/Cocktail",
          "Okonomiyaki/Monja/Teppanyaki",
          "Karaoke/Party",
          "Izakaya",
          "Italian/French",
          "Western food",
          "Bar/Cocktail",
          "Italian/French",
          "Chinese",
          "Chinese",
          "Chinese",
          "Italian/French",
          "Yakiniku/Korean food",
          "Italian/French",
          "Japanese food",
          "Chinese",
          "Chinese",
          "Italian/French",
          "Japanese food",
          "Sweets",
          "Chinese",
          "Thai",
          "Japanese food",
          "Western food"
        )
        ,stringsAsFactors=FALSE
      )  
    # Apply mapping determined in "Genre_Analysis.xlsx"
    #--------------------------------
      all_restaurants_mapped <- all_restaurants %>%
        dplyr::left_join(
          genre_mapping, by = c("hpg_genre_name" = "hpg_genre")
        )%>%
        mutate(
          hpg_genre_name_mapped = air_genre_lookup
          ,genre = ifelse(!is.na(air_genre_name),air_genre_name,hpg_genre_name_mapped)
          ,area = ifelse(air_area_name_depth2 != "NA NA",air_area_name_depth2,hpg_area_name_depth2)
        )%>%
        select(air_store_id,hpg_store_id,area,genre)
      
  # Find out numbers of local restaurants for each air_store_id
  #=================================================
    local_competition = air_store_id_list
    local_competition$num_local_competitors = rep(NA,nrow(local_competition))
    local_competition$num_local_competitors_same_genre = rep(NA,nrow(local_competition))

    for(i in 1:nrow(air_store_id_list)){
      
      area = all_restaurants_mapped[i,"area"]
      genre = all_restaurants_mapped[i,"genre"]
      
      #==num_local_competitors==#
        local_competition[i,"num_local_competitors"] = length(which(all_restaurants_mapped$area == area))-1
      
      #==num_local_competitors_same_genre===#
        local_competition[i,"num_local_competitors_same_genre"] = length(which(all_restaurants_mapped$area == area & all_restaurants_mapped$genre == genre))-1
    }

# Save relevant objects
############################      
  saveRDS(
    object = local_competition
    ,file = paste0(
      project_wd
      ,"/data/"
      ,"local_competition"
      ,".rds"
    )
  ) 
      
      
      