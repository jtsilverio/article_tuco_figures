calc_sunriset = function(tuco){
    # Calculate Sunrise and Sunset Times -------------------------------------------
    anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
    sunriset = tuco %>% dplyr::select(ID, season, datetime) %>%
        group_by(ID, season) %>% 
        summarise(datetime = median(datetime))
    
    sunriset$dawn = maptools::crepuscule(crds = anillaco,
                                         dateTime = sunriset$datetime,
                                         solarDep = 6, 
                                         direction = "dawn", 
                                         POSIXct.out=TRUE)$day_frac  * 1440
    
    sunriset$dusk = maptools::crepuscule(crds = anillaco,
                                         dateTime = sunriset$datetime,
                                         solarDep = 6,
                                         direction = "dusk",
                                         POSIXct.out=TRUE)$day_frac  * 1440
    
    sunriset_season = sunriset %>%
        group_by(season) %>% 
        summarise(dawn = median(dawn), dusk = median(dusk), daytime_length = dusk - dawn)
    
    return(sunriset_season)
}


