plot_actogram = function(tuco, height = "vedba", plot_days = NA){
    require(ggplot2)
    require(data.table)
    require(scales)
    require(egg)
    require(lubridate)
    require(dplyr)
    source("03_analysis/plot_actogram/stat-bar-tile-etho.R") # From ggetho
    source("03_analysis/plot_actogram/stat-tile-etho.R") # From ggetho 

    # Calculate Sunrise and Sunset Times
    # TODO: Take the median date first and only calculate crepuscules for that date.
    anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
    tuco$dawn    = maptools::crepuscule(crds = anillaco, dateTime = tuco$datetime,
                         solarDep = 6, direction = "dawn", POSIXct.out=TRUE)$day_frac  * 1440
    tuco$dusk    = maptools::crepuscule(crds = anillaco, dateTime = tuco$datetime,
                                        solarDep = 6, direction = "dusk", POSIXct.out=TRUE)$day_frac  * 1440
    tuco = tuco[,dawn := median(dawn), by = ID] 
    tuco = tuco[,dusk := median(dusk), by = ID] 
    
    # Fill animals that have less than 'days' with NAs
    if(!is.na(plot_days)){
        #  & any(n_days$max < plot_days)
        
        # number of days to show on actogram
        tuco = tuco[day_number <= plot_days]
        
        n_days = tuco %>% 
            group_by(ID) %>% 
            summarise(max = max(day_number))
        
        if(any(n_days$max < plot_days)){
            
            tuco_to_fill = tuco[, .(to_fill = max(day_number) < plot_days, max = max(day_number)), by = ID]
            tuco_to_fill = droplevels(tuco_to_fill[tuco_to_fill$to_fill])
            tuco_to_fill$max_datetime = tuco[ID %in% tuco_to_fill$ID][, .(max_datetime = max(datetime)), by = ID]$max_datetime
            
            # Create sequence of dates to fill
            seq_date = vector("list") # Initialize a empty list
            for (i in tuco_to_fill$ID) {
                max_datetime = tuco_to_fill[ID == i]$max_datetime
                fill_to = max_datetime + days(plot_days - tuco_to_fill[ID == i]$max)
                seq_date[[i]] = seq(from = max_datetime, to = fill_to, by = "1 min")
            }
            
            max_nrows = max(sapply(seq_date, length, simplify = T))
            seq_date = lapply(seq_date, 
                              function(x){
                                  if(length(x) < max_nrows){
                                      return(c(x,rep(NA,max_nrows - length(x))))
                                  }else{
                                      return(x)
                                  }
                              }
            )
            
            # Merge new sequence of datetimes to original dataset
            seq_date = setDT(seq_date) 
            seq_date = melt(seq_date, measure.vars = colnames(seq_date), variable.name = "ID", value.name = "datetime")
            tuco = data.table::merge.data.table(tuco, seq_date, by = c("ID","datetime"), all = T)
            tuco[is.na(tuco$vedba)]$vedba = 0
        }
    }
    
    # Separate Date and Time columns
    tuco[,time := round(hour(datetime)*60 + minute(datetime) + second(datetime)/60, 4)]
    tuco[,date := lubridate::date(datetime)]
    
    # Transform Lux into 0/1
    tuco[,lux := ifelse(lux >= 2, 1, 0)]
    tuco[,lux := ifelse(is.na(tuco$lux), 0, lux)]
    
    # Transformation function to invert datetime in ggplot
    c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
        a <- scales::as.trans(a)
        b <- scales::as.trans(b)
        
        name <- paste(a$name, b$name, sep = "-")
        
        trans <- function(x) a$trans(b$trans(x))
        inv <- function(x) b$inverse(a$inverse(x))
        
        scales::trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
    }
    rev_date <- c_trans("reverse", "date")
    # Actograms Activity -------------------------------------------------------
    # If 'none' it will plot VeDBA, otherwise it will plot states
    if(height == "vedba"){
        actograms =
            ggplot(data = tuco, aes(x = time, y = date, group = ID)) +
            geom_vline(aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
            geom_vline(aes(xintercept = dusk), color = "grey60", linetype = 2, size = 0.5) +
            geom_bar_tile(mapping = aes(height = lux), fill = "orange", alpha = 0.4, width = 5) +
            geom_bar_tile(mapping = aes(height = vedba), width = 1) +
            scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
            scale_y_continuous(trans = rev_date) + 
            facet_wrap(~ID, scales = "free_y", ncol = 3) +
            xlab("") +
            ylab("") + 
            #theme_article() +
            theme(panel.grid.major.y = element_line(color = "grey95"))
    }else{
        
        if(height %in% c("Low","Medium","High")){
            tuco$plot_state = F
            tuco$plot_state[which(tuco$state == height)] = T
        }else{
            stop("State is not a correct option. Should be 'vedba', 'rest', 'medium' or 'high'")
        }
        
        actograms =
            ggplot(data = tuco, aes(x = time, y = date, group = ID)) +
            geom_vline(aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
            geom_vline(aes(xintercept = dusk), color = "grey60", linetype = 2, size = 0.5) +
            #geom_bar_tile(mapping = aes(height = lux), fill = "orange", alpha = 0.4, width = 5) +
            geom_bar_tile(mapping = aes(height = as.numeric(plot_state)), width = 1) +
            scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
            scale_y_continuous(trans = rev_date) + 
            facet_wrap(~ID, scales = "free_y", ncol = 3) +
            xlab("") +
            ylab("") + 
            #theme_article() +
            theme(panel.grid.major.y = element_line(color = "grey95"))
    }
    
    sexlabels = unique(tuco[,.(sex,ID)]) %>% mutate(sex = if_else(sex == "m", "M", "F"))
    actograms = actograms + geom_text(x = Inf, y = Inf, 
                                      aes(label = sex), 
                                      data = sexlabels, vjust = 1.5, hjust = 1.6, 
                                      size = 3, family = "roboto") + 
        theme(legend.position = "none")
    actograms
}





