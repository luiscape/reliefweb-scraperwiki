#### Function for creating a single table with the reports per contry. #### 

rw.indicators.country <- function(df = NULL, begin = 2000, end = 2014, entity = NULL, focus = TRUE) {    
    
    require(hdxdictionary)  # add to NAMESPACE when complete working.
    
    if (is.null(entity) == TRUE) { stop('Please provide an entity.') }
    
    # Normalizing the ISO3 codes. 
    df$iso3 <- toupper(df$iso3)
    
    # Normalizing the data.
    df$created <- as.Date(df$created)
    
    # Adding a month and year facets. 
    # df$month <- month(df$created, label = TRUE, abbr = FALSE)  # we are not using month now.
    df$year <- year(df$created)
    
    if (focus == TRUE) { n.countries <- nrow(subset(hdx.dictionary, hdx.dictionary[7] == TRUE)) 
                         countries <- subset(hdx.dictionary, hdx.dictionary[7] == TRUE)}
    else { n.countries <- nrow(hdx.dictionary) 
           countries <- hdx.dictionary }
    
    # Create progress bar.
    pb <- txtProgressBar(min = 0, max = n.countries, style = 3)
    final.long <- data.frame()
    
    for (i in 1:n.countries) {  # for the number of focus countries in the master dictionary.
        
        setTxtProgressBar(pb, i)  # Updates progress bar.
        
        final.table <- data.frame()  # creating the a clean data.frame
        
        iso3.country <- as.character(countries$iso3c[i])
        
        for (i in begin:end) {  # iterations over the years. 1999 has a few months of data.
            year <- subset(df, df$year == i & df$iso3 == iso3.country)
            year.it <- data.frame(nrow(year))
            
            if (entity == 'report') { colnames(year.it)[1] <- 'n.reports' }
            if (entity == 'disaster') { colnames(year.it)[1] <- 'n.disasters' }
            
            year.it$year <- i
            year.it$iso3 <- iso3.country
            year.it$country.name <- hdxdictionary(year.it$iso3, 'iso3c', 'country.name')
            colnames(year.it)[ncol(year.it)] <- "Country Name" 
            
            if (i == 1999) { final.table <- year.it }  
            else { final.table <- rbind(final.table, year.it) }  
        }
        
        if (i == 1) { final.long <- final.table }  # creating a single long table.
        else { final.long <- rbind(final.long, final.table) }
        
    }
    
    
    if (entity == 'report') { final.wide <- dcast(final.long, iso3 ~ year, value.var="n.reports") } 
    if (entity == 'disaster') { final.wide <- dcast(final.long, iso3 ~ year, value.var="n.disasters") } 
    
    final.wide$country.name <- hdxdictionary(final.wide$iso3, 'iso3c', 'country.name')
    
    
    file.name.long <- paste("data-summary/ReliefWeb-", 
                            "ALLCountries-", entity, "-",
                            begin, "-", end, "-long.csv", sep = "")
    
    file.name.wide <- paste("data-summary/ReliefWeb-", 
                            "ALLCountries-", entity, "-",
                            begin, "-", end, "-wide.csv", sep = "")
    
    write.csv(final.long, file = file.name.long, row.names = FALSE)  # for long
    write.csv(final.wide, file = file.name.wide, row.names = FALSE)  # for wide
    
    return(final.wide)
}