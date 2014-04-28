#!/usr/bin/Rscript

## 
# This collector uses the basis of the `ReliefWeb` 
# R package (https://github.com/luiscape/reliefweb). 
# 
# It collects data from the ReliefWeb API (), creating 
# indicators in the process. 
# 
# The list of indicators created are: 
#     - Number of Reports
#     - Number of Disasters
# 
## 

library(RCurl)
library(rjson)

ReliefWebLatest <- function(entity = NULL,
                     limit = NULL,
                     text.query = NULL,
                     query.field = NULL,
                     query.field.value = NULL,
                     add.fields = NULL,
                     from = NULL,
                     to = NULL,
                     debug = FALSE,
                     csv = FALSE, 
                     ver = "v1") {  # the v0 can be used for testing.
    
    #### Validation tests. ####
    # The validation tests before are useful for helping the user make a 
    # right query and understand why his query isn't working. 
    
    # Install depencency packages if not installed     
    if (is.null(query.field) == TRUE && is.null(text.query) == TRUE) { 
        stop("You have to either provide a `text.query' input or a `query.field` + `query.field.value` input.") 
    }
    
    if (is.null(query.field) == FALSE && is.null(query.field.value) == TRUE) { 
        stop("Please provide a value with a query field.") 
    }
    if (length(query.field) > 1) { stop('Please provide only one query field. Run rw.query.fields() if you are in doubt.') }
    
    if (is.null(limit) == FALSE && limit < 0 && tolower(limit) != "all") { 
        stop('Please provide an integer between 1 and 1000 or all.') 
    }
    
    if (is.null(limit) == FALSE && limit > 1000 && tolower(limit) != "all") { 
        stop('Please provide an integer between 1 and 1000 or all.') 
    }  # Increase the upper limit of the function.
    
    if (is.null(limit) == FALSE) { limit <- tolower(limit) }
    
    if (is.null(entity) == TRUE) { stop('Please provide an entity.') }
    
    all <- "all"
    
    
    #### Building the URL. ####

    # The entity url to be queried.
    if (is.null(entity) == FALSE) { entity.url <- paste0('/', entity) }
    
    # Offset URL -- for iterations.
    offset.url <- "?offset=0"  # starting at 0.
    
    # The limit to be used. 1000 is the maximum.
    if (is.null(limit) == TRUE) { limit.url <- paste0("&", "limit=", 10, "&")
                                  warning("The default limit for this querier is 10. \nIf you need more please provide a number           using \nthe 'limit' parameter.") }
    
    # for colleting `all` the database.
    if (is.null(limit) == FALSE) { limit.url <- paste("&", "limit=", 
                                                      ifelse(limit == "all", 1000, limit),
                                                      "&") }
    
    # making sure the query is NULL 
    if (is.null(text.query) == TRUE) { text.query.url <- NULL }
    
    # for querying open text
    if (is.null(text.query) == FALSE) {
        text.query.url <- paste("query[value]=", 
                                text.query, 
                                sep = "")
        warning('In this version searching the open text field \nwill override whatever other field you have\nincluded the `query` paramenter. In further \nversions the open text field will allow you to\nfurther refine your search.')
    }
    
    # adding query fields.
    if (is.null(query.field) == FALSE) { query.field.url <- paste("query[value]=", 
                                                                  query.field, 
                                                                  ":", 
                                                                  query.field.value, 
                                                                  sep = "") }
    
    
    # cleaning the query field if nothing is provided. 
    if (is.null(query.field) == TRUE) { query.field.url <- NULL }
    
    # Function for building the right query when more than one field is provided.
    many.fields <- function(qf = NULL) { 
        
        # When the entity 'countries' or 'sources' is provided, 
        # don't add the 'date.created' -- they don't have one.
        
        ##
        # date.created was default in an old version. 
        # taking it out for now.
        ## 
#         
#         if (entity == "country" | entity == "source") { qf <- qf
#         } else { ifelse(all(is.na(match(qf, 'date.created')) == TRUE), 
#                         qf[length(qf) + 1] <- 'date.created', '') }
#         
        
        
        all.fields.url.list <- list()
        for (i in 0:(length(qf) - 1)) { 
            field.url <- paste("fields[include][",i,"]=", qf[i + 1], sep = "")
            all.fields.url.list[i + 1] <- paste("&", field.url, sep = "")
        }
        all.fields.url <- paste(all.fields.url.list, collapse = "")
        return(all.fields.url)
    }
    
    if (is.null(add.fields) == FALSE) { add.fields.url <- many.fields(qf = add.fields) }
    
    ## From and to paramenters. ##
    # if (is.null(from) == TRUE) {}  ## Implement in future versions.
    # if (is.null(to) == TRUE) {}  ## Implement in future versions.
    
    ## Building URL for aquiring data. ##
    api.url <- "http://api.rwlabs.org/"
    version.url <- ver

    # taking our date.created. 
    # not necessary in version 1 of the API.
    if (entity != "country" | entity != "sources") { 
        date.created.url <- "&sort[0]=date.created:desc" 
    }
    else { date.created.url <- "" }

    
    query.url <- paste0(api.url,
                       version.url,
                       entity.url,
                       offset.url,
                       limit.url,
                       text.query.url,
                       query.field.url,
                       add.fields.url,
                       date.created.url)
    
    

    #### Fetching the data. ####

    # Function for creating a data.frame.
    if (debug == TRUE) {
        x <- paste("The URL being queried is: ", query.url, sep = "")
        warning(x)
    }

    # Function to convert the resulting lits into rows in the data.frame
    FetchingFields <- function(df = NULL) {
        for (i in 1:df$count) {
            x <- data.frame(df$data[[i]]$fields)
            if (i == 1) { data <- x }
                else { 
                    common_cols <- intersect(colnames(data), colnames(x))
                    data <- rbind(
                        data[, common_cols], 
                        x[, common_cols]
                    )
                }
        }
        data
    }

    # Function for creating iterations to go around 
    # the 1000-results limitation.
    RWIterations <- function(df = NULL) {
        
        final <- df
        
        limit <- ifelse(limit == "all", 1000, limit)
        total <- ceiling(count/limit)
        
        # Create progress bar.
        pb <- txtProgressBar(min = 0, max = total, style = 3)
        
        for (i in 2:total) {
            
            setTxtProgressBar(pb, i)  # Updates progress bar.
            
            offset.url <- paste0("?offset=", limit * i)
            
            # updating URL in each iteration
            query.url.it <- paste(api.url,
                               version.url,
                               entity.url,
                               offset.url,
                               limit.url,
                               text.query.url,
                               query.field.url,
                               add.fields.url,
                               sep = "")
            
            if (debug == TRUE) {
                print(paste("This is the it.url", it.url, sep = ""))
                print(paste("From iteration number ", i, sep = ""))
            }
            
            ## Error handling function for each iteration.
            tryCatch(x <- fromJSON(getURLContent(query.url.it)), 
                     error = function(e) { 
                         print("There was an error in the URL queried. Skipping ...")
                         final <- final
                     }, 
                     finally = { 
                         x <- FetchingFields(df = x)  # Cleaning fields.
                         final <- rbind(final, x)
                     }
            ) 
        }
        close(pb)
        return(final)
    }

    
    # Getting the count number for iterations later.
    count <- fromJSON(getURLContent(query.url))$totalCount
    
    # Querying the URL.
    query <- fromJSON(getURLContent(query.url))
    
    # Retrieving a data.frame
    data <- FetchingFields(query)
    
    # Creating a metadata data.frame.
#     if (csv == TRUE) {
#         meta.data <- query[1, 1:7]
#         write.csv(meta.data, file = paste("data/", 
#                                           paste(add.fields, 
#                                                 collapse = "-", 
#                                                 "-", 
#                                                 entity, 
#                                                 "-metadata.csv", sep = ""), row.names = FALSE))}
#     
    
    
    # UI element.
    print(paste0("Fetching ~", 
                ifelse(is.null(limit) == TRUE, 10, 
                       ifelse(identical(limit,all) == TRUE, count, limit)), 
                " records."))
    

    
#     Only run iterator if we are fetching "all" entries.
#     Note: improve to be more > 1000, but not 'all'.
    if (identical(limit, all) == TRUE) { data <- RWIterations(df = data) }
    
    #### Cleaning the resulting data. ####
    # Transform dates from Epoch to year-month-day.
#     rw.time <- function(df = NULL) {
#         df$created <- df$created/1000 # To eliminate the miliseconds.
#         df$created <- as.Date(as.POSIXct(as.numeric(df$created), origin = "1970-01-01"))
#         return(df)
#     }
    
#     query <- rw.time(df = query)
    
    
    # Keeping only the columns of interest.
#     query <- query[,10:ncol(query)]
    
    
    if (debug == TRUE) {
        before.duplicates <- nrow(query)
    }
    
#     data <- unique(query)
    
    if (debug == TRUE) { 
        # UI element     
        after.duplicates <- nrow(query)
        duplicates <- before.duplicates - after.duplicates
        print(paste("There were ", duplicates, " duplicates in the query."))
    }
    
    #   # Storing the resulting data in a CSV file. 
    if (csv == TRUE) {
        write.csv(data, file = "ReliefWeb-query", "-", entity, ".csv", sep = "")
    }
    
    print("Done.")
    return(data)
}


GetLatestDisasters <- function() { 
    ReliefWebLatest(entity = 'disasters', 
                    limit = 1000,
                    text.query = "",
                    query.field = NULL,
                    query.field.value = NULL,
                    add.fields = c('id', 'primary_country.iso3', 'date.created'),
                    from = NULL,
                    to = NULL,
                    debug = TRUE,
                    csv = FALSE, 
                    ver = "v1") 
}

GetLatestReports <- function() { 
    ReliefWebLatest(entity = 'reports', 
                limit = 1000,
                text.query = "",
                query.field = NULL,
                query.field.value = NULL,
                add.fields = c('id', 'primary_country.iso3', 'date.created'),
                from = NULL,
                to = NULL,
                debug = TRUE,
                csv = FALSE, 
                ver = "v1") 
}

# # Storing the data in a SW database.
# library(RSQLite)
# db <- dbConnect(dbDriver("SQLite"), dbname="scraperwiki.sqlite", loadable.extensions=TRUE)
# dbWriteTable(db, "data")
# dbDisconnect(db)
