install.packages('quantmod')
library(quantmod)
install.packages('dplyr')
library(dplyr)
install.packages('infotheo')
library(infotheo)
install.packages('caret')
library(caret)


getSymbols(c("^GSPC"))
head <- head(GSPC)
tail <- tail(GSPC) 

GSPC <- data.frame(GSPC)

GSPC$Close.Date <- row.names(GSPC)


new_set <- c()
for (row_set in seq(10000)) {
     row_quant <- sample(10:30, 1)
    print(row_quant)
     row_start <- sample(1:(nrow(GSPC) - row_quant), 1)
     market_subset <- GSPC[row_start:(row_start + row_quant),]
     market_subset <- dplyr::mutate(market_subset, 
                                    Close_Date = max(market_subset$Close.Date),
                                    Close_Gap=(GSPC.Close - lag(GSPC.Close))/lag(GSPC.Close) ,
                                    High_Gap=(GSPC.High - lag(GSPC.High))/lag(GSPC.High) ,
                                    Low_Gap=(GSPC.Low - lag(GSPC.Low))/lag(GSPC.Low),
                                    Volume_Gap=(GSPC.Volume - lag(GSPC.Volume))/lag(GSPC.Volume),
                                    Daily_Change=(GSPC.Close - GSPC.Open)/GSPC.Open,
                                    Outcome_Next_Day_Direction= (lead(GSPC.Volume)-GSPC.Volume)) %>%
          dplyr::select(-GSPC.Open, -GSPC.High, -GSPC.Low, -GSPC.Close, -GSPC.Volume, -GSPC.Adjusted, -Close.Date) %>%
          na.omit
     market_subset$Sequence_ID <- row_set
     new_set <- rbind(new_set, market_subset)
}
print(head(new_set))

 
range(new_set$Close_Gap)
data_dicretized <- discretize(new_set$Close_Gap, disc="equalfreq", nbins=3)
new_set$Close_Gap <- data_dicretized$X
new_set$Close_Gap_LMH <- ifelse(new_set$Close_Gap == 1, 'L', 
                                ifelse(new_set$Close_Gap ==2, 'M','H'))

range(new_set$Volume_Gap)
data_dicretized <- discretize(new_set$Volume_Gap, disc="equalfreq", nbins=3)
new_set$Volume_Gap <- data_dicretized$X
new_set$Volume_Gap_LMH <- ifelse(new_set$Volume_Gap == 1, 'L', 
                                 ifelse(new_set$Volume_Gap ==2, 'M','H'))

range(new_set$Daily_Change)
data_dicretized <- discretize(new_set$Daily_Change, disc="equalfreq", nbins=3)
new_set$Daily_Change <- data_dicretized$X
new_set$Daily_Change_LMH <- ifelse(new_set$Daily_Change == 1, 'L', 
                                   ifelse(new_set$Daily_Change ==2, 'M','H'))


new_set <- new_set[,c("Sequence_ID", "Close_Date", "Close_Gap_LMH", "Volume_Gap_LMH", "Daily_Change_LMH", "Outcome_Next_Day_Direction")]

new_set$Event_Pattern <- paste0(new_set$Close_Gap_LMH,      
                                new_set$Volume_Gap_LMH, 
                                new_set$Daily_Change_LMH)

compressed_set <- dplyr::group_by(new_set, Sequence_ID, Close_Date) %>%
     dplyr::summarize(Event_Pattern = paste(Event_Pattern, collapse = ",")) %>%
     data.frame
 compressed_set <- merge(x=compressed_set,y=new_set[,c(1,6)], by="Sequence_ID")

compressed_set_validation <- dplyr::filter(compressed_set, Close_Date >= Sys.Date()-90)
compressed_set <- dplyr::filter(compressed_set, Close_Date < Sys.Date()-90)

compressed_set <- dplyr::select(compressed_set, -Close_Date)
compressed_set_validation <- dplyr::select(compressed_set_validation, -Close_Date)

compressed_set <- compressed_set[abs(compressed_set$Outcome_Next_Day_Direction) > 5260500,]
compressed_set$Outcome_Next_Day_Direction <- ifelse(compressed_set$Outcome_Next_Day_Direction > 0, 1, 0)
compressed_set_validation$Outcome_Next_Day_Direction <- ifelse(compressed_set_validation$Outcome_Next_Day_Direction > 0, 1, 0)


compressed_set_pos <- dplyr::filter(compressed_set, Outcome_Next_Day_Direction==1) %>% dplyr::select(-Outcome_Next_Day_Direction)
compressed_set_neg <- dplyr::filter(compressed_set, Outcome_Next_Day_Direction==0) %>% dplyr::select(-Outcome_Next_Day_Direction)


build_transition_grid <- function(compressed_grid, unique_patterns) {
     grids <- c()
     for (from_event in unique_patterns) {
          print(from_event)
        
          for (to_event in unique_patterns) {
               pattern <- paste0(from_event, ',', to_event)
               IDs_matches <- compressed_grid[grep(pattern, compressed_grid$Event_Pattern),]
               if (nrow(IDs_matches) > 0) {
                    Event_Pattern <- paste0(IDs_matches$Event_Pattern, collapse = ',', sep='~~')
                    found <- gregexpr(pattern = pattern, text = Event_Pattern)[[1]]
                    grid <- c(pattern,  length(found))
               } else {
                    grid <- c(pattern,  0)
               }
               grids <- rbind(grids, grid)
          }
     }
     print(head(grids))
     
     grid_Df <- data.frame(pairs=grids[,1], counts=grids[,2])
     grid_Df$x <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 1)
     grid_Df$y <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 2)
     
     all_events_count <- length(unique_patterns)
     transition_matrix = t(matrix(as.numeric(as.character(grid_Df$counts)), ncol=all_events_count, nrow=all_events_count))
     transition_dataframe <- data.frame(transition_matrix)
     names(transition_dataframe) <- unique_patterns
     row.names(transition_dataframe) <- unique_patterns
     print(head(transition_dataframe))
     
     transition_dataframe[is.na(transition_dataframe)] = 0
     transition_dataframe <- transition_dataframe/rowSums(transition_dataframe) 
     return (transition_dataframe)
}
unique_patterns <- unique(strsplit(x = paste0(compressed_set$Event_Pattern, collapse = ','), split = ',')[[1]])

grid_pos <- build_transition_grid(compressed_set_pos, unique_patterns)
grid_neg <- build_transition_grid(compressed_set_neg, unique_patterns)


actual = c()
predicted = c()
for (event_id in seq(nrow(compressed_set_validation))) {
     patterns <- strsplit(x = paste0(compressed_set_validation$Event_Pattern[event_id], collapse = ','), split = ',')[[1]]
     pos <- c()
     neg <- c()
     log_odds <- c()
     for (id in seq(length(patterns)-1)) {
          
          log_value <- log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]])
          if (is.na(log_value) || (length(log_value)==0) || (is.nan(log(grid_pos[patterns[id],patterns[id+1]] / grid_neg[patterns[id],patterns[id+1]]))==TRUE)) {
               log_value <- 0.0
          } else if (log_value == -Inf) {
               log_value <- log(0.00001 / grid_neg[patterns[id],patterns[id+1]])
          } else if (log_value == Inf) {
               log_value <- log(grid_pos[patterns[id],patterns[id+1]] / 0.00001)
               
          }
          log_odds <- c(log_odds, log_value)
          
          pos <- c(pos, grid_pos[patterns[id],patterns[id+1]])
          neg <- c(neg, grid_neg[patterns[id],patterns[id+1]])
     }
     print(paste('outcome:', compressed_set_validation$Outcome_Next_Day_Direction[event_id]))
     print(sum(pos)/sum(neg))
     print(sum(log_odds))
     
     actual <- c(actual, compressed_set_validation$Outcome_Next_Day_Direction[event_id])
     predicted <- c(predicted, sum(log_odds))
     
}
library(dplyr)
library(caret)
result <- confusionMatrix(table(ifelse (predicted>0,1,0), actual))
print(result)