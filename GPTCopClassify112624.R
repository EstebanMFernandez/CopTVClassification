library(readxl)
library(dplyr)
library(tidyr)
library(psych)
# Set the working directory to the folder containing the Excel sheets
setwd("")
### Classify Text Function.
classify_texts3 <- function(texts, api_key) {
  url <- "https://api.openai.com/v1/chat/completions"
  
  # Initialize the conversation history
  conversation_history <- list()
  
  # Function to append user and assistant messages to the conversation history
  add_message_to_history <- function(role, content) {
    conversation_history <<- append(conversation_history, list(list(role = role, content = content)))
  }
  
  results <- sapply(texts, function(text) {
    # Add user message to conversation history
    add_message_to_history("user", text)
    
    # Create payload with conversation history
    payload <- list(
      model = "gpt-4o",
      messages = conversation_history,
      max_tokens = 1000
    )
    
    jsonPayload <- toJSON(payload, auto_unbox = TRUE)
    
    response <- NULL
    max_retries <- 5
    retry_count <- 0
    success <- FALSE
    
    while (!success && retry_count < max_retries) {
      response <- POST(url, 
                       add_headers(Authorization = paste("Bearer", api_key),
                                   'OpenAI-Project'="proj_A0FjGjt29Rg92yrXBaVfxyPQ",
                                   'Content-Type' = "application/json"
                       ),
                       body = jsonPayload,
                       encode = "json"
      )
      
      if (status_code(response) == 429) {
        # Rate limit exceeded, wait and retry
        Sys.sleep(10)  # wait for 10 seconds before retrying
        retry_count <- retry_count + 1
      } else {
        success <- TRUE
      }
    }
    
    if (!success) {
      stop("Failed to get a response from the API after multiple attempts due to rate limiting.")
    }
    
    result <- content(response, as = "parsed")
    assistant_response <- result$choices[[1]]$message$content
    
    # Add assistant response to conversation history
    add_message_to_history("assistant", assistant_response)
    
    return(assistant_response)
  })
  
  return(results)
}

### PROMPTS ###
### negative prompts ###
P1 <- "Please briefly review the following transcript from a police show and summarize its main points. Focus on key elements of the scenes, character actions, and any major themes. Limit your response to 4-5 sentences."
# Condensed Prompt 2: Identifying Negative Police Actions
prompt2_neg <- "Review the transcript and identify if any negative actions by police officers are depicted, including:\n
1. Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
2. Police using unjustified excessive force\n
3. Racial profiling by police\n
4. Police failing to solve the case due to their own actions or negligence\n
5. Demonstrations of sexism or homophobia by police\n
For each identified action, provide a brief example or specific line from the transcript. Clearly explain why the example fits the category. Limit each example to 2-3 sentences or 100 characters."

# Condensed Prompt 3: Binary Classification for Negative Actions
prompt3_neg <- "Based on your analysis, classify each negative action category with 1 (yes) or 0 (no):\n
1. Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
2. Police using unjustified excessive force\n
3. Racial profiling by police\n
4. Police failing to solve the case due to their own actions or negligence\n
5. Demonstrations of sexism or homophobia by police\n
Only use 1 or 0 in your response. Keep explanations concise, and limit examples to 2-3 sentences or 100 characters."

# Condensed Prompt 4: Final Classification for Negative Actions
prompt4_neg <- "Provide the final classification for each category based on your analysis. Ensure that the identified actions are portrayed negatively, and not used as justifications in the pursuit of justice or to stop crime. Classify each action as follows:\n
1. Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
2. Police using unjustified excessive force\n
3. Racial profiling by police\n
4. Police failing to solve the case due to their own actions or negligence\n
5. Demonstrations of sexism or homophobia by police\n
Use 1 if the action is present and portrayed negatively, without being justified in pursuit of justice or stopping crime. Use 0 if the action is absent or portrayed positively. Include brief notes if necessary. Limit any examples to 2-3 sentences or 100 characters."
###
promptholderneg <- list(prompt2_neg, prompt3_neg, prompt4_neg)

## positive prompts ### 
# Prompt 2: Identifying Positive Actions
prompt2_pos <- "Review the provided transcript and identify if any of the following positive actions are depicted involving police officers:\n
- Police officers portrayed as 'good guy' characters (e.g., showing integrity, fairness, or empathy)\n
- Wrongful actions by police (e.g., coercion, violence, corruption) portrayed as just, routine, necessary, or noble in the pursuit of justice\n
- Wrongful actions by police not normalized but portrayed as actions by a 'bad apple' officer\n
- Police solving the case or arresting the criminal(s)\n
- Police officers taking heroic action, such as putting themselves in danger for the sake of justice\n
- Police officers being harmed or injured in the line of duty\n\n
For each identified action, provide specific examples or lines from the transcript. Clearly explain why each example fits the category, ensuring you only consider actions performed by police officers, not criminals or other members of the criminal justice system."

# Prompt 3: Binary Classification for Positive Actions
prompt3_pos <- "Now, based on your analysis, provide a simple 1 (yes) or 0 (no) classification for each of the following positive categories:\n
- Police officers portrayed as 'good guy' characters\n
- Wrongful actions by police portrayed as just, routine, necessary, or noble in the pursuit of justice\n
- Wrongful actions by police not normalized but portrayed as actions by a 'bad apple' officer\n
- Police solving the case or arresting the criminal(s)\n
- Police officers taking heroic action\n
- Police officers harmed or injured in the line of duty\n\n
Again, please only consider actions taken by police characters and not by criminals or other members of the criminal justice system. Only use 1 or 0 in the classification and nowhere else in the response."

# Prompt 4: Final Classification for Positive Actions
prompt4_pos <- "Integrating your analysis, provide the final binary classification for each positive category. Use 1 if the action is present and portrayed positively, and 0 if it is not present or portrayed negatively. Ensure your final classifications accurately reflect both the occurrence of the actions and their portrayal in the show:\n
- Police officers portrayed as 'good guy' characters\n
- Wrongful actions by police portrayed as just, routine, necessary, or noble in the pursuit of justice\n
- Wrongful actions by police not normalized but portrayed as actions by a 'bad apple' officer\n
- Police solving the case or arresting the criminal(s)\n
- Police officers taking heroic action\n
- Police officers harmed or injured in the line of duty\n\n
Your response should only include 1s and 0s, with brief explanatory notes if needed. Ensure accuracy and consistency with previous analysis."
###
promptholderpos <- list(prompt1,prompt2_pos,prompt3_pos,prompt4_pos)
# Create a file list (filelistnew) of all Excel sheets in the directory
filelistnew2 <- list.files(pattern = "\\.xlsx$")
# 
additional_combined_df_last <- data.frame()
#### NEGATIVE LABELING #### 

for (file in filelistnew2) {
  
  # Load the current Excel sheet
  df <- read_excel(file)
  
  # Ensure column names are valid and manageable by renaming them early
  colnames(df) <- c("Episode", "Script")  # Assuming two columns with episode number and script text
  
  # Remove empty rows
  df_cleaned <- df %>% filter_all(any_vars(!is.na(.)))
  
  # Extract the show name from the filename (without extension)
  show_name <- tools::file_path_sans_ext(file)
  
  # Loop through each row (episode)
  for (i in 1:nrow(df_cleaned)) {
    
    # Get the episode script text
    episode_text <- df_cleaned[i, "Script"] 
    
    # Clean the text
    cleaned_text <- gsub("[^a-zA-Z0-9\\s\n]", "", episode_text, perl = TRUE)
    
    # Create the prompts (for negative classification)
    prompt1 <- paste(P1, cleaned_text)
    current_prompts <- c(prompt1, promptholderneg)
    
    # Classify the text using classify_texts3
    GPTOutNeg <- classify_texts3(current_prompts, api_key)
    
    # Store the results in a new dataframe row and add the show name
    new_row <- cbind(df_cleaned[i, ], as.data.frame(t(GPTOutNeg)), Show = show_name)
    
    # Append the new row to additional_combined_df
    additional_combined_df <- rbind(additional_combined_df, new_row)
    
  }
  
  # Optional: Save the additional_combined_df after processing each Excel sheet
  save(additional_combined_df, file = paste0("additional_classified_last", show_name, ".Rda"))
}
### Positive Classification. Separate because of rate limit issues###
###
positive_combined_df <- data.frame()
# Loop through each Excel file in filelistnew
for (file in filelistnew2) {
  
  # Load the current Excel sheet
  df <- read_excel(file, col_names = FALSE)
  
  # Ensure column names are valid and manageable by renaming them early
  colnames(df) <- c("Episode", "Script")  # Assuming two columns with episode number and script text
  
  # Remove empty rows
  df_cleaned <- df %>% filter_all(any_vars(!is.na(.)))
  
  # Extract the show name from the filename (without extension)
  show_name <- tools::file_path_sans_ext(file)
  
  # Loop through each row (episode)
  for (i in 1:nrow(df_cleaned)) {
    
    # Get the episode script text
    episode_text <- df_cleaned[i, "Script"] 
    
    # Clean the text
    cleaned_text <- gsub("[^a-zA-Z0-9\\s\n]", "", episode_text, perl = TRUE)
    
    # Create the prompts (for positive classification)
    prompt1 <- paste(P1, cleaned_text)
    current_prompts <- c(prompt1, promptholderpos)  # Now using promptholderpos for positive classification
    
    # Classify the text using classify_texts3
    GPTOutPos <- classify_texts3(current_prompts, api_key)
    
    # Store the results in a new dataframe row and add the show name
    new_row <- cbind(df_cleaned[i, ], as.data.frame(t(GPTOutPos)), Show = show_name)
    
    # Append the new row to positive_combined_df
    positive_combined_df <- rbind(positive_combined_df, new_row)
    
  }
  
  # Optional: Save the positive_combined_df after processing each Excel sheet
  save(positive_combined_df, file = paste0("positive_classified", show_name, ".Rda"))
}
## Done in batches so combine with previous labeled DFs
# Bind rows of new_combined_df and additional_combined_df
negativelabs <- rbind(new_combined_df, additional_combined_df)
# Bind rows of combined_df and positive_combined_df
positivelabs <- rbind(combined_df, positive_combined_df)
#### match with years for TS analysis. M to 1 merge
matched_data <- inner_join(positivelabs, Listofshows, by = c("Show_Title" = "Show2", "Season_Number" = "Season"))
###
# Filter shows from Listofshows with Count > 2009
listofshows_titles <- unique(Listofshows$Show2[which(Listofshows$Count > 2009)])
negativelabs_titles <- unique(negativelabs$Show_Title)

# Perform an inner join to match negativelabs and Listofshows based on Show_Title and Season_Number
matched_dataneg <- inner_join(negativelabs, Listofshows, by = c("Show_Title" = "Show2", "Season_Number" = "Season"))
matched_datapos <- matched_data[which(matched_data$Count>2009),]
###
### Factor Analysis
factor_data <- matched_dataneg[, c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5")]
# Convert the columns to numeric if they are not already
factor_data[] <- lapply(factor_data, as.numeric)
# 
library(psych)
kmo_result <- KMO(factor_data)
print(kmo_result)
factor_data <- matched_dataneg[, c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5")]

# Convert the columns to numeric if they are not already
factor_data[] <- lapply(factor_data, as.numeric)
# 
kmo_result <- KMO(factor_data)
print(kmo_result)
###
tetra_matrix <- tetrachoric(factor_data)$rho

# Perform the factor analysis using the tetrachoric correlation matrix
fa_result <- fa(tetra_matrix, nfactors = 1, rotate = "varimax")
# 
factor_scores <- factor.scores(factor_data, fa_result)
matched_dataneg$Factor_Score <- factor_scores$scores[, 1]
###
### Positive Factor Scores 
matched_datapos$FinalPrompt <- sub("^1\\.", "", matched_datapos$Prompt4)
### 
matched_datapos$binary_classifications2 <- str_extract_all(matched_datapos$FinalPrompt, "\\b[01]\\b")

# Convert the list to a dataframe with separate columns for each binary classification
# Convert the list of binary classifications into a dataframe with each value in separate columns
classification_df <- do.call(rbind, lapply(matched_datapos$binary_classifications2, function(x) {
  # Ensure each row has 5 elements, pad with NA if needed
  if (length(x) < 6) {
    c(x, rep(NA, 6 - length(x)))
  } else {
    x[1:6]
  }
}))

# Convert to a data frame and ensure values are numeric
classification_df <- as.data.frame(classification_df, stringsAsFactors = FALSE)
classification_df[] <- lapply(classification_df, as.numeric)

# Rename columns to Class_1, Class_2, Class_3, Class_4, Class_5
colnames(classification_df) <- c("Class_1", "Class_2", "Class_3", "Class_4", "Class_5","Class_6")


# Bind the new classification columns to matched_dataneg
matched_datapos <- cbind(matched_datapos, classification_df)

### 
factor_data <- matched_datapos[, c("Class_2", "Class_6","Class_4","Class_1","Class_5")]
#
summary(factor_data$Class_4,na.rm=T)

# Convert the columns to numeric if they are not already
factor_data[] <- lapply(factor_data, as.numeric)
# 
kmo_result <- KMO(factor_data)
print(kmo_result)
### Not very good for positive. Too little variation in some of these (all true)
tetra_matrix <- tetrachoric(factor_data)$rho
# returns error, likely wrong
# Perform the factor analysis using the tetrachoric correlation matrix
fa_result <- fa(tetra_matrix, nfactors = 1, rotate = "varimax")
# 
factor_scores <- factor.scores(factor_data, fa_result)
matched_datapos$Factor_Score <- factor_scores$scores[, 1]
###
# Filter data for years 2010 and onward
matched_dataneg_filtered <- matched_dataneg %>% filter(Count >= 2010)
matched_datapos_filtered <- matched_datapos %>% filter(Count >= 2010)

# Calculate yearly average Factor_Score for matched_dataneg
neg_avg_scores <- matched_dataneg_filtered %>%
  group_by(Count) %>%
  summarise(Average_Factor_Score_Neg = mean(Factor_Score, na.rm = TRUE))

# Calculate yearly average Factor_Score for matched_datapos
pos_avg_scores <- matched_datapos_filtered %>%
  group_by(Count) %>%
  summarise(Average_Factor_Score_Pos = mean(Factor_Score, na.rm = TRUE))
average_scores <- merge(neg_avg_scores, pos_avg_scores, by = "Count", all = TRUE)
# change years for display
year_labels <- c(
  "2010" = "2010-2011",
  "2011" = "2011-2012",
  "2012" = "2012-2013",
  "2013" = "2013-2014",
  "2014" = "2014-2015",
  "2015" = "2015-2016",
  "2016" = "2016-2017",
  "2017" = "2017-2018",
  "2018" = "2018-2019",
  "2019" = "2019-2020",
  "2020" = "2020-2021",
  "2021" = "2021-2022",
  "2022" = "2022-2023"
)

# Apply the new year range labels to the 'Count' column in average_scores
average_scores$Count <- recode(average_scores$Count, !!!year_labels)
#
ggplot(data = average_scores, aes(x = Count)) +
  geom_line(aes(y = Average_Factor_Score_Neg, color = "Negative", group = 1), size = 1) +
  geom_line(aes(y = Average_Factor_Score_Pos, color = "Positive", group = 1), size = 1) +
  labs(x = "Year Range",  # Remove the title and change x-axis label to reflect year range
       y = "Average Factor Score",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###
### Since the positive is having strange results due to the low variance 
# lets try a different approach
matched_datapos <- matched_datapos %>%
  rename(
    good_guys = Class_1,
    justified = Class_2,
    bad_apples = Class_3,
    solve_case = Class_4,
    heroism = Class_5,
    harmed_in_line_of_duty = Class_6
  )

# Rename columns in matched_dataneg (Negative Traits)
matched_dataneg <- matched_dataneg %>%
  rename(
    corruption = Class_1,
    unjustified_excessive_force = Class_2,
    racial_profiling = Class_3,
    failure_to_solve_case = Class_4,
    sexism = Class_5
  )

# Create Traits by year and episode (binary presence)
positive_traits_long <- matched_datapos %>%
  select(Show_Title, Count, good_guys, solve_case, heroism,title) %>%
  filter(Count >= 2010) %>%
  pivot_longer(cols = good_guys:heroism, names_to = "Trait", values_to = "Value") %>%
  mutate(Valence = "Positive")
View(positive_traits_long)
# Step 2.2: Convert matched_dataneg to Long Format and Filter Data
negative_traits_long <- matched_dataneg %>%
  select(Show_Title, Count, corruption, racial_profiling, failure_to_solve_case,title) %>%
  filter(Count >= 2010) %>%
  pivot_longer(cols = corruption:failure_to_solve_case, names_to = "Trait", values_to = "Value") %>%
  mutate(Valence = "Negative")
# stack trait presence variables to take average
traits_combined <- bind_rows(positive_traits_long, negative_traits_long)
# rename traits for presentation
trait_renames <- c(
  "corruption" = "Corruption and Bribery",
  "failure_to_solve_case" = "Failure to Solve Case",
  "good_guys" = "Portrayal as Good Guys",
  "heroism" = "Heroic Actions",
  "racial_profiling" = "Racial Profiling",
  "solve_case" = "Successful Case Resolution"
)
# 
# Apply the renaming to the traits in the dataframe
traits_combined$Trait <- recode(traits_combined$Trait, !!!trait_renames)
#
year_labels <- c(
  "2010" = "2010-2011",
  "2011" = "2011-2012",
  "2012" = "2012-2013",
  "2013" = "2013-2014",
  "2014" = "2014-2015",
  "2015" = "2015-2016",
  "2016" = "2016-2017",
  "2017" = "2017-2018",
  "2018" = "2018-2019",
  "2019" = "2019-2020",
  "2020" = "2020-2021",
  "2021" = "2021-2022",
  "2022" = "2022-2023"
)

# Apply the new year range labels to the 'Count' column in traits_combined
traits_combined$Count <- recode(traits_combined$Count, !!!year_labels)
# New PLot
ggplot(traits_combined, aes(x = Count, y = Value, color = Valence, linetype = Trait, group = interaction(Trait, Valence))) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Positive and Negative Traits Over Time by Valence",
       x = "Year",
       y = "Average Value of Trait",
       color = "Valence",
       linetype = "Trait") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###
### Show Chart. Using proxy to label valence.
# negative if ANY of these negative things happen
negative_proxy <- matched_dataneg %>%
  group_by(Show_Title) %>%
  summarise(Proportion_One_Negative = mean(corruption + unjustified_excessive_force + racial_profiling + failure_to_solve_case + sexism > 0, na.rm = TRUE))
### Positive if 4/5 positives occur
positive_proxy <- matched_datapos %>%
  group_by(Show_Title) %>%
  summarise(Proportion_At_Least_Four_Positive = mean((good_guys + justified + solve_case + heroism + harmed_in_line_of_duty) >= 4, na.rm = TRUE))
## combine proxy values
show_proxy_summary <- inner_join(positive_proxy, negative_proxy, by = "Show_Title")
# rename for presentation
show_renames <- c(
  "blue bloods" = "Blue Bloods",
  "bones" = "Bones",
  "castle" = "Castle",
  "chicago pd" = "Chicago P.D.",
  "criminal minds" = "Criminal Minds",
  "csi" = "CSI",
  "csi miami" = "CSI: Miami",
  "csi: cyber" = "CSI: Cyber",
  "csi: ny" = "CSI: NY",
  "csi: vegas" = "CSI: Vegas",
  "elementary" = "Elementary",
  "fbi" = "FBI",
  "fbi most wanted" = "FBI: Most Wanted",
  "hawaii five-0" = "Hawaii Five-0",
  "lonestar" = "Lone Star",
  "mentalist" = "The Mentalist",
  "ncis" = "NCIS",
  "ncis: la" = "NCIS: LA",
  "ncis: no" = "NCIS: NO",
  "nine one one" = "9-1-1",
  "organized crime" = "Organized Crime",
  "shades of blue" = "Shades of Blue",
  "svu" = "SVU",
  "swat" = "SWAT",
  "the rookie" = "The Rookie"
)

# Apply the renaming to the 'Show_Title' column in the dataframe
show_proxy_summary$Show_Title <- recode(show_proxy_summary$Show_Title, !!!show_renames)
#
ggplot(show_proxy_summary, aes(x = Show_Title)) +
  # Add lines between positive and negative proxies
  geom_segment(aes(xend = Show_Title, y = Proportion_At_Least_Four_Positive, yend = Proportion_One_Negative), color = "gray", size = 1) +
  
  # Add blue points for highly positive episodes
  geom_point(aes(y = Proportion_At_Least_Four_Positive), color = "blue", size = 3) +
  
  # Add red points for episodes with negative depictions
  geom_point(aes(y = Proportion_One_Negative), color = "red", size = 3) +
  
  # Add axis labels and theme adjustments
  labs(x = "Show Title", y = "Proportion of Episodes") +
  
  # Theme adjustments for improved readability
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
