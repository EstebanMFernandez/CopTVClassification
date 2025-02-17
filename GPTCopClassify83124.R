library(httr)
library(jsonlite)
library(readxl)
library(tidyverse)
library(tidytext)
library(readtext)
library(quanteda)
library(glue)
# read in atts data
DFSm <- read_excel("C:/Users/esteb/Dropbox (MIT)/Cop TV Project/13 TV Transcripts (Web-scraped)/TrainingAtts.xlsx")
# read in transcripts
filelist <- list.files("C:/Users/esteb/OneDrive/Documents/Desktop/Transcripts")
setwd("C:/Users/esteb/OneDrive/Documents/Desktop/Transcripts")
hope <-readtext(filelist,docvarnames = c("Order","Show","Season","Episode"), 
                docvarsfrom = "filenames",dvsep = "_")

# Header placeholder.
header <- "Next we will go through a set document term frequencies based on transcripts from popular police television programs 
with brief descriptions of the summary and whether events listed above occur in the episode. Please use this information to help us determine whether these events occur in other episodes. It is also important that we only want these actions / events if they involve a police character.
Here is one set of document term frequencies presented like this [word frequency] for example if the word man occurs three times in a transcript it will appear like this: [man 3]
. Remember that the following events from the above list occur in this episode which help determine how the police are portrayed in this episode: \n\n"

Intro <- "I am going to train you to classify episodes of police television. We
want you to tell us when episodes depict police officers taking certain
positive or negative actions.For this training this I am going to provide you document term frequencies, 
a summary of the episode, and the meta information about the episode (show title,season, episode number). 
I will also tell you whether the actions we are interested in occur in the episode and a brief description of
those actions. 
We want you to use the information provided in these training episodes for future 
episodes without the actions being labeled. Please tell us when you have stored the 
info needed to help you classify in the future"

header<- c()
header[1] <- "This is the first of three sets of summaries,document term frequencies and action labels/descriptions 
for training. The frequencies are
presented like this [word frequency]. For example if the word man occurs three times in a transcript 
it will appear like this: [man 3]"
header[2] <- 
  "This is the second of three sets of summaries,document term frequencies and action labels/descriptions  for training. 
  The frequencies are presented like this [word frequency]."
header[3] <-   "This is the last of three sets of summaries, document term frequencies for training and action labels/descriptions 
. The frequencies are presented like this [word frequency]."



summaryint <- "In addition to the attribute descriptions here is a brief summary of the episode"

outro <- "Again, store this information and use it to label episode transcripts for the presence of these attributes in future queries.Acknowledge 
that you have done this"

ClassHeader <- "Now I am going to show you another set of term frequencies based on transcripts of 
police shows. I would like you
to tell my if anything of the following are performed by police characters from THIS episode provided below DO NOT
evaluate the previous 5 training prompts again..:
Police being presented as good guys, Police solve a case, police arrest a person who committed a crime,
a cop puts themselves in danger for others, a cop is injured. 

Also tell us if a police officer does any of these bad things but try and make
sure that these actions are portrayed as negative in the story: 
police officers take bribes, police use unjustified excessive force, racial profiling, 
failing to solve the case because of their own actions, sexism or homophobia. Remember we only want
you to tell us who did these things if they involved a police officer and not a criminal performing similar actions.
So tell us if any of the above occured and who performed them. Please use the information from the previous training episodes I provided but do not summarize those episodes again. Also use information you already know about this episode
from the internet generally. I am going to provide the frequencies to you below. You should list what happens only like this: Yes, the police
solve the case.:
"

### list of attributes
# paste together the attributes so it is robust to any number of attributes being present
# split into even (labels) and odds (descriptions)
evens <- function(x) subset(x, x %% 2 == 0)
odds <- function(x) subset(x, x %% 2 != 0)
check <- evens(1:ncol(DFSm))
ers <- odds(1:ncol(DFSm))
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
DFMer <- function(string){
  #stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
  #stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  
  freqs <- string %>% 
    str_replace_all(stopwords_regex, '') %>% 
    str_to_lower %>%                   
    str_replace_all("[[:punct:][:digit:][:cntrl:]]", "") %>%
    tibble(documents=.) %>%
    unnest_tokens(word, documents) %>%
    count(word, sort = TRUE) %>%
    filter(n > 1)
  prompter <- paste("[",freqs$word,freqs$n,collapse="]")  
  return(prompter)
}
#
summaryint <- "In addition to the action descriptions here is a brief summary of the episode"
i <- 1
AttsList <- list()
for(i in 1:3){
  m1 <- as.matrix(DFSm[i,ers])
  m2 <- as.matrix(DFSm[i,check])
  Holder <- paste(m1,m2,sep=":")
  ifelse(length(which(grepl("NA:", Holder)==TRUE))>=1,
         gee <- which(grepl("NA:", Holder)!=TRUE),gee <- 1:6)
  Holder <- Holder[gee]
  # 
  summary <- DFSm$Summary[i]
  # prompt summary placeholder

  finsumm <- paste(summaryint,summary,sep=":")
  # add breaks
  Atts2 <- glue_collapse(Holder,sep = "\n")
  Transcript <- hope$text[i]
  quick <- paste("This is from episode",DFSm[i,"Show"],"episode",DFSm[i,"Episode"],"season",DFSm[i,"Season"])
  AttsList[[i]] <- paste(quick,header[i],Atts2,DFMer(Transcript),outro,sep="\n")
}
AttsList[[1]] <- paste(Intro,AttsList[[1]])
AttsList[[2]]
# 
ClassHeader <- "
Now we are moving onto the next part where I want you to classify episodes of police television. I would like you to tell me if any of the following actions are performed by police characters:
police officers take bribes or engage in corruption, police use unjustified excessive force or commit
sexual assault, racial profiling, 
failing to solve the case because of their own actions, sexism or homophobia. 
Remember we only want you to positively 
identify an action if it is performed by a police officer and not a non-police character. 
To perform I'd like you to also use your general training data—your pre-existing knowledge and patterns— in conjunction
with with  the patterns you learned from the training we did prior for these document term frequencies and episode metadata I provide to you. 
this I will provide you with document term frequencies and episode metadata. 
I'd like you to also use your general training data—your pre-existing knowledge and patterns— in conjunction
with the term frequencies and metadata patterns you learned. 
Please return the classification with the action and label seperated by a colon: Police solve the case: Yes/No.
"
ClassHeader <-  "We are now moving on to the next task: classifying episodes of police television shows. 
For each episode, please determine if any of the following actions are performed by police characters:
\n\n- Police officers take bribes,commit crimes, assault women or engage in corruption\n- Police use unjustified 
excessive force
\n- Racial profiling\n- Failing to solve the case due to their own actions\n- Sexism or homophobia\n\nImportant: 
Only identify these actions if they are performed by police officers and not by non-police characters.
\n\nTo complete this task, use both:\n\n1. Your general training data—your pre-existing knowledge and patterns.
\n2. The document term frequencies and episode metadata that I will provide to you.\n\nPlease format your 
classification as follows: Action: Yes/No, where Action is one of the actions listed above.\n\nFor example: 
Police use unjustified excessive force: Yes. 
Metadata: "

 
ClassHeader <- "
We are now moving on to the next task: classifying episodes of police television shows.

For each episode, please determine if any of the following actions are performed by police characters:

- Police officers take bribes, commit crimes, assault women, or engage in corruption
- Police use unjustified excessive force
- Racial profiling
- Failing to solve the case due to their own actions
- Sexism or homophobia

**Important:** Only identify these actions if they are performed by police officers, not by non-police characters.

To complete this task, use both:

1. **Your general knowledge:** Apply the patterns and relationships learned from your training data.
2. **The document term frequencies (DTFs), episode descriptions, and human labels provided in previous prompts:** In three previous prompts, you have learned the relationships between labels and their corresponding DTFs, descriptions, and summaries.

Format your classification as follows:

Action: Yes/No, where Action is one of the actions listed above.

For example:
Police use unjustified excessive force: Yes.
"





TestPrompt4
# 
TestPrompt1 <- paste(paste("This is from the show","Blue Bloods on CBS","episode",hope[6,"Episode"],"season",hope[6,"Season"]),
                     ClassHeader,DFMer(hope$text[6]),"")
##
TestPrompt2 <- paste(paste("This is from the show",hope[7,"Show"],"episode",hope[7,"Episode"],"season",hope[7,"Season"]),
                     ClassHeader,DFMer(hope$text[7]),"")
##
TestPrompt3 <- paste(paste("This is from the show",hope[8,"Show"],"episode",hope[8,"Episode"],"season",hope[8,"Season"]),
                     ClassHeader,DFMer(hope$text[8]),"")
##
TestPrompt4 <- paste(ClassHeader,paste("This is the episode 'Internal Affairs' from the show","Law and Order:SVU","episode",hope[9,"Episode"],"season",hope[9,"Season"]),
                     DFMer(hope$text[9]),"")
##
TestPrompt5 <- paste(ClassHeader,paste("This is from the show","Chicago PD","episode",hope[10,"Episode"],"season",hope[10,"Season"]), 
                     "",DFMer(hope$text[10]),"")
## 
TestPrompt5 <- paste(ClassHeader, 
                     "",DFMer(hope$text[11])," \n\n", 
                     paste("This is from the show","Law and Order: SVU"
                           ,"episode",hope[11,"Episode"],"season",hope[11,"Season"],
                           "titled Guardians and Gladiators"))
TestPrompt5

TestPrompt5 <- paste(ClassHeader,"Remember to summarize this episode/ the one I am providing below and NOT one of the training
      episodes. The correct episode to summarize is: ",paste("This is from the show",hope[10,"Show"],"episode",hope[10,"Episode"],"season",hope[10,"Season"]), 
      "Here is the DFM",DFMer(hope$text[10]),"","Remember to utilize the training from the previous DFM I have
      given you to analyze this and use this information in conjunction with your own knowledge of the episode to classify and be a little conservative. If it deals heavily with an issue just say that it occurs")
### 
filelist
##
TestPrompt4
AttsList[[4]]
##
AttsList[[4]] <- TestPrompt5 
AttsList[[7]] <- TestPrompt2
AttsList[[8]] <- TestPrompt3
AttsList[[9]] <- TestPrompt4
AttsList[[10]] <- TestPrompt5
TestPrompt5
# function to call API and classify. Return GPT's response. Beginning stages of doing multiple transcripts
# use 3.5 for testing and 4.0 for actual training

### creating prompts
###
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
# 
api_key <- "" #enterhere
TestPrompt3
AttsList
AttsList[[4]] <- TestPrompt5 
GPTOut <- classify_texts3(AttsList[c(1:4)], api_key)
print(GPTOut[[4]])
AttsList[[4]]
str_count(AttsList[[2]])
### 


# Start Here
# create Tyler's OG version
# Function for API queries
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
    # keep retrying to make connection until it fails 5 times
    while (!success && retry_count < max_retries) {
      response <- POST(url, 
                       add_headers(Authorization = paste("Bearer", api_key),
                                   'OpenAI-Project'="proj_D1OR1VtjYLBtPEl9WRz9AnxW",
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
# API Key
api_key <- ""
# load in transcripts
filelist <- list.files("C:/Users/esteb/OneDrive/Documents/Desktop/Transcripts")
setwd("C:/Users/esteb/OneDrive/Documents/Desktop/Transcripts")
hope <-readtext(filelist,docvarnames = c("Order","Show","Season","Episode"), 
                docvarsfrom = "filenames",dvsep = "_")
hope$doc_id
# Create prompts. intro to transcript

### 10 ,11, 5, 1, 2 are a good mix

P1 <- "Please carefully review the following transcript from a popular police show. 
I will ask you to analyze the content in a follow-up question."
prompt1 <- paste(P1,hope$text[5])

# Prompt 2: Identify Police Actions
prompt2 <- "Review the provided transcript and identify if any of the following actions are depicted involving police officers:\n
- Taking bribes, committing crimes, assaulting women, or engaging in corruption\n
- Using unjustified excessive force\n
- Engaging in racial profiling\n
- Failing to solve the case due to their own misconduct or negligence\n
- Demonstrating sexism or homophobia\n\n
For each identified action, provide specific examples or lines from the transcript. Clearly explain why each example fits the category, ensuring you only consider actions performed by police officers, not criminals or other justice system members."

# Prompt 3: Binary Classification

prompt3 <- "Based on your detailed analysis, provide a binary classification (1 for 'yes' or 0 for 'no') for each category below. Ensure your classification reflects both the presence of the action and its portrayal as negative or positive:\n
- Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
- Police using unjustified excessive force\n
- Engaging in racial profiling\n
- Failing to solve the case due to their own actions or negligence\n
- Demonstrating sexism or homophobia\n\n
Strictly use 1 or 0 in your response, with 1 indicating that the action is present and negatively portrayed, and 0 indicating that the action is either not present or portrayed positively. Do not include additional text or explanations in this classification step."





# Prompt 4: Portrayal Assessment
prompt4 <- "Update the binary classification for each category based on whether the identified action was portrayed negatively in the show:\n
- Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
- Police using unjustified excessive force\n
- Racial profiling by police\n
- Police failing to solve the case due to their own actions\n
- Demonstrations of sexism or homophobia by police\n\n
Use 1 to indicate that the action is present and portrayed negatively. 
Use 0 to indicate that the action is not present or is portrayed positively. 
Provide only 1s and 0s for the updated classification."




# Prompt 5: Final Classification
prompt5 <- "Integrating your updated analysis and portrayal assessment, provide the final binary classification for each category. Use 1 if the action is present and portrayed negatively, and 0 if it is not present or portrayed positively. Ensure your final classifications accurately reflect both the occurrence of the actions and their portrayal in the show:\n
- Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
- Police using unjustified excessive force\n
- Engaging in racial profiling\n
- Failing to solve the case due to their own actions or negligence\n
- Demonstrating sexism or homophobia\n\n
Your response should only include 1s and 0s, with brief explanatory notes if needed. Ensure accuracy and consistency with previous analysis and portrayal assessments."


AttsList <- list(prompt1,prompt2,prompt3,prompt4,prompt5)
### Do multiple transcripts
ger <- c(1,2,5,10,11)
responseholder <- list()
promptholder <- list(prompt1,prompt2,prompt3,prompt4,prompt5)
# loop over and create prompts. keep binary responses which are the third element of result list
for(i in 1:length(ger)){
  poi <- ger[i]
  responseholder[[i]]<-print(GPTOut2[[3]])
} 
rm(list = setdiff(ls(), "GPTOutPos"))
GPTOut2 <- classify_texts3(promptholder2[c(1:4)], api_key)
GPTOut2
# Expected labels
expec1 <- c("1","0","0","0","0")
expec2 <- c("0","0","0","0","0")
expec5 <- c("0","0","0","1","0")
expect10 <-  c("0","1","1","1","0")
expec11 <- c("0","1","1","1","0")
hope$
# Extract ChatGPT label results
tofind <- paste(c("1","0"), collapse="|")

res1 <- str_extract_all(print(responseholder[[1]]), tofind)
res2 <- str_extract_all(print(responseholder[[2]]), tofind)
res3 <- str_extract_all(print(responseholder[[3]]), tofind)
res4 <- str_extract_all(print(responseholder[[4]]), tofind)
res5 <- str_extract_all(print(responseholder[[5]]), tofind)
# ARe they the same? 
same1 <- setequal(expec1,unlist(res1))
same2 <- setequal(expec2,unlist(res2))
same3 <- setequal(expec5,unlist(res3))
same4 <-setequal(expect10,unlist(res4))
same5<-setequal(expec11,unlist(res5))
### Now Scale this up to many episodes
filelist2 <- list.files("C:/Users/esteb/OneDrive/Documents",pattern=".Rda")
setwd("C:/Users/esteb/OneDrive/Documents")

###
### 
P1 <- "Please carefully review the following transcript from a popular police show. 
I will ask you to analyze the content in a follow-up question."
prompt1 <- paste(P1,gsub("[^[:alnum:][:space:].]", "", hope$text[11]))
# 
prompt2 <- "Review the transcript and provide a binary classification for each of the following categories based on the depicted actions involving police officers:\n\n
1. **Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption**\n
2. **Police using unjustified excessive force**\n
3. **Racial profiling by police**\n
4. **Police failing to solve the case due to their own actions or negligence**\n
5. **Demonstrations of sexism or homophobia by police**\n\n
For each category, use '1' if the action is present and portrayed negatively in the transcript, or '0' if the action is absent or portrayed positively. Ensure your classification reflects a thorough review of the transcript."

# Basic approach, more hardcore about actual prompting, example discussion with ChatGPT.
# 
prompt3 <- "Based on the updated analysis and portrayal assessment, provide the final binary classification for each category below:\n
- Police officers taking bribes, committing crimes, assaulting women, or engaging in corruption\n
- Police using unjustified excessive force\n
- Racial profiling by police\n
- Police failing to solve the case due to their own actions\n
- Demonstrations of sexism or homophobia by police\n\n
Provide a single binary value for each category:\n
"
### 
promptholder <- list(prompt1,prompt2,prompt3,prompt4,prompt5)
##
BigOne <- data.frame(matrix(ncol=9))
colnames(BigOne) <- colnames(garchomp)
### 
# BigOne3 <- BigOne
for(b in 62:length(filelist2)){
# wait for 2 minutes between seasons to avoid rate limit issues
Sys.sleep(120)
# load in a season of transcripts

load(filelist2[b])

# the transcripts are  in a dataframe called garchomp, I think from the RA? 
# remove non alphanumeric characters
garchomp$transcript <- gsub("[^[:alnum:][:space:].]", "", garchomp$content)
# create the first prompt
garchomp<- garchomp %>%
  mutate(prompt1 = paste0(P1, transcript))
# put prompts together into a list
promptholder2 <- lapply(garchomp$prompt1, function(text) {
  # Combine the dataframe entry with additional strings
  list(text, prompt2, prompt3)
})
# save name of the season
garchomp$show <- filelist2[[b]]
### nested loop
# within each season, go through each episode and classify, justify the corpus.
for (i in 1:length(promptholder2)){
  geck <- classify_texts3(promptholder2[[i]],api_key)
  # remove numbers betwen **
  Classes <- regmatches(geck[[3]], gregexpr("(?<=\\*\\*)\\d+(?=\\*\\*)", geck[[3]], perl = TRUE))
  # remove all digits
  all_digits <- regmatches(geck[[3]], gregexpr("\\d+", geck[[3]]))[[1]]
  garchomp$class1[i] <- Classes
  garchomp$class2[i] <- list(all_digits)
  # just in case, save the final classification
  garchomp$class3[i] <- geck[3]
  Sys.sleep(10)
}
# put together dataframe
BigOne <- rbind(BigOne,garchomp)
# save because sometimes ChatGPT will fail for NO reason. Just a connection error. Save in case of failure
# or hit some limit.
save(BigOne,file=paste0("C:/Users/esteb/Desktop/Outputs/Labels_latest.Rda"))
}

# All responses start with classifications. Are told only to put 1's and 0's in response
# extract first 5 0's or 1's
extract_first_five <- function(text) {
  binary_classifications <- regmatches(text, gregexpr("[01]", text))[[1]]
  first_five <- binary_classifications[1:5]
  return(as.integer(first_five))
}
extract_bin <- function(text) {
  binary_classifications <- regmatches(text, gregexpr("[01]", text))[[1]]
  return(as.integer(binary_classifications))
}
BigOne$binary_classifications2 <- 0
BigOne$binary_classifications2[-c(1:120)] <- lapply(BigOne$class3[-c(1:120)], extract_bin)
#
###
extract_first_five_binary <- function(x) {
  binary_only <- x[x %in% c(0, 1)]  # Filter to keep only 1's and 0's
  return(binary_only[1:5])          # Return the first 5 of them
}
BigOne$binary_classifications[c(1:120)]<- lapply(BigOne$class2[c(1:120)], extract_first_five_binary)
# split classifications that are stored in a vector and add as 5 separate columns. 
binary_matrix <- do.call(rbind, BigOne$binary_classifications)

# Convert the matrix to a data frame and assign column names
df_split <- as.data.frame(binary_matrix)
names(df_split) <- c("corruption", "excessiveforce", "racism", "failure", "sexism")

# Combine the new columns with the original data frame
BigOne <- cbind(BigOne, df_split)
BigOne<- BigOne[-1,]
# sum stats.
sum(as.numeric(BigOne$excessiveforce))/1245
sum(as.numeric(BigOne$failure))
BigOne[, 13:17] <- lapply(BigOne[, 13:17], as.numeric)
BigOne$NegScore <- apply(BigOne[, 13:17], 1, function(row) ifelse(any(row == 1), 1, 0))
sum(BigOne$NegScore)/1245
BigOne$NegScore
# 
library(tidyr)
BigOne2 <- BigOne %>%
  separate(show, 
           into = c("ShowTitle", "Season"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )
#
remove_substrings <- function(filename) {
  # Remove "911" and ".Rda"
  cleaned <- gsub("911", "", filename)  # Remove "911"
  cleaned <- gsub("\\.Rda$", "", cleaned)  # Remove ".Rda" at the end
  return(cleaned)
}
#

#
BigOne2$Season[which(is.na(BigOne2$Season)==TRUE)] <-BigOne2$ShowTitle[which(is.na(BigOne2$Season)==TRUE)]
# 
BigOne2$Season<- sapply(BigOne2$Season, remove_substrings)
#
BigOne2$ShowTitle[1:78] <- "911"
# 
library(dplyr)
result <- BigOne2 %>%
  group_by(ShowTitle) %>%
  summarize(
    SumNegScore = sum(NegScore),
    TotalObservations = n(),
    AverageNegScore = SumNegScore / TotalObservations
  )
result
save(BigOne2,file="C:/Users/esteb/Dropbox (MIT)/Cop TV Project/13 TV Transcripts (Web-scraped)/ChatGPTOut.Rda")
