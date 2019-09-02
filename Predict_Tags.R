setwd("D:/Study/Data Science/The Edwisor/Project/Major Project_Kaggle")
getwd()

#Loading datasets
Cooking  = read.csv("cooking.csv", stringsAsFactors = FALSE)
Biology  = read.csv("biology.csv", stringsAsFactors = FALSE)
Crypto   = read.csv("crypto.csv", stringsAsFactors = FALSE)
Diy      = read.csv("diy.csv", stringsAsFactors = FALSE)
Robotics = read.csv("robotics.csv", stringsAsFactors = FALSE)
Travel   = read.csv("travel.csv", stringsAsFactors = FALSE)

# Structures of data
str(Biology)
str(Cooking)
str(Crypto)
str(Diy)
str(Robotics)
str(Travel)

# Adding "category variable" to datasets
Biology$category = "biology"
str(Biology)

Cooking$category = "cooking"
str(Cooking)

Crypto$category = "crypto"
str(Crypto)

Diy$category = "diy"
str(Diy)

Robotics$category = "robotics"
str(Robotics)

Travel$category = "travel"
str(Travel)

# Combining the datasets to one
Misc_Data = rbind(Biology, Cooking, Crypto, Diy, Robotics, Travel)
str(Misc_Data)

ls()

# Removing unused data frames
rm(Biology, Cooking, Crypto, Diy, Robotics, Travel)
ls()

# duplicate id's in the dataset
print(sum(duplicated(Misc_Data$id)))

# Concatenate id & catagory for identification (Removal of dunplicate id's)
Misc_Data$id_cat = paste0(Misc_Data$id, "|", Misc_Data$catagory)
str(Misc_Data)

# duplicate id's in the dataset
print(sum(duplicated(Misc_Data$id_cat)))


# Loading library
library(tm)

#customize functions
RemoveHtmlTags = function(htmlString){
return(gsub("<.*?>","",htmlString))
}

customF = function(param_big_string){
#' lower-cases, removes punctuation, new line and return characters,
#' and removes unnecessary whitespace, then strsplits
split_content = sapply(param_big_string, removePunctuation, preserve_intra_word_dashes = T)
split_content = sapply(split_content, function(y) gsub("[\r\n]"," ",y))
split_content = sapply(split_content, tolower)
split_content = sapply(split_content, function(y) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", y, perl = TRUE))
return(split_content = (sapply(split_content, strsplit, " ")))
}

# Applying the customized function
token_content = customF(Misc_Data$content)
token_tags = customF(Misc_Data$tags)
token_titles = customF(Misc_Data$title)

# Check data
print(Misc_Data[1143,2])

# initialize a big empty character matrix
results = matrix("", ncol=10, nrow = nrow(Misc_Data))


# Loop to stablish relation b/w tags in title/content 
# browser() can be used to debug the loop in R.

for(i in 1:nrow(Misc_Data)){
if(i %% 10000 == 0){print(paste0('printing index every 10k indeces: ', i))}


#subset
this_tags    = token_tags[[i]]
this_title   = token_titles[[i]]
this_content = token_content[[i]]

# Copy the ids in results matrix
results[i,10] = Misc_Data$id_cat[i]

# error checking....no length should be zero
if(length(this_tags) == 0 | length(this_title) == 0 | length(this_content) == 0) {
print('length == 0 , skipping this line')
next
}

#length
this_tag_length = length(this_tags)
results[i,1] =  this_tag_length

this_title_length = length(this_title)
results[i,2] = this_title_length

this_content_length = length(this_content)
results[i,3]= this_content_length

#matches
sum_title_matches = sum(this_tags %in% this_title)  # - col 4
results[i,4] = sum_title_matches

sum_content_matches = sum(this_tags %in% this_content) #  - col 5
results[i,5] = sum_content_matches


#percents
results[i,6] = round(sum_title_matches/this_tag_length * 100, digits = 2)  #perc_tag_title_matches - col 6
results[i,7] = round(sum_content_matches/this_tag_length * 100, digits = 2)  #perc_tag_content_matches - col 7
results[i,8] = round(sum_title_matches/this_title_length * 100, digits = 2)  #perc_title_richness - col 8
results[i,9] = round(sum_content_matches/this_content_length * 100, digits = 2) #perc_content_richness - col 9
}

#join back into original data
result_names = c("tag_length", "title_length", "content_length", "title_matches", "content_matches", "percent_title_matches", "percent_content_matches", "title_tag_richness", "content_tag_richness", "id_cat")
result_names

# Converting matrix into data frame
results_df = data.frame(results, stringsAsFactors = F)
str(results_df)

# Naming the column
names(results_df) = result_names
str(results_df)

#quicker merge of huge data using data.table library 
library(data.table)

setDT(Misc_Data); setkey(Misc_Data, id_cat)
setDT(results_df); setkey(results_df, id_cat)

initial_tag_matches = merge(x=Misc_Data, results_df, all = T)
str(initial_tag_matches)

setDF(initial_tag_matches)

# Converting variables to 'numeric' one
for(i in 7:15){
initial_tag_matches[,i] = as.numeric(initial_tag_matches[,i])
}
str(initial_tag_matches)


# Plots using 'ggplot2' library
library(ggplot2)

# Set the theme
theme_set(theme_grey(base_size = 12))

# all overlapping with transparency

# Density curve: Percents of Tag words found in Title
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches), fill = category)) +
geom_density(alpha=0.3) +
ggtitle("Density: Percents of Tags that exactly match a Word in the Title") +
labs(x='Percents of Tag Words found in Title') +
geom_rug(color = 'darkblue')

# Density curve: Percents of Tag words found in Content
ggplot(initial_tag_matches, aes(x = as.numeric(percent_content_matches), fill = category)) +
geom_density(alpha=0.3) +
ggtitle("Density: Percent of Tags that exactly match a Word in the Content") +
labs(x='Percent of Tag Words found in Content') +
geom_rug(color = 'darkblue')


# Faceted Density curve: Percents of Tag words found in Title
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches) )) +
geom_density(alpha=0.8, fill='darkred') + 
facet_wrap(~category) +
ggtitle("Density: Percent of Tags that EXACTLY Match a Word in the Title") +
labs(x='Percent of Tag Words Found in Title') +
geom_rug(color='darkgreen')

# Faceted Histrogram: Percents of Tag words found in Title
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches) )) +
geom_histogram(alpha=0.8, fill='darkred', binwidth = 10) +
facet_wrap(~category) + 
ggtitle("Histogram: Percent of Tags that exactly match a Word in the Title") +
labs(x='Percent of Tag Words found in Title') +
geom_rug(color = 'darkgreen')


# Faceted Density curve: Percents of Tag words found in Content
ggplot(initial_tag_matches, aes(x = as.numeric(percent_content_matches) )) +
geom_density(alpha=0.6, fill='darkgreen') + 
facet_wrap(~category) +
ggtitle("Density: Percent of Tags that EXACTLY Match a Word in the Content") +
labs(x='Percent of Tag Words Found in Content') +
geom_rug(color='darkred')

# Faceted Histrogram: Percents of Tag words found in Content
ggplot(initial_tag_matches, aes(x = as.numeric(percent_title_matches) )) +
geom_histogram(alpha=0.6, fill='darkgreen', binwidth = 10) +
facet_wrap(~category) + 
ggtitle("Histogram: Percent of Tags that exactly match a Word in the Content") +
labs(x='Percent of Tag Words found in Content') +
geom_rug(color = 'darkred')
