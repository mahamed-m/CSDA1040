#Topic Modelling for Project 2

library(tidyr)
library(dplyr)
library(tidytext)
library(textdata)
library(broom)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(tm)
library(ggthemes)
library(textclean)
library(stringr)
library(twitteR)
library(topicmodels)
library(gsubfn)
library(htmlTable)

data <- tidytext::unnest_tokens(read.csv(file.choose(), na.strings = c("", "NA"), stringsAsFactors = FALSE), word, text)


data <- subset(data, select = c(tweet_id, word))#leaves only our terms and IDs

#check column names and check if there are any NA values
colnames(data)#have the correct columns
sum(is.na(data))#no NAs in our data

clean_tokens <- data
clean_tokens$word <- gsubfn('[[:digit:]]+', '', clean_tokens$word)
clean_tokens$word <- gsubfn('[[:punct:]]+', '', clean_tokens$word)
data("stop_words")

clean_tokens <- clean_tokens %>%
  filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)

data_tokens <- clean_tokens %>%
  filter(!(word==""))


data_tokens <- data_tokens %>%
  group_by(tweet_id) %>%
  mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)

data_tokens[is.na(data_tokens)] <- ""

data_tokens <- tidyr::unite(data_tokens, word,-tweet_id,sep =" " )
data_tokens$word <- trimws(data_tokens$word)

#Create a DocumentTermMatrix

library(textmineR)
library(stopwords)

dtm <- CreateDtm(data_tokens$word,
                 doc_names = data_tokens$tweet_id,
                 ngram_window = c(1, 2),
                 stopword_vec = c(stopwords::stopwords(language = "en",
                                                       source = "smart")))


#Check Term Frequency

tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>%
  select(term, term_freq, doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

#remove any words that appear <2 or in >half the doc

vocab <- tf$term[tf$term_freq >1 & tf$doc_freq < nrow(dtm)/2]

dtm = dtm


#use Latent Dirichlet Allocation

data_lda <- LDA(dtm, k=20, control = list(seed = 1234))
data_lda

#check per-topic per-word probabilites
data_topics <- tidy(data_lda, matrix = 'beta')
data_topics

data_top_terms <- data_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Graph below gives us topic and term graphs
#like it better since it splits into more categories


data_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



#creating a model a different way
#both work and have different strengths
#method above requires less work and doesn't have as much calculations
#method below allows us to create topic labels easily using theta values


model <- FitLdaModel(dtm = dtm, k = 20, iterations = 200,
                     burnin = 180, alpha = 0.1, beta = 0.05,
                     optimize_alpha = TRUE, calc_likelihood = TRUE,
                     calc_coherence = TRUE, calc_r2 = TRUE, cpus = 2)
#alpha and beta for the LDA model control the distribution of topics/terms
#high alpha and beta correspond to more terms/topics and vice-versa for low
#we want low values for both to limit the number of topics
#as well as to limit how many terms appear in multiple topics

model$r2
#R-squared is not very high, 0.05070639; low proportion of variability in terms
#suggests that the terms typically revolve around the same topics
#makes sense for our dataset -> specificity of airline tweets means
#we see the same topics discussed online


plot(model$log_likelihood, type='l')
#we probably don't need the log_likelihood and r-squared numbers
#not working a regression model so it's not truly necessary

summary(model$coherence)#check term coherence (don't expect high numbers)
hist(model$coherence, col = 'blue', main = "Histogram of probabilistic coherence")

#get the top 5-10 terms
model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
head(t(model$top_terms))

#check the prevalence of each topic
model$prevalence <- colSums(model$theta)/sum(model$theta) * 100

#plot prevalence; should be proportional to alpha, which it is
plot(model$prevalence, model$alpha, xlab = 'prevalence', ylab = 'alpha')

#Let's label our topics and model the labels
model$labels <- LabelTopics(assignments = model$theta > 0.05,
                            dtm = dtm,
                            M = 1)

head(model$labels)

#next, we'll add it all together; topics, coherence score, prevalence
##and top terms
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence, 4),
                            top_terms = apply(model$top_terms, 2,
                                              function(x){
                                                paste(x, collapse = ", ")
                                              }),
                            stringsAsFactors = FALSE)

#create a table showing LDA-intuited topic labels
htmlTable(model$summary[ order(model$summary$prevalence,
                               decreasing = TRUE), ][1:10, ])


#THIS PART IS NOT NECESSARY FOR THE MARKDOWN
#Our model was fitted using the Gibbs method for LDA

#compare different LDA methods for future predictions
#gibbs method uses conditional probability to decide topics
assignments <- predict(model, dtm,
                       method = "gibbs",
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)

#dot method; calculates dot-product probability of topics
assignments_dot <- predict(model, dtm,
                           method = "dot")
#let's compare the two methods
barplot(rbind(assignments[10,], assignments_dot[10,]),
        col = c("red", "blue"), las = 2, beside = TRUE)
legend("topright", legend = c("gibbs", "dot"), col = c("red", "blue"), 
       fill = c("red", "blue"))
#plot shows per topic which method works best. Overall, gibbs is slower
#but there are benefits to each


#EXPLANATION FOR THE MODEL

#Our Markdown already has the first half, which is good and works well
#Think we can use parts of both
#secondary model using textmineR allows us to get a closer look at topic labels
#can pull a table together with labelled topics and terms
##probably the only thing we really need from it tbh


##There are some limitations that the model showed
#reviews from twitter tend to revolve around similar topics
#points out the same words the wordcloud does, which reinforces certain issues
#this could be a result of the limited characters available
#also, people tend to complain online on platforms like Twitter
#probably find more robust reviews elsewhere
#still useful to see this information
#ex; topic 6 revolves around customer service and response times
##could use that to show airlines they need to look into it
###clients are most likely complaining about that (or they could find it's an area they are doing well in)


