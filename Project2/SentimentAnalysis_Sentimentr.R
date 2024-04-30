install.packages("pdftools")
library(pdftools)
library(plotly)
library(magrittr)

txt <- pdf_text('/Users/admin/Documents/WebSearch-7700/Project1/beauty-the-beast.pdf')

lines <- ''

for (i in 2:length(txt)) {
  tmp <- gsub('\r\n', ' ', txt[i]) 
  lines <- paste(lines, tmp, sep=' ')
}

vec <- strsplit(lines, '\\.')
df <- data.frame(vec)

df <- as.data.frame(df[-c(nrow(df), nrow(df)-1), ])
colnames(df)[1] = 'line'
df$line <- as.character(trimws(df$line, which= 'both'))

install.packages("sentimentr")
library(sentimentr)
sentiment(head(df$line,5))
print(head(df$line,5))

sentence <- c()
for (line in df$line) {
  tmp <- get_sentences(line)
  for(i in 1:length(tmp[[1]])) {
    sentence_tmp <- tmp[[1]][i]
    sentence <- c(sentence, sentence_tmp)
  }
}
print(sentence)
df_sentr <- data.frame(sentence)
df_sentr$sentence <- as.character(df_sentr$sentence)

sentiment <- sentiment(df_sentr$sentence)
s<- cbind(df_sentr, sentiment)

df_sentr$sentiment <- as.numeric(sentiment$sentiment)

df_sentr$pntag <- ifelse(sentiment$sentiment == 0, 'Neutral',
                         ifelse(sentiment$sentiment > 0, 'Positive',
                                ifelse(sentiment$sentiment < 0, 'Negative', 'NA')))

df_sentr$element_id<- s$element_id
ggplot()+geom_density(aes(df_sentr$sentiment))

install.packages("plotly")
install.packages("magrittr")

# base R plot
plot(df_sentr$sentiment, type='l', pch=3)


ax <- list(
  title = "Sentence",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
)



plot_ly(data = df_sentr, x = ~sentence, y = ~sentiment, color = ~pntag,
        type = 'scatter', mode = 'markers') %>% layout(xaxis = ax)
plot_ly(data = df_sentr, y = ~sentiment, color = ~pntag,
        type = 'scatter', mode = 'markers') %>% layout(xaxis = ax)

plot_ly(data = df_sentr,x= ~element_id, y = ~sentiment, color = ~pntag,
        type = 'scatter', mode = 'markers') %>% layout(xaxis = ax)


df_sentr$sentence[which.max(df_sentr$sentiment)]
df_sentr$sentence[which.min(df_sentr$sentiment)]

# Get average
mean(df_sentr$sentiment)

# Check positive and negative sentences
check_pos <- subset(df_sentr, df_sentr$sentiment >= 0.1)
check_pos <- check_pos[order(check_pos$sentiment, decreasing = T),]
write.csv(check_pos, "positive.csv")
check_neg <- subset(df_sentr, df_sentr$sentiment <= -0.1)
check_neg <- check_neg[order(check_neg$sentiment, decreasing = F),]
write.csv(check_neg, "negative.csv")


