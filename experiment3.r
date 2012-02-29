library(ggplot2)
library(reshape)
## creating the word cloud visualization
library(wordcloud)
library(RColorBrewer)

non.word.columns <- 1:3

treatment.ranks <- function(row, f) {
  as.integer(sub("X", "", colnames(sort(f[row, -(non.word.columns)]))))
}
word.ranks <- function(berry, earthy) {
  berry.order <- data.frame(berry=1:length(berry),word=berry)
  berry.order <- berry.order[with(berry.order, order(word)), ]
  earthy.order <- data.frame(earthy=1:length(earthy),word=earthy)
  earthy.order <- earthy.order[with(earthy.order, order(word)), ]

  data.frame(berry=berry.order$berry,earthy=earthy.order$earthy,word=words[earthy.order$word,1],color=words[earthy.order$word,3])
}
rank.chart <- function(ranks, title="Blah", filename="word-rank.pdf") {
  graph.df <- melt(ranks)
  ggplot(graph.df, aes(factor(variable), value, group = word, colour = color, label = word)) +
    geom_line() +
      geom_text(data = subset(graph.df, variable == "earthy"), aes(x = factor(variable)), size = 3.5, hjust = 0) +
        geom_text(data = subset(graph.df, variable == "berry"), aes(x = factor(variable)), size = 3.5, hjust = 1.0) +
          theme_bw() +
            opts(title=title, legend.position = "none", panel.border = theme_blank(), axis.ticks = theme_blank()) +
              scale_colour_identity() + 
                scale_x_discrete(breaks = levels(graph.df$variable), labels = c("berry", "earthy")) +
                  scale_y_continuous(breaks = NA, trans = "reverse") + xlab(NULL) + ylab(NULL)
  ggsave(filename)
}
output.cloud <- function(wine, treatment, d, filename="wordcloud.png") {
  crit <- d$wine == wine & d$treatment == treatment
  pal <- brewer.pal(8,"Dark2")
  png(filename, width=1280,height=800)
  wordcloud(d$variable[crit],d$value[crit],
            scale=c(8,.2),
            min.freq=3,
            max.words=Inf,
            random.order=FALSE,
            ordered.colors=TRUE,
            rot.per=.10,
            colors=as.character(words$color))
  dev.off()
}

experiment.three <- read.table("samples/survey_3.csv", header = TRUE, sep=",")
words <- read.table("samples/descriptors.csv", header = TRUE, sep=",")

exp.melt <- melt(experiment.three, id.vars = c("treatment", "wine"), fun=sum)
word.freq.table <- cast(wine + treatment ~ variable | value, data=exp.melt, fun.aggregate=sum)$`1`

wine.D.earthy.ranks <- treatment.ranks(1, word.freq.table)
wine.D.berry.ranks  <- treatment.ranks(4, word.freq.table)
wine.E.earthy.ranks <- treatment.ranks(2, word.freq.table)
wine.E.berry.ranks  <- treatment.ranks(3, word.freq.table)

by.treatment <- rowsum(experiment.three, experiment.three[,1])[,-(non.word.columns)]
by.wine <- rowsum(experiment.three, experiment.three[,2])[,-(non.word.columns)]


wine.D.ranks <- word.ranks(wine.D.berry.ranks, wine.D.earthy.ranks)
wine.E.ranks <- word.ranks(wine.E.berry.ranks, wine.E.earthy.ranks)

raw.words <- read.table("samples/survey_3.csv", header = TRUE, sep=",")[,-3]
colnames(raw.words) <- c(colnames(raw.words)[1:2],
                         as.character(words[as.integer(sub("X","",colnames(raw.words)[-(1:2)])),1]))
raw.words.melt <- melt(raw.words, id.vars = c("treatment", "wine"), fun=sum)
raw.words.melt.cast <- cast(wine + treatment ~ variable | value, data=raw.words.melt, fun.aggregate=sum)$`1`
word.freq.df <- melt(raw.words.melt.cast)

# charting the rank charts
rank.chart(wine.D.ranks, "Wine D Word Usage Rank Given Association", "word-rank-wine-d.pdf")
rank.chart(wine.E.ranks, "Wine E Word Usage Rank Given Association", "word-rank-wine-e.pdf")

#wine, treatment
output.cloud(1, 1, word.freq.df, "Wine D as Earthy.png")
output.cloud(1, 2, word.freq.df, "Wine E as Earthy.png")

output.cloud(2, 1, word.freq.df, "Wine E as Berry.png")
output.cloud(2, 2, word.freq.df, "Wine D as Berry.png")

# preference count visualizations
