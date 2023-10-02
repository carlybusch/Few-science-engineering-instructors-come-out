### Study 1 in "Few science and engineering instructors come out to students, despite potential benefits"
###National survey of science and engineering instructors

library(dplyr)
library(stringr)
library(ggplot2)
library(ordinal)
library(nnet)

my_data <- read.csv("lgbtq2part_study1_deid_data.csv")
concealable_df <- read.csv("lgbtq2part_concealable_df.csv")

#set gay as ref group
concealable_df$identity_clean <- factor(concealable_df$identity_clean, 
                                        levels = c("bi", "gay", "queer+",
                                                   "nb", "trans"))
concealable_df$identity_clean <- relevel(concealable_df$identity_clean, ref = "gay")

### RQ1a: concealable percentages ----
table(my_data$lgbtq.gender.conceal)
table(my_data$lgbq.conceal)
table(concealable_df$concealable)

### RQ1b: logistic regression concealable ~ id ----
summary(glm(formula = concealable ~ identity_clean,
            data = concealable_df, family = "binomial"))

### RQ2: percent out by context ----
# Figure: bar graph out by context
extent_out_tbl <- read.csv("lgbtq2part_fig1_table.csv")

fig1 <- extent_out_tbl %>%
  filter(extent_out != "yes") %>%
  mutate(extent_out = factor(extent_out, levels = c("all", "some", "none"))) %>%
  mutate(context = factor(context, levels = c("colleauges", "lab", "grads", "ugs"))) %>%
  ggplot(aes(x = extent_out, y = liberal_pct/100)) + 
  geom_col(fill = "slategrey") +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  geom_text(aes(y = liberal_pct/100,
                label = scales::percent(liberal_pct/100, accuracy = .1), hjust = 1.1,
                fontface = "bold", size = 10), color = "white") +
  labs(y = "Percent of LGBTQ+ instructors", x = "Extent out to individuals in each context", legend = "") +
  coord_flip() +
  facet_wrap(.~context,
             labeller = as_labeller(c('colleauges' = "A. Colleagues", 'lab' = "B. Research lab",
                                      'grads' = "C. Graduate courses", 'ugs' = "D. Undergraduate courses"))) + 
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 10),
        axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        strip.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold", hjust = 0),
        strip.background = element_blank(),
        legend.position = "none")

fig1
#ggsave(filename = "lgbtq_2part_fig1.tiff", plot = fig1, units = "in", width = 8, height = 3.75)

### RQ3: reasons to reveal table -----

#vector of reasons
reasons_reveal<-c("I felt like I had a personal relationship with the students in the course",
                  "students in this course was appropriate",
                  "I typically share",
                  "relevant to the students in this course",
                  "relevant to the course content",
                  "I knew others in the department",
                  "live authentically or be open",
                  "I wanted to be an example to my students",
                  "I wanted to serve as a mentor",
                  "I wanted to be known as a supporter",
                  "could make me more relatable",
                  "could make students more comfortable",
                  "could help students understand me or my circumstances better",
                  "course material by making a connection")

#aggregate table
reveal_counts_agg <-do.call(rbind,
                            lapply(reasons_reveal,
                                   function(x){
                                     tmp<-as.data.frame(length(which(str_detect(my_data[["lgbtq.gender.reveal.why"]],x) | 
                                                                       str_detect(my_data[["lgbq.reveal.why"]],x) )))
                                     colnames(tmp) <- "count"
                                     tmp$reason<-x
                                     tmp$csi<-"aggregate"
                                     return(tmp)
                                   }))


reveal_counts_agg <- reveal_counts_agg %>% dplyr::select(csi, everything())

reveal_counts_agg$saw_question <- sum(length(which(my_data[["lgbtq.gender.reveal.why"]] != "")), length(which(my_data[["lgbq.reveal.why"]] != "")))
reveal_counts_agg$pct <- (reveal_counts_agg$count*100)/reveal_counts_agg$saw_question

### RQ5: percent who conceal and anticipate student benefits ----

table(my_data$lgbtq.gender.reveal.benefit, my_data$lgbq.reveal.benefit)

### RQ6: reasons to conceal table------

reasons_conceal <- c("I did not feel like I had a personal enough relationship with the students in this course",
                     "to all undergraduates in this course was inappropriate",
                     "I typically do not share",
                     "was relevant to the students in this course",
                     "was relevant to the course content",
                     "I did not know others in the department, such as other faculty or instructors, who had revealed a similar identity to people in the department",
                     "I had never thought about",
                     "I was concerned students would have a negative opinion", 
                     "result in poor course evaluations",
                     "I was concerned that I would be subjected to departmental disciplinary action",
                     "I was concerned I could be fired",
                     "would waste class time")

#aggregate table
conceal_counts_agg <-do.call(rbind,
                             lapply(reasons_conceal,
                                    function(x){
                                      tmp<-as.data.frame(length(which(str_detect(my_data[["lgbtq.gender.conceal.why"]],x) | 
                                                                        str_detect(my_data[["lgbq.conceal.why"]],x) )))
                                      colnames(tmp) <- "count"
                                      tmp$reason<-x
                                      tmp$csi<-"aggregate"
                                      return(tmp)
                                    }))


conceal_counts_agg <- conceal_counts_agg %>% dplyr::select(csi, everything())

conceal_counts_agg$saw_question <- sum(length(which(my_data[["lgbtq.gender.conceal.why"]] != "")), length(which(my_data[["lgbq.conceal.why"]] != "")))
conceal_counts_agg$pct <- (conceal_counts_agg$count*100)/conceal_counts_agg$saw_question

