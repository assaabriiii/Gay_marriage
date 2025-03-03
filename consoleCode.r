library(socsci)
library(haven)

sbc <- gss %>% 
  mutate(abort = recode(abany, "1=1; 2=0; else = 99")) %>% 
  # filter(evangelical ==1) %>% 
  group_by(year) %>% 
  filter(abort != 99) %>% 
  mean_ci(abort)



ggplot(sbc, aes(x=year, y=mean)) + 
  geom_line(size  = 1) +
  geom_point(colour="cornflowerblue", size=2, stroke=2)+
  geom_smooth() +
  theme_minimal() + 
  theme_gg("Abel") +
  scale_y_continuous(limits=c(.28,.55), labels = scales::percent) +
  labs(x = "Year", y = "Percent in Favor", title = "Allow An Abortion If The Woman Wants It For Any Reason", caption = "Data: GSS (1977-2016)") +
  ggsave("D://abort_gss_overall.png", width = 8, height = 6)



## Gay Marriage Reltrad Bars ####


gay <- gss %>% 
  filter(marhomo != "NA") %>%
  filter(year == 2016) %>% 
  mutate(reltrad = frcode(reltrad == 1 ~ "Evangelicals", 
                          reltrad == 2 ~ "Black Protestant",
                          reltrad == 3 ~ "Mainline", 
                          reltrad == 4 ~ "Catholic", 
                          reltrad == 5 ~ "Jewish", 
                          reltrad == 6 ~ "Other Faith",
                          reltrad == 7 ~ "No Faith")) %>% 
  group_by(reltrad) %>% 
  ct(marhomo) %>% 
  ungroup(reltrad) %>% 
  mutate(gaym = frcode(marhomo == 5 ~ "Strongly disagree",
                       marhomo == 4 ~ "Disagree",
                       marhomo == 3 ~ "Neither Agree or Disagree",
                       marhomo == 2 ~ "Agree",
                       marhomo == 1 ~ "Strongly agree",
                       TRUE ~ "REMOVE"))  
  

gay %>% 
  filter(reltrad != "NA") %>% 
  ggplot(., aes(x=gaym, y = pct, fill = reltrad)) +
  geom_col(color = "black") +
  facet_wrap(~ reltrad, ncol = 4) +
  theme_gg("Abel") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust =1, size = 24)) +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  scale_fill_paletteer_d(ggthemes, calc) +
  labs(x = "Homosexual couples should have the right to marry one another", y = "", title = "Support for Gay Marriage by Religious Tradition", caption = "Data: GSS 2016") +
  ggsave("D://gay_m_reltrad.png")

## Two generations ribbons ####

ggg1 <- gss %>% 
  filter(marhomo != "NA") %>%
  mutate(gaym = recode(marhomo, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  filter(evangelical == 1) %>% 
  filter(race ==1) %>% 
  mutate(age2 = recode(age, "18:35 = '35 and Under'; 36:100 = 'Over 35'")) %>% 
  group_by(year, age2) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "White Evangelicals") %>% 
  na.omit()

ggg2 <- gss %>% 
  filter(marhomo != "NA") %>%
  mutate(gaym = recode(marhomo, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(age2 = recode(age, "18:35 = '35 and Under'; 36:100 = 'Over 35'")) %>% 
  group_by(year, age2) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "Entire Sample") %>% 
  na.omit()


gss %>% 
  filter(marhomo != "NA") %>%
  mutate(gaym = recode(marhomo, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  group_by(year) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "Entire Sample") %>% 
  na.omit()

graph <- bind_df("ggg")

graph <- graph %>% 
  mutate(g2 = paste(age2, group, sep = " - "))

graph %>% 
  filter(year > 2000) %>% 
  ggplot(., aes(x = year, y = mean, group = g2, color = g2)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = g2, fill = g2), alpha = .4, show.legend = FALSE) +
  theme_gg("Abel") +
  scale_y_continuous(breaks = c(1,2,3,4,5), labels = c("Strongly Disagree", "Disagree", "Neither Agr. or Dis.", "Agree", "Strongly Agree")) +
  theme(legend.position = c(0.8, 0.1)) +
  scale_fill_paletteer_d(ggthemes, calc) +
  scale_color_paletteer_d(ggthemes, calc) +
  labs(x = "Church Attendance", y = "Support for Gay Marriage", title = "Younger Evangelicals are Rapidly Accepting SSM", caption = "Data: GSS 2004-2016") +
  ggsave("D://gay_2ages.png", width = 8)

### Attendance Ribbons ####


aaa1 <- cces16 %>% 
  filter(evangelical ==1) %>% 
  filter(race ==1) %>% 
  mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
  mutate(attend = frcode(pew_churatd == 6 ~ "Never",
                         pew_churatd == 5 ~ "Seldom", 
                         pew_churatd == 4 ~ "Yearly",
                         pew_churatd == 3 ~ "Monthly", 
                         pew_churatd == 2 ~ "Weekly", 
                         pew_churatd == 1 ~ "Weekly+", 
                         TRUE ~ "REMOVE")) %>% 
  group_by(attend) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "White Evangelicals")

aaa2 <- cces16 %>% 
  filter(catholic ==1) %>% 
  mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
  mutate(attend = frcode(pew_churatd == 6 ~ "Never",
                         pew_churatd == 5 ~ "Seldom", 
                         pew_churatd == 4 ~ "Yearly",
                         pew_churatd == 3 ~ "Monthly", 
                         pew_churatd == 2 ~ "Weekly", 
                         pew_churatd == 1 ~ "Weekly+", 
                         TRUE ~ "REMOVE")) %>% 
  group_by(attend) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "Catholics")

aaa3 <- cces16 %>% 
  filter(mainline ==1) %>% 
  mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
  mutate(attend = frcode(pew_churatd == 6 ~ "Never",
                         pew_churatd == 5 ~ "Seldom", 
                         pew_churatd == 4 ~ "Yearly",
                         pew_churatd == 3 ~ "Monthly", 
                         pew_churatd == 2 ~ "Weekly", 
                         pew_churatd == 1 ~ "Weekly+", 
                         TRUE ~ "REMOVE")) %>% 
  group_by(attend) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "Mainline")


aaa4 <- cces16 %>% 
  mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
  mutate(attend = frcode(pew_churatd == 6 ~ "Never",
                         pew_churatd == 5 ~ "Seldom", 
                         pew_churatd == 4 ~ "Yearly",
                         pew_churatd == 3 ~ "Monthly", 
                         pew_churatd == 2 ~ "Weekly", 
                         pew_churatd == 1 ~ "Weekly+", 
                         TRUE ~ "REMOVE")) %>% 
  group_by(attend) %>% 
  mean_ci(gaym) %>% 
  mutate(group = "Entire Sample")

graph <- bind_df("aaa")

graph %>% 
  filter(attend != "REMOVE") %>% 
  ggplot(., aes(x= attend, y = mean, group = group, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = group, fill = group), alpha = .4, show.legend = FALSE) +
  theme_gg("Abel") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = c(0.4, 0.2)) +
  scale_fill_paletteer_d(ggthemes, calc) +
  scale_color_paletteer_d(ggthemes, calc) +
  labs(x = "Church Attendance", y = "Support for Gay Marriage", title = "Frequent Church Attendance is Related to Lower Support for SSM", caption = "Data: CCES 2016") +
  ggsave("D://gay_attend_reltrad.png", width = 8)


## Dumbbells of Support Changes ####

fun <- function(df, relig, fam){
  relig <- enquo(relig)
  fam <- enquo(fam)
  
  df %>% 
    group_by(!! relig) %>% 
    mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
    mean_ci(gaym) %>% 
    filter(n > 100) %>% 
    mutate(relig = to_factor(!! relig)) %>% 
    filter(relig != "Skipped") %>% 
    select(relig, mean, sd, n, level, se, lower, upper) %>% 
    mutate(family = !! fam)
  
  
}

aa <- fun(cces16, religpew_methodist, "Methodist")
aa1 <- fun(cces16, religpew_baptist, "Baptist")
aa2 <- fun(cces16, religpew_nondenom, "Non-Denom")
aa3 <- fun(cces16, religpew_lutheran, "Lutheran")
aa4 <- fun(cces16, religpew_presby, "Presbyterian")
aa5 <- fun(cces16, religpew_pentecost, "Pentecostal")
aa6 <- fun(cces16, religpew_episcop, "Episcopal")
aa7 <- fun(cces16, religpew_congreg, "Congregational")
aa8 <- fun(cces16, religpew_holiness, "Holiness")
aa9 <- fun(cces16, religpew_reformed, "Reformed")

aa10 <- cces16 %>% 
  group_by(religpew) %>% 
  mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
  mean_ci(gaym) %>% 
  filter(n > 100) %>% 
  mutate(relig = to_factor(religpew)) %>% 
  filter(relig != "Protestant") %>% 
  select(-religpew)

aa10$family <- c("Catholic", "Mormon", "Orthodox", "Jewish", "Other Religion", "Other Religion", "Other Religion", "None", "None", "None", "Other Religion") 


g16 <- bind_df("aa") %>% 
  rename(mean16 = mean) %>% 
  select(relig, mean16, family)


cces14 <- read_dta("D://cces/data/cces14.dta")

fun <- function(df, relig, fam){
  relig <- enquo(relig)
  fam <- enquo(fam)
  
  df %>% 
    group_by(!! relig) %>% 
    mutate(gaym = recode(CC14_327, "1=1; 2=0; else = NA")) %>% 
    mean_ci(gaym) %>% 
    filter(n > 100) %>% 
    mutate(relig = to_factor(!! relig)) %>% 
    filter(relig != "Skipped") %>% 
    select(relig, mean, sd, n, level, se, lower, upper) %>% 
    mutate(family = !! fam)
  
  
}


aa <- fun(cces14, religpew_methodist, "Methodist")
aa1 <- fun(cces14, religpew_baptist, "Baptist")
aa2 <- fun(cces14, religpew_nondenom, "Non-Denom")
aa3 <- fun(cces14, religpew_lutheran, "Lutheran")
aa4 <- fun(cces14, religpew_presby, "Presbyterian")
aa5 <- fun(cces14, religpew_pentecost, "Pentecostal")
aa6 <- fun(cces14, religpew_episcop, "Episcopal")
aa7 <- fun(cces14, religpew_congreg, "Congregational")
aa8 <- fun(cces14, religpew_holiness, "Holiness")
aa9 <- fun(cces14, religpew_reformed, "Reformed")


aa10 <- cces14 %>% 
  group_by(religpew) %>% 
  mutate(gaym = recode(CC14_327, "1=1; 2=0; else = NA")) %>% 
  mean_ci(gaym) %>% 
  filter(n > 100) %>% 
  mutate(relig = to_factor(religpew)) %>% 
  filter(relig != "Protestant") %>% 
  select(-religpew)

aa10$family <- c("Catholic", "Mormon", "Orthodox", "Jewish", "Other Religion", "Other Religion", "Other Religion", "None", "None", "None", "Other Religion") 

g14 <- bind_df("aa")

g14 <- g14 %>% 
  rename(mean14 = mean) %>% 
  select(relig, mean14, family)



cces12 <- read_dta("D://cces/data/cces12.dta")

fun <- function(df, relig, fam){
  relig <- enquo(relig)
  fam <- enquo(fam)
  
  df %>% 
    group_by(!! relig) %>% 
    mutate(gaym = recode(CC326, "1=1; 2=0; else = NA")) %>% 
    mean_ci(gaym) %>% 
    filter(n > 100) %>% 
    mutate(relig = to_factor(!! relig)) %>% 
    filter(relig != "Skipped") %>% 
    select(relig, mean, sd, n, level, se, lower, upper) %>% 
    mutate(family = !! fam)
  
  
}


aa <- fun(cces12, religpew_methodist, "Methodist")
aa1 <- fun(cces12, religpew_baptist, "Baptist")
aa2 <- fun(cces12, religpew_nondenom, "Non-Denom")
aa3 <- fun(cces12, religpew_lutheran, "Lutheran")
aa4 <- fun(cces12, religpew_presby, "Presbyterian")
aa5 <- fun(cces12, religpew_pentecost, "Pentecostal")
aa6 <- fun(cces12, religpew_episcop, "Episcopal")
aa7 <- fun(cces12, religpew_congreg, "Congregational")
aa8 <- fun(cces12, religpew_holiness, "Holiness")
aa9 <- fun(cces12, religpew_reformed, "Reformed")


aa10 <- cces12 %>% 
  group_by(religpew) %>% 
  mutate(gaym = recode(CC326, "1=1; 2=0; else = NA")) %>% 
  mean_ci(gaym) %>% 
  filter(n > 100) %>% 
  mutate(relig = to_factor(religpew)) %>% 
  filter(relig != "Protestant") %>% 
  select(-religpew)

aa10$family <- c("Catholic", "Mormon", "Orthodox", "Jewish", "Other Religion", "Other Religion",  "None", "None", "None", "Other Religion") 

g12 <- bind_df("aa")

g12 <- g12 %>% 
  rename(mean12 = mean) %>% 
  select(relig, mean12, family)



graph <- left_join(g12, g16) %>% 
  mutate(diff = mean16 - mean12)

font_add_google("Abel", "font")
showtext_auto()

graph <- graph %>% 
  mutate(diff2 = round(diff, 3)) %>% 
  mutate(diff2 = diff2 * 100) %>% 
  mutate(diff2 = paste(diff2, "%", sep = ""))

graph %>% 
  ggplot(., aes(x = mean12, xend = mean16, y = reorder(relig, mean16))) +
  geom_dumbbell(colour_x = "firebrick2", colour_xend = "darkorchid",  size = .75, size_x =  2.75, size_xend = 2.75 ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  labs(x= "Percent in Favor", y = "Religious Tradition", title = "Change in Support for Gay Marriage from 2012 to 2016", caption = "Data: CCES 2012 and 2016", subtitle = "")+
  theme(text=element_text(size=64, family="font")) +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(plot.title = element_text(face= "bold", size = 66)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) + 
  geom_vline(xintercept = .521, color = "firebrick2", linetype = "dashed") +
  geom_vline(xintercept = .651, color = "darkorchid", linetype = "dashed") +
  annotate("text", x=.521, y = 44, label = "Avg.", size = 16, family = "font") +
  annotate("text", x=.521, y = 43, label = "2012", size = 16, family = "font") +
  annotate("text", x=.651, y = 44, label = "Avg.", size = 16, family = "font") +
  annotate("text", x=.651, y = 43, label = "2016", size = 16, family = "font") +
  geom_text(data = filter(graph, relig == "Anglican Church (Church of England)"), aes(x= mean12, y = relig, label = "2012"), vjust = -.75, family = "font", size = 16) +
  geom_text(data = filter(graph, relig == "Anglican Church (Church of England)"), aes(x= mean16, y = relig, label = "2016"), vjust = -.75, family = "font", size = 16) +
  geom_rect(data=graph, aes(xmin=1, xmax=1.1, ymin=-Inf, ymax=Inf), fill="gray") +
  geom_text(data=graph, aes(label=diff2, y=relig, x=1.05), fontface="bold", size=13, family="font") +
  geom_text(data=filter(graph, relig=="Atheist"), aes(x=1.05, y=relig, label=""), size=10, fontface="bold", family="font", vjust = -.65) +
  ggsave("D://cces/images/gay_m_moves12.png", height = 10, width = 12)


cces12 %>% 
  mutate(gaym = recode(CC326, "1=1; 2=0; else = NA")) %>% 
  mean_ci(gaym)

cces16 %>% 
  mutate(gaym = recode(CC16_335, "1=1; 2=0; else = NA")) %>% 
  mean_ci(gaym)
