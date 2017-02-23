########################
### DATA DOWNLOADING ###
########################
# source("~/R/DFootprints/wallpostSQL.R")
# load("~/R/DFootprints/wallpost.RData")
load("~/Google Drive/Digital Footprints/DFootprints/wallpost.RData")


#####################
### DATA CLEANING ###
#####################
library(lubridate)
library(plyr)
library(dplyr)

# RENAMING
wp <- rename(wp,
        posts = count,
        edu = level_of_education,
        birth = year_of_birth,
        id = participant_id,
        munic = name
        )
wp$using_real_name_on_fb <- NULL

# TIME
wp$time <- wp$month %>%
    paste(1, sep=" - ") %>%     # appending day to month (1st)
    ymd()                       # converting to date-format using lubridate
wp$month <- NULL

# BIRTH
wp$birth <- ymd(wp$birth)

# GENDER
wp$gender <- factor(wp$gender, levels = c("MALE", "FEMALE"))  # factorizing
wp$gender <- revalue(wp$gender, c("MALE"="Male", "FEMALE"="Female"))   # relabelling

# EDUCATION
wp$edu <- factor(wp$edu, levels = c("SHORT", "MEDIUM", "LONG"))  # factorizing
class(wp$edu)
wp$edu <- revalue(wp$edu, c(
                    "SHORT"="Short", 
                    "MEDIUM"="Medium",
                    "LONG"="Long"))     # relabelling

# MUNICIPALITIES
Encoding(wp$munic) <- "latin1"   # encoding charset to support ÆØÅ
wp$munic <- factor(wp$munic)  # factorizing

## REORDERING COLUMNS
wp <- wp[c("id", "time", "posts","birth","gender","edu","munic")]

## FILLING TIME SERIES
wp <- merge(wp, expand.grid(id = unique(wp$id),
                              time = unique(wp$time)),
             all.y=TRUE)

wp$posts[is.na(wp$posts)] <- 0 # replacing NA with 0 for "posts"
wp <- ddply(wp, .(id), zoo:::na.locf) # replacing NA with prior observation by ID
wp <- na.omit(wp) # removing rows with NA (yet unenrolled participants)


## CREATING NEW VARIABLES

# REGIONS
hovedstaden <- c("Gribskov","Helsingør","Fredensborg","Hillerød","Halsnæs",
                 "Frederikssund","Allerød","Hørsholm","Rudersdal","Furesø",
                 "Egedal","Ballerup","Herlev","Gladsaxe","Gentofte","Lyngby-Taarbæk",
                 "Høje-Taastrup","Albertslund","Glostrup","Rødovre","Brøndby",
                 "Hvidovre","Vallensbæk","Ishøj","Tårnby","København","Frederiksberg",
                 "Bornholm","Dragør")   # Dragør NA

sjælland <- c("Lolland","Guldborgsund","Vordingborg","Stevns","Faxe","Næstved",
              "Slagelse","Sorø","Ringsted","Køge","Kalundborg","Odsherred","Holbæk",
              "Lejre","Roskilde","Solrød","Greve")

nordjylland <- c("Hjørring","Frederikshavn","Brønderslev","Jammerbugt","Aalborg",
                 "Thisted","Morsø","Vesthimmerland","Rebild","Mariagerfjord","Læsø")

midtjylland <- c("Lemvig","Struer","Skive","Holstebro","Viborg","Randers",
                 "Norddjurs","Syddjurs","Favrskov","Silkeborg","Herning",
                 "Ringkøbing-Skjern","Ikast-Brande","Aarhus","Skanderborg",
                 "Odder","Horsens","Samsø","Hedensted")     # Hedensted NA

syddanmark <- c("Varde","Billund","Vejle","Esbjerg","Vejen","Kolding","Haderslev",
                "Fredericia","Tønder","Aabenraa","Sønderborg","Ærø","Svendborg",
                "Faaborg-Midtfyn","Nyborg","Odense","Kerteminde","Nordfyn",
                "Middelfart","Assens","Langeland","Fanø")   # Fanø NA, Langeland NA

wp$region[wp$munic %in% syddanmark] <- "Syddanmark"
wp$region[wp$munic %in% midtjylland] <- "Midtjylland"
wp$region[wp$munic %in% nordjylland] <- "Nordjylland"
wp$region[wp$munic %in% sjælland] <- "Sjælland"
wp$region[wp$munic %in% hovedstaden] <- "Hovedstaden"
wp$region <- factor(wp$region)  # factorizing
rm(nordjylland, midtjylland, syddanmark, sjælland, hovedstaden)

# URBAN
library(car)
urban <- c("Aalborg", "Aarhus", "Odense", "Frederiksberg", "København")
wp$urban <- recode(wp$munic, "urban='Urban'; else='Rural'") # recoding
wp$urban <- factor(wp$urban)    # factorizing
rm(urban)

# AGE
wp$age <- difftime(wp$time, wp$birth, units="days")/365.25 
wp$age <- trunc(wp$age) %>% 
    as.integer() # removing decimals

# AGE GROUPS
wp$agecat <- wp$age %>%
    cut(breaks=c(14, 30, 45, 60, Inf),
        labels=c("15-29", "30-44", "45-59", "60+"))

## FORMATTING
wp$id <- as.integer(wp$id)
wp$time <- ymd(wp$time)
wp$posts <- as.integer(wp$posts)
wp$birth <- ymd(wp$birth)
wp$gender <- as.factor(wp$gender)
wp$edu <- as.factor(wp$edu)
wp$munic <- as.factor(wp$munic)
wp$age <- as.integer(wp$age)
wp <- arrange(wp, id, time) %>% 
    .[,c(2,1,3:13)]


##################
### REGRESSION ###
##################
library(plm)
library(pglm)

# Setting panel data
pwp <- pdata.frame(wp, c("id", "time"))

# Adding variables
pwp$month <- as.numeric(pwp$time)

## LINEAR MODEL
# OLS
pooling <- plm(posts ~ month, data=pwp, model="pooling")
# FIXED
fixed <- plm(posts ~ month, data=pwp, model="within")
# RANDOM
random <- plm(posts ~ month, data=pwp, model="random")

## DIAGNOSTICS (PLM.pdf, p. 26ff)
# Test of poolability (same coefficients apply to each individual)
pooltest(pooling, fixed) # sig => coefficients not equal

# Tests for individual and time effects 
# Lagrange multiplier test (RE vs OLS)
plmtest(pooling, effect="twoways", type="ghm") # sig => use random effects
# F test for individual effects (FE vs OLS)
pFtest(fixed, pooling) # sig => use fixed effects
# Hausman test (FE vs RE)
phtest(random, fixed) # sig => use fixed effects (RE is inconsistent)

# Linear model = fixed effects
summary(fixed)

## GENERALIZED LINEAR MODEL
pfixed <- pglm(posts ~ month, 
               data=pwp, 
               model="within", 
               family="poisson")
summary(pfixed)
exp(summary(pfixed)$est)

pfixed2 <- pglm(posts ~ month + gender + urban + agecat, 
                data=pwp, 
                model="within", 
                family="poisson")
summary(pfixed2)
exp(summary(pfixed2)$est)

#####################
### VISUALIZATION ###
#####################
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(zoo)
library(reshape2)

# Monthly posts
wpost1 <- aggregate(posts ~ as.yearmon(time) + id, data=wp, sum)
wpost1$time <- mdy(wpost1$`as.yearmon(time)`)
wpost1$`as.yearmon(time)` <- NULL

# Plot 1: Overview
plot1 <- ggplot(wpost1, aes(x=time, y=posts)) +
    geom_point(alpha=0.5) +          # color = tid siden først signup
    geom_smooth(method="glm", family="poisson") +
    geom_smooth(method="glm") +
    xlab("Date (monthly)") +
    ylab("Wallposts (count)") +
    ggtitle("Monthly wallposts over time") +
    theme_stata(scheme = "s2color", base_size = 11, base_family = "sans")
plot1


# By demographics
wpost2 <- aggregate(posts ~ as.yearmon(time) + gender + region + 
                        urban + age + agecat, data=wp, sum)
wpost2$time <- mdy(wpost2$`as.yearmon(time)`)
wpost2$`as.yearmon(time)` <- NULL

# Plot 2a: Age continuous
plot2a <- ggplot(wpost2, aes(x=time, y=posts)) +
    geom_point(aes(color=as.integer(age))) +
    scale_color_gradient(low="green3", high="purple", name="Age") +
#     stat_smooth(method="glm", 
#                 data = wpost2,
#                 family="poisson", 
#                 formula= y ~ x,
#                 color="blue") +
#     stat_smooth(method="glm", 
#                 data = wpost2,
#                 family="gaussian",
#                 formula= y ~ x,
#                 color="red") +
    facet_wrap(gender ~ urban) +
    xlab("Date (month)") +
    ylab("Wallposts (count)") +
    ggtitle("Monthly wallposts") +    # + ylim(0,50)
    theme_stata(scheme = "s2color", base_size = 11, base_family = "sans")
plot2a

# Plot 2b: Age discrete
plot2b <- ggplot(wpost2, aes(x=time, y=posts)) +
    geom_point(aes(color=agecat), alpha=0.5) +
#     stat_smooth(method="glm", 
#                 family="poisson", 
#                 color="blue") +
    facet_wrap(gender ~ urban) +
    xlab("Date (month)") +
    ylab("Wallposts (count)") +
    ggtitle("Monthly wallposts") +
    scale_color_discrete(name="Age") +
    # theme(legend.position = "top")
    theme_stata(scheme = "s2color", base_size = 11, base_family = "sans")
plot2b



# plotting data
dates <- seq(min(wp$time), max(wp$time), by="month") %>% 
    rep(.,each=5) %>% 
    data.frame(time=., agecat=rep(c(NA,"15-29","30-44","45-59","60+"), length(.)/5))

# potential users by age group
pusers <- wp[,c("time","agecat","posts")] %>% 
    cbind(., pusers=1) %>%
    as.data.frame() %>% 
    group_by(time, agecat) %>% 
    summarize(posts=sum(posts), pusers=sum(pusers)) %>% 
    arrange(., agecat, time)

# active users by age group
ausers <- wp[,c("id", "time","posts","agecat")] %>% 
    filter(posts > 0) %>% 
    group_by(time, agecat) %>% 
    summarize(ausers=n_distinct(id)) %>% 
    arrange(., time, agecat)

df <- merge(dates, pusers, by=c("time", "agecat"), all=TRUE) %>%
    merge(., ausers, by=c("time","agecat"), all=TRUE) %>% 
    arrange(., agecat, time) 

df$posts[is.na(df$posts)] <- 0
df$pusers[is.na(df$pusers)] <- 0
df$ausers[is.na(df$ausers)] <- 0

# posts per potential user by age group
df$ppp <- df$posts/df$pusers

# Plot 3: Potential and active users by age group over time (+ posts)
df %>% 
    na.omit() %>% # exclude <15 agecat
    select(time, agecat, pusers, ausers, ppp) %>%
    mutate(ppp=ppp*10) %>% # scaling 2nd y-axis
    melt(id=c("time", "agecat")) %>%  # to long format
    ggplot(., aes(x=time, y=value, group=variable, color=variable)) +
    geom_line() +
    facet_wrap(~agecat) +
    scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "Posts")) + # scaling 2nd y-axis
    scale_color_discrete(name="", 
                         labels=c("Potential users", "Active users", "Posts per potential user")) +
    labs(x="Date", y="Users") +
    theme(legend.position="bottom") +
    theme_stata(scheme = "s2color", base_size = 11, base_family = "sans")


# Plot 4: Share active users of potential users by age group over time
df %>% 
    na.omit() %>% 
    select(time, agecat, pusers, ausers) %>% 
    mutate(apshare= ausers/pusers*100) %>% 
    select(time, agecat, apshare) %>% 
    ggplot(., aes(x=time, y=apshare, group=agecat, color=agecat)) +
    geom_line() +
    scale_color_discrete(name="") +
    labs(x="Date", y="Active of potential users (%)") +
    theme(legend.position="bottom") +
    theme_stata(scheme="s2color", base_size=11, base_family="sans")
    

# Plot 5: Share of total posts over time by percentile most active users
# data to plot
wp %>% 
    select(time, posts) %>% 
    filter(time=="2007-05-01") %>% 
    arrange(time,desc(posts)) %>% 
    group_by(time) %>% 
    mutate(share = posts/sum(posts)*100, 
           n = n()) %>% 
    mutate(cshare = cumsum(share)) %>% 
    arrange(time, desc(posts), desc(cshare)) %>% 
    mutate(p1 = cshare[ceiling(floor(n)*0.01)])

perc <- wp %>% 
    select(time, posts) %>% 
    arrange(time, desc(posts)) %>% 
    group_by(time) %>% 
    mutate(# csum = cumsum(posts),
           share = posts/sum(posts)*100,
           n = n()) %>% 
    mutate(cshare = cumsum(share)) %>% 
    mutate(p1 = cshare[ceiling(floor(n)*0.01)], # % of content produced by 1% most active
           p2_5 = cshare[ceiling(floor(n)*0.025)], # ... 2.5% most active
           p5 = cshare[ceiling(floor(n)*0.05)], # ... 5% most active
           p10 = cshare[ceiling(floor(n)*0.1)], # ... 10% most active etc.
           p20 = cshare[ceiling(floor(n)*0.2)],
           p25 = cshare[ceiling(floor(n)*0.25)],
           p30 = cshare[ceiling(floor(n)*0.30)],
           p40 = cshare[ceiling(floor(n)*0.40)],
           p50 = cshare[ceiling(floor(n)*0.50)],
           p75 = cshare[ceiling(floor(n)*0.75)],
           p100 = cshare[ceiling(floor(n)*1.00)])

# simple line plot
perc %>% 
    select(-c(posts, share, n, cshare)) %>% 
    unique() %>% 
    melt(id="time") %>% 
    ggplot(., aes(x=time, y=value, group=variable, color=variable)) + 
    geom_line() +
    labs(x = "Date", y = "Share of total wallposts (%)") +
    scale_color_brewer(name="", 
                       palette="Paired",
                       labels=c("1%", "2.5%", "5%", "10%", "20%", "25%", "30%", 
                                "40%", "50%", "75%", "100%"),
                       guide=guide_legend(reverse=TRUE))


# Mean monthly wallposts by demographic
wp %>% 
    select(posts, gender, urban, agecat) %>% 
    group_by(gender, urban, agecat) %>% 
    summarize(mean_posts=mean(posts)) %>% 
    na.omit() %>% 
    ggplot(., aes(x=agecat, y=mean_posts, fill=agecat)) +
    geom_bar(stat="identity") +
    facet_wrap(gender~urban) +
    labs(x="", y="Average monthly wallposts") +
    scale_fill_brewer(name="",
                      palette="Set1")

