library("tidyverse")

drwho_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv')
drwho_directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_directors.csv')
drwho_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_writers.csv')

drwho_episodes<-drwho_episodes[,-c(1)] #removed era column cause its not needed
summary(drwho_episodes$rating) #for finding the statistics of rating of the episodes 
summary(drwho_episodes$uk_viewers) #for finding the statistics of no of viewers
summary(drwho_episodes$duration) #for finding the statistics of duration of episodes

drwho_episodes_splits<-split(drwho_episodes,drwho_episodes$type)
count_episode<-0 #for determining the no of episodes which were of type "episode"
count_special<-0

for(i in drwho_episodes_splits){
  if(i$type[1]=="episode"){
    count_episode<-count(i)$n
  }
  else{
    count_special<-count(i)$n
  }
}

temp<-drwho_episodes$first_aired #dates of airing
viewer_count<-drwho_episodes$uk_viewers # no of viewers

plot(temp,viewer_count,type = "l",col="red",main="date aired vs viewer count",xlab="dates",ylab="viewers (in millions)",cex=0.3,xlim=c(min(temp),max(temp)))

temp_2<-c()
for(i in drwho_writers$story_number){
  len<-length(temp_2)
  temp_2[len+1]<-filter(drwho_episodes,drwho_episodes$story_number==i)$uk_viewers
}
drwho_writers<-cbind(drwho_writers,uk_viewers=temp_2)
drwho_writers_split<-drwho_writers %>% group_by(drwho_writers$writer) %>% 
