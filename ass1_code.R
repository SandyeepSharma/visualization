library(gganimate)
library(plotly)
library(ggplot2)
library(dplyr)
library(viridis)
library(knitr)

library(rgdal)
library(tmap)


pollution = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/pollution_2016.csv')
env_satisfaction = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/env_satisfaction_2013.csv')
life_satisfaction = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/life_satisfaction_2013.csv')
politics = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/trust_in_politics_2013.csv')
income = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/median_income_2016.csv')
legal = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/trust_in_legal_2013.csv')
job_satisfaction = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/job_satisfaction_2013.csv')
budget = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/make_ends_meet_2016.csv')
crime = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/crime_2016.csv')
leisure_satisfaction = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/leisure_satisfaction_2013.csv')
underemployment = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/underemployment_2016.csv')
close_relations = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/close_relations_2015.csv')
low_savings = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/low_savings_2016.csv')
weather = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/weather.csv')
life_expectancy = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/life_expectancy_2016.csv')
unemployment = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/unemployment_2016.csv')
police = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/trust_in_police_2013.csv')
gdp = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/gdp_2016.csv')
health = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/perceived_health_2016.csv')
population = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/population_2011.csv')
work_hours = read.csv('C:\\College\\01_Subjects\\sem2\\visualisation\\assignment\\europe-datasets/work_hours_2016.csv')



MyMerge <- function(x, y){
  df <- merge(x, y, by= "country", all.x= TRUE, all.y= TRUE)
  return(df)
}
df <- Reduce(MyMerge, list(pollution,env_satisfaction,life_satisfaction,politics,income,legal,
                           job_satisfaction,budget,crime,leisure_satisfaction,underemployment,
                           close_relations,low_savings,weather,
                           life_expectancy,unemployment,police,gdp,health,
                           population,work_hours))


GDP_Job_Population <- ggplot(df, aes(x = gdp, y = prct_job_satis_high)) + 
  geom_point(aes(color = country, size = total_pop), alpha = 0.5) +
  #scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 20), name="Total Population") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  ylab("Job Satisfaction") +
  xlab("Gdp per Capita") +
  ggtitle("GDP vsJob vs Population")

ggplotly(GDP_Job_Population)



Income_Env_Population <- ggplot(df, aes(x = median_income, y = prct_env_satis_high)) + 
  geom_point(aes(color = country, size = total_pop), alpha = 0.5) +
  #scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 20), name="Total Population") +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  xlab("Income") +
  ylab("Environment Satisfaction") +
  ggtitle("Income vs Environment vs Population")

ggplotly(Income_Env_Population)


Life_Expectancy_crime_Population <- ggplot(df, aes(x = life_expect, y = prct_rpt_crime)) + 
  geom_point(aes(color = country, size = total_pop), alpha = 0.5) +
  scale_size(range = c(0.5, 20), name="Total Population") +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  xlab("Life Expectancy") +
  ylab("crime") +
  ggtitle("Life Expectancy vs Pollution")

ggplotly(Life_Expectancy_crime_Population)



workhours_satisfaction_Population <- ggplot(df, aes(x = avg_hrs_worked, y = prct_leisure_satis_high)) + 
  geom_point(aes(color = country, size = total_pop), alpha = 0.5) +
  scale_size(range = c(0.5, 20), name="Total Population") +
  xlab("Hours Worked") +
  ylab("Leisure Satisfaction") +
  ggtitle("Hours worked vs Leisure Satisfaction")

ggplotly(workhours_satisfaction_Population)



#reading shape file
europe_shp <- readOGR("C:\\Users\\sandy\\OneDrive\\Desktop\\ne_50m_admin_0_countries\\ne_50m_admin_0_countries.shp")

#subsetting data to make it for europe
europe_shp <- subset(europe_shp, is.element(europe_shp$NAME , df$country ))

#merging with data
spdf = merge(europe_shp ,df, by.y = "country", by.x = "NAME")



tmap_mode("view")
legend_title = expression("Unemployment Rate")

if (require(RColorBrewer)) {
  #pal <- brewer.pal(10, "Set2")[c(1, 8, 4, 5)]
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape1 <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'unemp_rate', palette= pal ,title = legend_title)+
    tm_fill("unemp_rate", title = "Unemployment Rate") +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1) +
    tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
  
  shape1
}

# barchart with added parameters
barplot(spdf$unemp_rate,
        main = "Unemployment in European Countries",
        ylab = "Unemployment Rate",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "red",
        las = 2)



legend_title = "political trust rating"

if (require(RColorBrewer)) {
  
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape2 <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'political_trust_rating',title = legend_title, palette= pal)+
    tm_fill( title = expression("political trust rating")) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)+
    tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3), 
             alpha = .7,    view.legend.position = c("left","bottom")) 
  
  shape2
}

# barchart with added parameters
barplot(spdf$political_trust_rating,
        main = "Political Trust in European Countries",
        ylab = "political trust rating",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "blue",
        las = 2)



#Good Health in European Countries

legend_title = expression("health percentage")

if (require(RColorBrewer)) {
  
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'prct_health_verygood',title = legend_title, palette= pal)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  # tm_layout(title = "Good Health in European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$prct_health_verygood,
        main = "Good Health in European Countries",
        ylab = "percentage of health",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "green",
        las = 2)


#Avg. Pollution in European Countries

legend_title = expression("Pollution")

if (require(RColorBrewer)) {
  
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'prct_rpt_pollution', palette= pal ,title = legend_title)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  # tm_layout(title = "Avg. Pollution in European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$prct_rpt_pollution,
        main = "Avg. Pollution in European Countries",
        ylab = "pollution",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "steelblue",
        las = 2)


#Avg. Temperature in European Countries

legend_title = expression("avg temperature")

if (require(RColorBrewer)) {
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'avg_temp', palette= pal,title = legend_title)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  #tm_layout(title = "Avg. Temperature in European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$avg_temp,
        main = "Avg. Temperature in European Countries",
        ylab = "avg temperature",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "orange",
        las = 2)


#Low Savings in European Countries

legend_title = expression("savings")


if (require(RColorBrewer)) {
  
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'prct_low_savings', palette= pal ,title = legend_title)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  #tm_layout(title = "Low Savings in European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$prct_low_savings,
        main = "Low Savings in European Countries",
        ylab = "savings",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "yellow",
        las = 2)



#High Job Satisfaction in European Countries

legend_title = expression("Job Satisfaction")


if (require(RColorBrewer)) {
  
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'prct_job_satis_high', palette= pal,title = legend_title)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  #tm_layout(title = "High Job Satisfaction in European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$prct_job_satis_high,
        main = "High Job Satisfaction in European Countries",
        ylab = "Job Satisfaction",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "blue",
        las = 2)



#GDP in European Countries

legend_title = expression("GDP (per Capita)")

if (require(RColorBrewer)) {
  #pal <- brewer.pal(10, "Set2")[c(1, 8, 4, 5)]
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'gdp', palette= pal,title = legend_title)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  # tm_layout(title = "GDP Of European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$gdp,
        main = "GDP in European Countries",
        ylab = "gdp per capita",cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "skyblue",
        las = 2)


#%age of Reported Crime in European Countries

legend_title = expression("Crime Reported")


if (require(RColorBrewer)) {
  #pal <- brewer.pal(10, "Set2")[c(1, 8, 4, 5)]
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'prct_rpt_crime',  palette= pal,title = legend_title)+
    tm_fill( title = legend_title) +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  # tm_layout(title = "%age of Reported Crime in European Countries")
  shape + tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
}

# barchart with added parameters
barplot(spdf$prct_rpt_crime,
        main = "%age of Reported Crime in European Countries",
        ylab = "Crime Reported", cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "brown",
        las = 2)



#population in European Countries
legend_title = expression("total population")

if (require(RColorBrewer)) {
  #pal <- brewer.pal(10, "Set2")[c(1, 8, 4, 5)]
  pal <- c('#ff4d4d','#ccebff','#ffff1a','#1aff1a')
  
  shape <- tm_shape(spdf)+
    tm_borders()+
    tm_polygons(col = 'total_pop', palette= pal,title = legend_title)+
    tm_fill( title = 'Total Population') +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  # tm_layout(main.title = "population in European Countries" )#+tm_text(text = 'total_pop')
  shape+ tm_view( set.zoom.limits = c(3,8), set.view = c(lon = 10, lat = 56, zoom = 3)) 
  
}

# barchart with added parameters
barplot(spdf$total_pop,
        main = "Population in European Countries",
        ylab = "total population", cex.axis = .7,
        names.arg = spdf$NAME, font.axis = 1, col.axis = "Red", las = 0,cex.names = .7,
        col = "steelblue",
        las = 2)