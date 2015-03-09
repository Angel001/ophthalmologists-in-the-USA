##### File downloaded from https://data.medicare.gov/data/physician-compare
##### Unzip and place Physician_Compare_Databases/National_Downloadable_File.csv
##### in working directory

library(dplyr)
library(ggplot2)
library(zipcode)
library(ggmap)
library(grid)

data(zipcode)
data0 <- read.csv("National_Downloadable_File.csv")
data <- select(data0, Primary.specialty, City, State, Zip.Code)
data <- filter(data, Primary.specialty %in% c("OPTOMETRY", "OPHTHALMOLOGY"))
data <- droplevels(data)
data$Zip.Code <- as.character(data$Zip.Code)
for (i in 1:nrow(data)) {
        while (nchar(data$Zip.Code[i]) < 5)
                data$Zip.Code[i] <- paste0("0", data$Zip.Code[i])
        while (nchar(data$Zip.Code[i]) > 5 & nchar(data$Zip.Code[i]) < 9) {
                data$Zip.Code[i] <- paste0("0", data$Zip.Code[i])
        }
}
data$Zip.Code <- substr(data$Zip.Code, 1,5)

df <- data %>% group_by(Zip.Code) %>% summarise(value = n())
df <- left_join(df, zipcode, by = c("Zip.Code" = "zip"))
df <- mutate(df, N = ifelse (value <= 10, "1 - 10", "Over 10"))

uscont <- get_map(location = c(left = -126, bottom = 24, right = -66, top = 50),
                  source = "stamen", maptype = "toner")
ak <- get_map(location = c(left = -168, bottom = 56, right = -145, top = 67),
              source = "stamen", maptype = "toner")
hi <- get_map(location = c(left = -161, bottom = 18, right = -154, top = 24),
              source = "stamen", maptype = "toner")

mytheme <- theme(axis.title = element_blank(),
                 axis.text = element_blank(),
                 legend.title = element_blank(),
                 legend.key = element_blank(),
                 legend.key.size = unit(2, "cm"),
                 legend.position = "bottom",
                 axis.ticks = element_blank()
)

ggmap(uscont) + geom_point(aes(x = longitude, y = latitude, color = N),
                           alpha = 0.3, data = df) +
        ggtitle("Number of optometrists / ophthalmologists per zipcode") +
        scale_colour_manual(values = c("deepskyblue", "red4")) +
        guides(colour = guide_legend(override.aes = list(size = 6))) +
        mytheme +
        theme(plot.margin = unit(c(2,0,0,0), "cm"),
              plot.title = element_text(face = "bold", vjust = 5),
              legend.text = element_text(size = 15)
        )

ggmap(ak) + geom_point(aes(x = longitude, y = latitude, color = N),
                       alpha = 0.3, data = df) +
        scale_colour_manual(values = c("deepskyblue", "red4")) +
        guides(colour = guide_legend(override.aes = list(size = 6))) +
        mytheme

ggmap(hi) + geom_point(aes(x = longitude, y = latitude, color = N),
                       alpha = 0.3, data = df) +
        scale_colour_manual(values = c("deepskyblue", "red4")) +
        guides(colour = guide_legend(override.aes = list(size = 6))) +
        mytheme
