#Open the datasets
data <- read.csv("~/Desktop/data.csv")
summary <- read.csv("~/Desktop/summary.csv")
#View the dataset
View(data)
View(summary)
print(data)
print(summary)
summary(data)
summary(summary)

#Open ggplot
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)

#Subset into groups
data.BelmontShore <- subset(data, Location == "Belmont Shore")
data.CabrilloBeachBay <- subset(data, Location == "Cabrillo Beach - Bay")
data.CabrilloBeachSZ <- subset (data, Location == "Cabrillo Beach - SZ")
data.MarinadelRey <- subset (data, Location == "Marina del Rey")
data.NaplesIsland <- subset (data, Location == "Naples Island")

#To test for normality and run a Shapiro-Wilk test on each group
shapiro.test(data.BelmontShore$Mean.Density)
shapiro.test(data.CabrilloBeachBay$Mean.Density)
shapiro.test(data.CabrilloBeachSZ$Mean.Density)
shapiro.test(data.MarinadelRey$Mean.Density)
shapiro.test(data.NaplesIsland$Mean.Density)

#create histograms: mean density in different location
ggplot()+
  geom_histogram(data=data.BelmontShore, aes(x=Mean.Density), binwidth = 5, fill = "salmon")+
  theme_bw(base_size=10)+ xlim(0,600) + ylim(0,5) + 
  xlab("Mean Density of Fish in Belmont Shore")+ylab("Frequency")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))
ggplot()+
  geom_histogram(data=data.CabrilloBeachBay, aes(x=Mean.Density), binwidth = 5, fill = "salmon")+
  theme_bw(base_size=10)+ xlim(0,600) + ylim(0,5) + 
  xlab("Mean Density of Fish in Cabrillo Beach - Bay")+ylab("Frequency") +
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))
ggplot()+
  geom_histogram(data=data.CabrilloBeachSZ, aes(x=Mean.Density), binwidth = 5, fill = "salmon")+
  theme_bw(base_size=10)+ xlim(0,200) + ylim(0,5) + 
  xlab("Mean Density of Fish in Cabrillo Beach - SZ")+ylab("Frequency") +
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))
ggplot()+
  geom_histogram(data=data.MarinadelRey, aes(x=Mean.Density), binwidth = 5, fill = "salmon")+
  theme_bw(base_size=10)+ xlim(0,600) + ylim(0,5) + 
  xlab("Mean Density of Fish in Marina del Rey")+ylab("Frequency") +
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))
ggplot()+
  geom_histogram(data=data.NaplesIsland, aes(x=Mean.Density), binwidth = 5, fill = "salmon")+
  theme_bw(base_size=10)+ xlim(0,600) + ylim(0,5) + 
  xlab("Mean Density of Fish in Naples Island")+ylab("Frequency")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))

#Location and Mean Density
ggplot()+
  geom_bar(data=summary, aes(x=Location, y=Mean.Density, fill=Location), stat="identity", position="dodge") +
  scale_fill_manual(values=c("brown1", "rosybrown1","peachpuff2","darkseagreen3","darkseagreen4"))+
  theme_classic(base_size=10) + 
  xlab("Beach Seine Location") + ylab("Mean Density of Fish (No./100 m²)") + 
  theme(legend.position='none', axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))+
  scale_y_continuous(expand = c(0, 0))

#SST and Mean density
ggplot()+
  geom_point(data=summary, aes(x=Average.of.Temperature.C, y=Mean.Density), color="grey") + 
  geom_smooth(data=summary, aes(x=Average.of.Temperature.C, y=Mean.Density), method="lm", se=F, color="lightblue") +
  theme_classic(base_size=10) + 
  xlab("Average Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))+
  annotate(geom="text", x=19.4, y=120, label="y = 0.0045x + 18.613", color="black",size=3) +
  annotate(geom="text", x=19.4, y=113, label="R² = 0.079", color="black",size=3)

#how SST influence mean density in different locations
ggplot()+
  geom_point(data=data.BelmontShore, aes(x=Temperature.C, y=Mean.Density), color="brown1")+ 
  geom_smooth(data=data.BelmontShore, aes(x=Temperature.C, y=Mean.Density),method="lm", se=F,color="brown1") + 
  theme_classic(base_size=10) + ylim(0, 1000)+ 
  xlab("Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))+
  annotate(geom="text", x=14, y=100, label="y = 6.0772x + 112.94", color="black",size=3) +
  annotate(geom="text", x=14, y=70, label="R² = 0.0004", color="black",size=3) +
  ylim(0,700) + xlim(10,25)+ scale_y_continuous(expand = c(0, 0), limits = c(0,700))

ggplot()+
  geom_point(data=data.CabrilloBeachBay, aes(x=Temperature.C, y=Mean.Density),color="brown1")+
  geom_smooth(data=data.CabrilloBeachBay, aes(x=Temperature.C, y=Mean.Density),method="lm", se=F,color="brown1") +
  theme_classic(base_size=10) +
  xlab("Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12)) +
  annotate(geom="text", x=21.5, y=210, label="y = 8.7026x - 38.713", color="black",size=3)+
  annotate(geom="text", x=21.5, y=180, label="R² = 0.0153", color="black",size=3)+
  xlim(12,25)+   scale_y_continuous(expand = c(0, 0), limits = c(0,600))
  
ggplot()+
  geom_point(data=data.CabrilloBeachSZ, aes(x=Temperature.C, y=Mean.Density),color="brown1")+
  geom_smooth(data=data.CabrilloBeachSZ, aes(x=Temperature.C, y=Mean.Density),method="lm", se=F,color="brown1") +
  theme_classic(base_size=10) +
  xlab("Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))+
  annotate(geom="text", x=21.5, y=40, label="y = 2.1361x - 19.448", color="black",size=3)+
  annotate(geom="text", x=21.5, y=33, label="R² = 0.0316", color="black",size=3)+
  scale_y_continuous(expand = c(0, 0), limits=c(0,150))

ggplot()+
  geom_point(data=data.MarinadelRey, aes(x=Temperature.C, y=Mean.Density),color="brown1")+
  geom_smooth(data=data.MarinadelRey, aes(x=Temperature.C, y=Mean.Density),method="lm", se=F,color="brown1") +
  theme_classic(base_size=10) +
  xlab("Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)")+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))+
  annotate(geom="text", x=22, y=190, label="y = 10.701x - 71.176", color="black",size=3)+
  annotate(geom="text", x=22, y=160, label="R² = 0.0293", color="black",size=3)+
  xlim(15,25)+  scale_y_continuous(expand = c(0, 0), limits = c(0,700))

ggplot()+
  geom_point(data=data.NaplesIsland, aes(x=Temperature.C, y=Mean.Density),color="brown1")+
  geom_smooth(data=data.NaplesIsland, aes(x=Temperature.C, y=Mean.Density),method="lm", se=F,color="brown1") +
  theme_classic(base_size=10) +
  xlab("Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)") +
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12)) +
  annotate(geom="text", x=23, y=300, label="y = 2.247x + 153.62", color="black",size=3)+
  annotate(geom="text", x=23, y=270, label="R² = 0.0014", color="black",size=3)+
  xlim(15,25)+  scale_y_continuous(expand = c(0, 0),limits = c(0,600))

#SST and Location with Mean Density
ggplot()+
  geom_point(data=data, aes(x=Temperature.C, y=Mean.Density, color=Location, shape=Location), size=1.5)+ 
  geom_smooth(data=data, aes(x=Temperature.C, y=Mean.Density, color=Location), method="lm", se=F) +
  theme_classic(base_size=10)+ ylim(0, 100) +
  scale_shape_discrete(name="Beach Seine Location") + 
  scale_color_manual(name="Beach Seine Location", values=c("brown1", "rosybrown1","peachpuff2","darkseagreen3","darkseagreen4")) + 
  xlab("Sea Surface Temperature (°C)" ) + ylab("Mean Density of Fish (No./100 m²)") +
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size=12))+
  annotate(geom="text", x=16, y=62, label="y = 2.247x + 153.62", color="darkseagreen4",size=3) +
  annotate(geom="text", x=16, y=60, label="R² = 0.0014", color="darkseagreen4",size=3) +
  annotate(geom="text", x=26, y=62, label="y = 8.7026x - 38.713", color="rosybrown2",size=3)+
  annotate(geom="text", x=26, y=60, label="R² = 0.0153", color="rosybrown2",size=3) +
  annotate(geom="text", x=26, y=50, label="y = 10.701x - 71.176", color="darkseagreen3",size=3)+
  annotate(geom="text", x=26, y=48, label="R² = 0.0293", color="darkseagreen3",size=3) +
  annotate(geom="text", x=27, y=30, label="y = 6.0772x + 112.94", color="brown1",size=3) +
  annotate(geom="text", x=27, y=28, label="R² = 0.0004", color="brown1",size=3)+
  annotate(geom="text", x=27.2, y=10, label="y = 2.1361x - 19.448", color="peachpuff3",size=3)+
  annotate(geom="text", x=27.2, y=8, label="R² = 0.0316", color="peachpuff3",size=3)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100))

#run a GLM
glm.results <- glm(Mean.Density ~ Location + Temperature.C + Location:Temperature.C, data=data) 
#run a LM
lm.results <- lm(Mean.Density ~ Location + Temperature.C + Location:Temperature.C, data=data)
lm.results1 <- lm(Mean.Density ~ Location, data=data)
lm.results2 <- lm(Mean.Density ~ Temperature.C, data=data)

#print the results of GLM and LM to screen
summary(glm.results)
summary(lm.results)
summary(lm.results1)
summary(lm.results2)

#Boxplot: location vs mean density
ggplot()+
  geom_boxplot(data=data, aes(x=Location, y=Mean.Density), col=c("brown1", "rosybrown1","peachpuff2","darkseagreen3","darkseagreen4"))+
  theme_bw(base_size=10) + ylab("Mean Density") + ylim(0,1000) +
  xlab("Beach Seine Location" ) + ylab("Mean Density of Fish (No./100 m²)")+ 
  scale_y_continuous(expand = c(0, 0), limits=c(0,500))

