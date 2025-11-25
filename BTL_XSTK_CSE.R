# 1. Doc du lieu
GPU_data <- read.csv("~/Desktop/BTL_XSTK/All_GPUs.csv")
head(GPU_data,3)
# 2. Lam sach du lieu
new_data <- GPU_data [,c("Manufacturer","Memory_Bandwidth","Release_Date",
                         "Memory_Bus", "Texture_Rate", "Memory_Speed")]
str(new_data)
#Loại bỏ đơn vị của biến Memory_Bandwidth

new_data$Memory_Bandwidth <- as.numeric(sub("GB/sec", "", 
                                            new_data$Memory_Bandwidth))
#Loại bỏ đơn vị của biến Memory_Bus
new_data$Memory_Bus <- as.numeric(sub("Bit", "", 
                                      new_data$Memory_Bus))
new_data$Memory_Speed <- as.numeric(sub("MHz", "", new_data$Memory_Speed))
#Loại bỏ đơn vị của biến Texture_Rate
new_data$Texture_Rate <- as.numeric(sub(" GTexel/s", "", 
                                        new_data$Texture_Rate))
#Loại bỏ các dòng Release_Date chứa cụm từ “Unknown Release Date”:
new_data <- new_data[!grepl("Unknown Release Date", new_data$Release_Date),]
#Tạo thêm biến Year trích từ cột Relase_Date
test_mat<-strsplit(new_data$Release_Date,split = '-')
date_matix<-matrix(unlist(test_mat),ncol=3,byrow=T)
new_data$Year<-date_matix[,3]
new_data$Year<-as.numeric(new_data$Year)
#Kiểm tra dữ liệu khuyết
colSums(is.na(new_data))
#Xử lý dữ liệu khuyết
new_data <- na.omit(new_data)
which(is.na(new_data))
#Làm rõ dữ liệu
by(new_data$Memory_Bandwidth, new_data$Manufacturer,function(x) {c(summary(x),sd=sd(x))})

by(new_data$Memory_Bandwidth, new_data$Memory_Bus,function(x) {c(summary(x),sd=sd(x))})
# Kiểm tra mẫu
Memory_Bandwidth<-new_data$Memory_Bandwidth
Manufacturer<-new_data$Manufacturer

shapiro.test(Memory_Bandwidth[Manufacturer == "AMD"])
shapiro.test(Memory_Bandwidth[Manufacturer == "ATI"])
shapiro.test(Memory_Bandwidth[Manufacturer == "Intel"])
shapiro.test(Memory_Bandwidth[Manufacturer == "Nvidia"])


sample_AMD <- sample(Memory_Bandwidth[Manufacturer == "AMD"],2240,replace = T)
sample_Nvidia <- sample(Memory_Bandwidth[Manufacturer == "Nvidia"],2240,replace = T)
sample_Intel <- sample(Memory_Bandwidth[Manufacturer == "Intel"], 2240, replace = T)


wilcox.test(sample_AMD,sample_Nvidia, alternative = "less")

wilcox.test(sample_Nvidia,sample_Intel, alternative = "greater")

wilcox.test(sample_AMD,sample_Intel, alternative = "greater")

# 3. Thống kê mô tả
#Vẽ đồ thị hist thể hiện sự phân phối của Memory_Bandwidth theo hãng anova kiểm tra
hist(new_data$Memory_Bandwidth,xlab="Memory_Bandwidth",
     main="Histogram of Memory_Bandwidth", labels=T, ylim = c(0, 1800))
qqnorm(new_data$Memory_Bandwidth)
qqline(new_data$Memory_Bandwidth)

hist(new_data$Memory_Speed,xlab="Memory_Bandwidth",
     main="Histogram of Memory_Bandwidth", labels=T, ylim = c(0, 1800))
qqnorm(new_data$Memory_Speed)
qqline(new_data$Memory_Speed)

hist(new_data$Memory_Bus,xlab="Memory_Bandwidth",
     main="Histogram of Memory_Bandwidth", labels=T, ylim = c(0, 1800))
qqnorm(new_data$Memory_Bus)
qqline(new_data$Memory_Bus)

hist(new_data$Texture_Rate,xlab="Memory_Bandwidth",
     main="Histogram of Memory_Bandwidth", labels=T, ylim = c(0, 1800))
qqnorm(new_data$Texture_Rate)
qqline(new_data$Texture_Rate)
#Vẽ độ thị Boxplot thể hiện phân phối của Memory_Bandwidth theo các hãng (Manufacturer): 
boxplot(Memory_Bandwidth~Manufacturer,new_data)
#Vẽ độ thị boxplot và plot của Memory_Bandwidth theo năm (Year):
par(mfrow=c(1,2))
hist(new_data$Year,xlab="Year",
     main="", labels=T, ylim = c(0, 1800))
boxplot(Memory_Bandwidth~Year,new_data)
plot(new_data$Year,new_data$Memory_Bandwidth,xlab="Year",ylab="Memory_Bandwidth")

#Vẽ độ thị boxplot và plot của Memory_Bandwidth theo độ lớn của kênh truyền dẫn dữ liệu bên trong RAM:
boxplot(Memory_Bandwidth~Memory_Bus,new_data)
plot(new_data$Memory_Bus,new_data$Memory_Bandwidth,xlab="Memory_Bus",ylab="Memory_Bandwidth")
#Vẽ độ thị boxplot và plot của Memory_Bandwidth theo tốc độ làm đầy:
boxplot(Memory_Bandwidth~Texture_Rate,new_data)
plot(new_data$Texture_Rate,new_data$Memory_Bandwidth,xlab="Texture_Rate",ylab="Memory_Bandwidth")
plot(new_data$Memory_Speed,new_data$Memory_Bandwidth,xlab="Memory_Speed",ylab="Memory_Bandwidth")


plot(Memory_Bandwidth~Memory_Bus,new_data,main="đồ thị")
plot(Memory_Bandwidth~Texture_Rate,new_data,main="đồ thị")
plot(Memory_Bandwidth~Memory_Speed,new_data,main="đồ thị")
plot(Memory_Bandwidth~Year,new_data,main="đồ thị")
#5.1 Kiểm định 1 mẫu
n <- length(new_data$Memory_Bandwidth)
xtb <- mean(new_data$Memory_Bandwidth)
s <- sd(new_data$Memory_Bandwidth)
data.frame(n, xtb, s)

qqnorm(new_data$Memory_Bandwidth)
qqline(new_data$Memory_Bandwidth)


shapiro.test(residuals(new_data$Memory_Bandwidth))

E = qnorm(p=1-0.05/2)*s/sqrt(n)
print(E)



data.frame(CT=xtb-E, CP=xtb+E)

E1 = qt(p=1-0.05/2, df = n-1)*s/sqrt(n)
print(E1)

data.frame(CT1=xtb-E1, CP1=xtb+E1)

new_data$Group<-ifelse(new_data$Memory_Speed<= 1300, "Group_1", "Group_2")
# Nhóm 1
Group_1_data <- subset (new_data, Group=="Group_1") 
n1 <- length (Group_1_data$Memory_Bandwidth) 
xtb1 <- mean (Group_1_data$Memory_Bandwidth) 
s1 <- sd (Group_1_data$Memory_Bandwidth)
# Nhóm 2
Group_2_data <- subset (new_data, Group=="Group_2" ) 
n2 <- length(Group_2_data$Memory_Bandwidth) 
xtb2 <- mean (Group_2_data$Memory_Bandwidth)
s2 <- sd (Group_2_data$Memory_Bandwidth)
# In ra kết quả
data.frame(n1,xtb1,s1,n2,xtb2, s2)
#Tính thống kê kiểm định
z0=(xtb1-xtb2)/sqrt(s1^2/n1+s2^2/n2)
print(z0)

RR = qnorm(p=1-.01)
print(RR)

t0=(xtb1-xtb2)/sqrt(s1^2/n1+s2^2/n2)
print(t0)
df = ((s1^2/n1)+(s2^2/n1))^2/((s1^2/n1)/(n1-1)+(s2^2/n1)/(n2-1))
print(df)
qt(1-0.01,df = 25426)
var.test(Group_1_data$Memory_Bandwidth,Group_2_data$Memory_Bandwidth,alternative = "less")
#5.3 Kiểm định 2 mẫu
#anova
shapiro.test(new_data$Memory_Bandwidth)
shapiro.test(new_data$Memory_Bus)
shapiro.test(new_data$Texture_Rate)

library(car)
leveneTest(Memory_Bandwidth~as.factor(new_data), data=new_data)
leveneTest(Memory_Bandwidth~as.factor(new_data$Manufacturer))
anv_1_way <- aov(Memory_Bandwidth~Manufacturer,data=new_data)
shapiro.test(anv_1_way$residuals)

summary(anv_1_way)
TukeyHSD(anv_1_way)
plot(TukeyHSD(anv_1_way))
anova_2_way<-aov(Memory_Bandwidth~Manufacturer*Group,data=new_data)
summary(anova_2_way)
#Hồi quy tuyến tính đơn
ggscatter(data = new_data, x = "Memory_Speed", y = "Memory_Bandwidth") + geom_smooth(method = "lm", se = TRUE)
RR = qnorm(p = 1-0.01)
lm<-lm(Memory_Bandwidth~Memory_Speed,new_data)
summary(lm)
plot(lm)
#Bài toán 4. Hồi quy tuyến tính đa biến
lm_model<-lm(Memory_Bandwidth~Manufacturer+Year+Memory_Bus+Texture_Rate+Memory_Speed,new_data)
summary(lm_model)

lm_model_2<-lm(Memory_Bandwidth~Manufacturer+Year+Memory_Bus+Texture_Rate+Memory_Speed,new_data)
summary(lm_model_2)

anova(lm_model,lm_model_2)

plot(lm_model)

M= cor(new_data_2[,1:5])


set.seed(1)
train_rows <- sample (rownames (new_data), dim(new_data) [1]*0.8)
train_data <- new_data[train_rows, ]
test_rows <- setdiff(rownames(new_data), train_rows)
test_data <- new_data[test_rows, ]
head(test_data,10)
str(train_data)
str(test_data)

lm_model1<-lm(Memory_Bandwidth~Manufacturer+Year+Memory_Bus+Texture_Rate+Memory_Speed,train_data)
summary(lm_model1)

lm_model2<-lm(Memory_Bandwidth~Year+Memory_Bus+Texture_Rate+Memory_Speed,train_data)
summary(lm_model2)

anova(lm_model1,lm_model2)

plot(lm_model1)

data(managers)
chart.Correlation(new_data[,1:4], histogram=TRUE, pch="+")
new_data_2 <- new_data [,c("Memory_Bandwidth","Year","Texture_Rate",
                           "Memory_Bus", "Memory_Speed")]
M = cor(new_data_2[,1:5])
corrplot(M, method = "circle")

chart.Correlation(new_data_2, histogram=TRUE, pch='+')

test_data$Memory_Bandwidth <- predict(lm_model2, test_data)
head(test_data,10)

Value = data.frame(Manufacturer="AMD", Memory_Speed =940, Year=2025, Memory_Bus=512,
                   Texture_Rate=234)
# tạo khung dữ liệu
predict(lm_model2, Value, interval = "confidence")# dự đoán các giá trị tương lai
