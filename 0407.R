# April. 7th
in1 <- datanl1(newdata)  #all the index database of compdata
write.csv(in1,"index1.csv")
in2 <- datanl1(compda)   #the data for analysing
write.csv(in2,"index2.csv")

hist(in2$number,25,col = rainbow(15)) 
hist(indexbase1$number,25,col = rainbow(15))


#===============================================
library(car)
scatterplotMatrix(indexbase1[,c(2,3,4,5)],diagonal = 'histogram')

scatterplotMatrix(~ h.index + g.index + r.index ,lty.smooth = 2,data=indexbase1)

r <- ggplot(data = in11, mapping = aes(x = avc,y=hac,col = h.index))
r+geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 'black'),legend.text=
        element_text(size = 12),legend.title = element_text(size = 13),
        axis.title=element_text(size = 15))+labs(x = "平均被引频次",y="a值") + 
        xlim(c(0,150)) + ylim(c(0,10))+geom_abline(intercept=1.59884,slope = 0.04182,
                        col = "black",cex = 0.8)

        
corrgram(in12[,c(2,5,6,7,8,9,10)],lower.panel=panel.shade,
          upper.panel=panel.pie,cex=1.3)    


# the expectation of the rmhindex
fakedata <- cbind("kong",data.frame("tc"=1:1000))
rmhindex(fakedata)

hist(newdata1$pyear,30,col = rainbow(15))
hist(compdata1$pyear,30,col = rainbow(15))
hist(in11$year1st,25,col = rainbow(15))
hist(in12$year1st,25,col = rainbow(15))

colors() #demo all the colors in R

hist(in12$g.index,20,col = "springgreen2")
hist(in11$g.index,20,col = "springgreen1")
hist(in12$r.index,20,col = "springgreen4")
hist(in11$r.index,20,col = "springgreen4")
hist(in11$journals,20,col = "springgreen4")


opar <- par(no.readonly=TRUE)


#==========================================================  
#=================The first figure=========================
#==========================================================

#            ==========Part I===========
s <- ggplot(data=in1,mapping = aes(x=number,y=h.index))
s+geom_point(color = "springgreen4") +theme(axis.text=element_text(face="bold",
        size=12,colour = 'black'),axis.title = 
                element_text(size = "15"))+labs(x="发文数量",y="h指数")+
        geom_smooth(cex = 1,color="black") + ylim(c(0,150))+xlim(c(0,600))+geom_abline()

hist(in1$number,15,col = rainbow(10),xlim = c(0,600))
hist(in1$h.index,10,col = rainbow(10),xlim = c(150,0))
boxplot(in1$number,horizontal = TRUE,ylim = c(0,600))
boxplot(in1$h.index,horizontal = TRUE,ylim = c(0,150))
#                
#           ===========Part II==========

s2 <- ggplot(data=in2,mapping = aes(x=number,y=h.index))
s2+geom_point(color = "springgreen4") +theme(axis.text=element_text(face="bold",
                 size=12,colour = 'black'),axis.title = 
                element_text(size = "15"))+labs(x="发文数量",y="h指数")+
        geom_smooth(cex = 1,color="black") 

hist(in2$number,15,col = rainbow(10))
hist(in2$h.index,15,col = rainbow(10),xlim = c(160,0))
boxplot(in2$number,horizontal = TRUE)
boxplot(in2$h.index,horizontal = TRUE,ylim = c(0,160))

#===========================================================
#===================---End---===============================
#===========================================================


#                    ========
#===========================================================
#========h-index t test for number bigger than 200==========
#===========================================================

mean(in1$h.index);mean(in2$h.index)

h200_1 <- in1$h.index[which(in1$number >= 200)]
h200_2 <- in2$h.index[which(in2$number >= 200)]
mean(h200_1)
mean(h200_2)
t.test(h200_1,h200_2)

h200_11 <- in1$h.index[which(in1$number < 200)]
h200_22 <- in2$h.index[which(in2$number < 200)]
t.test(h200_11,h200_22)

t.test(in1$h.index,in2$h.index)


var(in2$h.index)/var(in1$h.index) #if bigger than 1
var(h200_22)/var(h200_11) # if > 1
var(h200_2)/var(h200_1)



#              =============
#==================================================================
#=========Relation among "number","pyear" & "h.index"==============
#==================================================================


p <- ggplot(data = in1,mapping=aes(x = year1st,y=number,col = h.index))
p + geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 
        'black'),legend.text=element_text(size = 12),legend.title =
        element_text(size = 13),axis.title=element_text(size = 15))+
        labs(x = "科研年龄",y="发文数量")+ylim(c(0,700))+xlim(c(0,80))

hist(in1$year1st,15,col = rainbow(10),xlim = c(0,80))
boxplot(in1$year1st,horizontal = TRUE,ylim = c(0,80))

#==========================Part II==============================
q <- ggplot(data = in2,mapping=aes(x = year1st,y=number,col = h.index))
q + geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 
        'black'),legend.text=element_text(size = 12),legend.title = 
                element_text(size = 13),axis.title=element_text(size = 15))+
        labs(x = "科研年龄",y="发文数量")+xlim(c(0,80))

hist(in2$year1st,15,col = rainbow(10),xlim = c(0,80))
boxplot(in2$year1st,horizontal = TRUE,ylim = c(0,80))

#=======================find the outliers=======================

which(in1$year1st <40 & in1$number > 400)
which(in2$year1st <40 & in2$number > 650)

#======average year when have more than 100 papers==============

ye1 <- in1$year1st[in1$number >= 200]
ye2 <- in2$year1st[in2$number >= 200]
t.test(ye1,ye2);length(ye1);length(ye2)
#Stratified test shows there is no difference between them
t.test(in1$year1st,in2$year1st)


#========================--END--================================



#===============================================================
#========================Corrgram===============================
#===============================================================
library(corrgram)
corin1 <- in1
colnames(corin1) <- c("researcher","h","g","r",'num','age','j','v','ac','tc',
                      'rhmax','rhmin','rhavg')
corrgram(corin1[,-c(11,12,13)],lower.panel=panel.shade,
         upper.panel=panel.pie,cex=1.5)

corin2 <- in2
colnames(corin2) <- c("researcher","h","g","r",'num','age','j','v','ac','tc',
                      'rhmax','rhmin','rhavg')
corrgram(corin2[,-c(11,12,13)],lower.panel=panel.shade,
         upper.panel=panel.pie,cex=1.5)

#======================--END--===============================




#============================================================
#====================The a Value=============================
#============================================================
in1$a_val <- in1$allc/in1$h.index^2
r <- ggplot(data = in1, mapping = aes(x = avc,y=a_val,col = h.index))
r+geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 'black'),legend.text=
        element_text(size = 12),legend.title = element_text(size = 13),
        axis.title=element_text(size = 11))+labs(x = "平均被引频次",y="a值") + 
        geom_abline(intercept=1.59884,slope = 0.04182,
        col = "black",cex = 0.8)
r+geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 'black'),legend.text=
        element_text(size = 12),legend.title = element_text(size = 13),
        axis.title=element_text(size = 15))+labs(x = "平均被引频次",y="a值") + 
        geom_abline(intercept=1.59884,slope = 0.04182,
        col = "black",cex = 0.8)+ylim(c(2,12))+xlim(c(0,150))

corin1$a_val <- in1$a_val
corrgram(corin1[,-c(11,12,13)],lower.panel=panel.shade,
         upper.panel=panel.pie,cex=1.5)

length(which(in1$a_val > 2.5 & in1$a_val < 4.5))
length(which(in1$a_val > 2 & in1$a_val < 6))
length(which(in1$a_val > 5))

#========================Part II=================================

in2$a_val <- in2$allc/in2$h.index^2
r2 <- ggplot(data = in2, mapping = aes(x = avc,y=a_val,col = h.index))
r2+geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 
                                                    'black'),legend.text=
        element_text(size = 12),legend.title = element_text(size = 13),
        axis.title=element_text(size = 11))+labs(x = "平均被引频次",y="a值") + 
        geom_abline(intercept=3.2747,slope = 0.0163,col = "black",cex = 0.8)

r2+geom_point(cex.lab = 4,cex = 3)+scale_colour_gradient(low = "blue",high = 
        'red')+theme(axis.text=element_text(face="bold",size=12,colour = 
                                'black'),legend.text=
        element_text(size = 12),legend.title = element_text(size = 13),
        axis.title=element_text(size = 15))+labs(x = "平均被引频次",y="a值") + 
        geom_abline(intercept=3.2747,slope = 0.0163,col = "black",cex = 0.8)+
        xlim(c(0,200)) + ylim(2,12)

corin2$a_val <- in2$a_val
cor(corin2[,-1])
length(which(in2$a_val > 5 ))
boxplot(in1$a_val,in2$a_val,ylim = c(0,12))

t.test(in1$a_val,in2$a_val)


g <-ggplot(data = indata,mapping = aes(x = h.index,y = allc,   col = 领域))
g+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="h指数",y="总被引频次")

in1$领域 <- "M"
in2$领域 <- "N"
indata <- rbind(in1,in2)

g2 <-ggplot(data = indata,mapping = aes(x = h.index^2,y = allc,   col = 领域))
g2+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="h指数的平方",y="总被引频次")

#==============================================================
#=====================Other Parameters=========================
#==============================================================


#============Publication year========================
write.csv(table(newdata$pyear),"pyear1.csv")
write.csv(table(compda$pyear),"pyear2.csv")

#====================================================


#===================--g index--======================
#===================--r index--======================

hist(in1$g.index,15,col = rainbow(15))
hist(in2$g.index,15,col = rainbow(15))
gdata1 <- data.frame("g" = in1$g.index,"tc" = in1$allc,"领域" = "微生物学")


gdata2 <- data.frame("g" = in2$g.index,"tc" = in2$allc,  "领域" = "神经学")

gdata <- rbind(gdata1,gdata2)
gdata[,1] <- as.numeric(gdata[,1])
gdata[,3] <- as.numeric(gdata[,3])


boxplot(in1$g.index,in2$g.index,in1$r.index,in2$r.index,ylim = c(0,200),notch = 
                TRUE,col = c("yellow2","green","yellow2","green"),varwidth = TRUE,
        horizontal = T,names= c("g-index","g-index","r-index","r-index"))
legend(legend =  c("M","N"), "bottomright",col = c("yellow2","green"),pch = 20,cex = 1.5)
abline(v = 58.11,lty = 2,col = "red")


library(ggplot2)


#===============================================
#shaping data to use geom_boxplot
#ggplot(diamonds) + geom_boxplot(aes(x = cut, y = price, fill = color))

Mg_in <- data.frame("researcher" = in1$researcher,"val" = in1$g.index1,"indty" = "g.index")
Mr_in <- data.frame("researcher" = in1$researcher,"val" = in1$r.index,"indty" = "r.index")
Mrg <- rbind(Mg_in,Mr_in); Mrg <- cbind(Mrg,"area" = "Microbiology")

Ng_in <- data.frame("researcher" = in2$researcher,"val" = in2$g.index1,"indty" = "g.index")
Nr_in <- data.frame("researcher" = in2$researcher,"val" = in2$r.index,"indty" = "r.index")
Nrg <- rbind(Ng_in,Nr_in); Nrg <- cbind(Nrg,"area" = "Neuroscience")

rgdat <- rbind(Mrg,Nrg)
require(ggthemes)
inpl1 <- ggplot(data = rgdat, mapping = aes(x = area, y = val,fill = indty))
inpl1 + geom_boxplot(notch = TRUE) + theme_wsj() + ylim(c(0,200)) + geom_hline(
        yintercept = 58.11,linetype = 2)




gin <-ggplot(data = indag,mapping = aes(x = h.index,y = g.index1/h.index,   col = 领域))
gin+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) +labs(x="h指数",y="g指数/h指数")

hist(indag$g.index1/indag$h.index,20,col = rainbow(15),xlim = c(2.5,1.2))

indag <- indata[indata$g.index1 >= indata$r.index,]

gbiggerthanr <-ggplot(data = indag,mapping = aes(x = h.index,y = g.index1-h.index,   col = 领域))
gbiggerthanr+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="h指数",y="g指数 - h指数")

hist(indag$g.index1-indag$h.index,20,col = rainbow(15),xlim = c(130,0))


in1gr <- in1[in1$g.index1 <= in1$r.index,]  #g < r
in1rg <- in1[in1$g.index1 > in1$r.index,]   #g >r

in2gr <- in2[in2$g.index1 <= in2$r.index,]  #g < r
in2rg <- in2[in2$g.index1 > in2$r.index,]   #g >r

rin <-ggplot(data = indata,mapping = aes(x = h.index,y = r.index/h.index,   col = 领域))
rin+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="h指数",y="r指数/h指数")+ylim(c(1,5))

hist(indata$r.index/indata$h.index,20,col = rainbow(15),xlim = c(5,1))

grin <-ggplot(data = indag,mapping = aes(x = h.index,y = g.index1/r.index,  col = 领域))
grin+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="h指数",y="g指数/r指数")


hist(indag$g.index1/indag$r.index,20,col = rainbow(15),xlim = c(1.12,1))


jin <-ggplot(data = indata,mapping = aes(x = number,y = journals,  col = 领域))
jin+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="发文数",y="不同期刊数量") + 
        xlim(c(0,600))+ylim(c(0,150))
                

hist(indata$journals,20,col = rainbow(15),xlim = c(150,0))

vin <-ggplot(data = indata,mapping = aes(x = vip,y = h.index,  col = 领域))
vin+geom_point()+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),legend.text=element_text(size = 
        11),legend.title = element_text(size = 11),axis.title=element_text(
        size = 13)) + geom_smooth()+labs(x="重要文献数量",y="h指数") + 
        xlim(c(0,100)) + ylim(c(0,120))



t.test(in1$g.index,in2$g.index)


#================================================================
#===================Linear regression============================
#================================================================


# F-test: 检验方程中的各个系数是否显著不为零，检验的原假设为所有系数均为0；检验
# 是否所有系数都等于零等同于检验R^2 = 0
# t-test: 方程的 总体线性关系显著 !=  每个解释变量 对被解释变量的影响都是显著的，
# 因此，必须对每个解释变量进行显著性检验，以决定是否作为解释变量被保留在模型中，
#这一检验既是对变量的t检验




fit1 <- lm(h.index ~ number + year1st + journals + vip + allc,data = in1)

plot(fit1$residuals,col = "blue",pch = 20,cex = 0.8)
abline(h = 0,lty = 2, col = "red",lwd =2)

plot(fit1,2);shapiro.test(fit1$residuals)

fit11 <- lm(h.index ~sqrt(number) + year1st + journals + vip + sqrt(allc),data = in1)

hc1 <- cbind(in1$h.index,fit1$fitted.values)

hc11 <- cbind(in1$h.index,fit11$fitted.values)
colnames(hc1) <- c("h.index","predict")
colnames(hc11) <- c("h.index","predict")
library(ggplot2)
hc1 <- as.data.frame(hc1)
hc1 <- ggplot(data = hc1,mapping = aes(x = h.index,y = predict ))
hc1 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype =
                        2,cex=1)+ylim(c(0,120))+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

hc11 <- as.data.frame(hc11)
hc11 <- ggplot(data = hc11,mapping = aes(x = h.index,y = predict ))
hc11 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
                = 2,cex=1)+ylim(c(0,120))+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)


fit12 <- lm(h.index ~ sqrt(number) + journals  + vip + sqrt(allc) ,data = in1)
hc12 <- cbind(in1$h.index,fit12$fitted.values)
hc12 <- as.data.frame(hc12)
colnames(hc12) <- c("h.index","predict")
hc12 <- ggplot(data = hc12,mapping = aes(x = h.index,y = predict ))
hc12 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+ylim(c(0,120))+theme(axis.text=element_text(
        face="bold",size=11,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

hist(fit12$residuals,45,freq = F)
x <- seq(-10,10,by = .1);ynor <- dnorm(x =x, mean = mean(fit12$residuals),sd =sd(fit12$residuals))
lines(x = x,y = ynor,col = "red",lwd = 2,lty =2)

hist(fit11$residuals,45,freq = F)
x <- seq(-10,10,by = .1);ynor <- dnorm(x =x, mean = mean(fit11$residuals),sd =sd(fit11$residuals))
lines(x = x,y = ynor,col = "red",lwd = 2,lty =2)


plot(fit1,2);shapiro.test(fit1$residuals)

fit21 <- lm(h.index ~ sqrt(number) + journals + vip + sqrt(allc),data = in2)

hist(fit21$residuals,25,freq = F,col= "grey",xlab = "残差",ylab = "密度")
x <- seq(-13,13,by = .1);ynor <- dnorm(x =x, mean = mean(fit21$residuals),sd =sd(fit21$residuals))
lines(x = x,y = ynor,col = "red",lwd = 2,lty =2)


hc21 <- cbind(in2$h.index,fit21$fitted.values)
hc21 <- as.data.frame(hc21)
colnames(hc21) <- c("h.index","predict")
hc21 <- ggplot(data = hc21,mapping = aes(x = h.index,y = predict ))
hc21 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
                 = 2,cex=1)+theme(axis.text=element_text(
                face="bold",size=11,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

fitg1 <- lm(g.index ~ sqrt(number) + journals  + sqrt(allc),data = in1)

fitr1 <- lm(r.index ~ sqrt(number) + journals + vip + sqrt(allc),data = in1)





#======================去掉那些科研年龄小于15的数据=============================

res15_1 <- in1$researcher[which(in1$year1st < 15)]

reschoice <- vector()
for (i in 1:22527) {
        if (sum(nd1$researcher[i] == res15_1) == 0 ){
                reschoice[i] <- TRUE
        } else{
                reschoice[i] <- FALSE
        }
}

nd15_1 <- nd1[reschoice,]

res15_2 <- in2$researcher[which(in2$year1st < 15)]
reschoice2 <- vector()
for (i in 1:33163) {
        if (sum(cmp$researcher[i] == res15_2) == 0 ){
                reschoice2[i] <- TRUE
        } else{
                reschoice2[i] <- FALSE
        }
} 

cmp15_1 <- cmp[reschoice2,]

 M_py2000 <- sepr(nd15_1,2000)
 M_py2001 <- sepr(nd15_1,2001)
 M_py2005 <- sepr(nd15_1,2005)
 M_py2010 <- sepr(nd15_1,2010)
 N_py2000 <- sepr(cmp15_1,2000)
 N_py2001 <- sepr(cmp15_1,2001)
 N_py2005 <- sepr(cmp15_1,2005)
 N_py2010 <- sepr(cmp15_1,2010)

M_in00 <- datanal(M_py2000)
M_in01 <- datanal(M_py2001)
M_in05 <- datanal(M_py2005)
M_in10 <- datanal(M_py2010)

N_in00 <- datanal(N_py2000)
N_in01 <- datanal(N_py2001)
N_in05 <- datanal(N_py2005)
N_in10 <- datanal(N_py2010)

M_in00$a <- M_in00$allc/M_in00$h.index^2
Mindata <- M_in00
Mindata$h01 <- M_in01$h.index;Mindata$h05 <- M_in05$h.index; Mindata$h10 <-M_in10$h.index

N_in00$a <- N_in00$allc/N_in00$h.index^2; Nindata <- N_in00
Nindata$h01 <- N_in01$h.index;Nindata$h05 <- N_in05$h.index; Nindata$h10 <-N_in10$h.index




#================比较不同时间的a值变化===================
N_in01$a <- N_in01$allc/N_in01$h.index^2
N_in05$a <- N_in05$allc/N_in05$h.index^2
N_in10$a <- N_in10$allc/N_in10$h.index^2


#============去掉2000年之后发文数量过多的数据==============
summary(M_in10$number/M_in00$number);quantile(M_in10$number/M_in00$number,0.9)
summary(N_in10$number/N_in00$number);quantile(N_in10$number/N_in00$number,0.9)

length(which(M_in10$number/M_in00$number <= 5.23))
length(which(N_in10$number/N_in00$number <= 5.45))

plot(M_in10$number/M_in00$number,pch = 20,cex = 0.8,ylab = "2010年发文量/2000年发文量",xlab = "微生物学")
plot(N_in10$number/N_in00$number,pch = 20,cex = 0.8,ylab = "2010年发文量/2000年发文量",xlab = "神经科学")


Mindata$ratio <- M_in10$number/M_in00$number
Nindata$ratio <- N_in10$number/N_in00$number

Mindata1 <- Mindata[Mindata$ratio <= 5.23,]
Nindata1 <- Nindata[Nindata$ratio <= 5.45,]


#====================分成训练集和测试集===================


set.seed(1194)
sap1 <- sample(173,121)
train_M <- Mindata1[sap1,];test_M <- Mindata1[-sap1,]


summary(train_M[,c("h.index","number","allc")])
summary(test_M[,c("h.index","number","allc")])

ks.test(train_M$h.index,test_M$h.index)
ks.test(train_M$number,test_M$number)
ks.test(train_M$allc,test_M$allc)

set.seed(343)
sap2 <- sample(273,191)
train_N <- Nindata1[sap2,];test_N <- Nindata1[-sap2,]

summary(train_N[,c("h.index", "number","allc")])
summary(test_N[,c("h.index", "number","allc")])

ks.test(train_N$h.index,test_N$h.index)
ks.test(train_N$number,test_N$number)
ks.test(train_N$allc,test_N$allc)

#========================h指数预测================================

# one year later

fit01 <- lm(h01 ~ h.index + sqrt(number) + journals + vip + sqrt(allc)+a,data = train_M)


pretest <- predict(fit01,test_M)
preM01 <- data.frame("predict" = pretest, "h01" = test_M$h01)
mp01 <- ggplot(data = preM01,mapping = aes(x = h01,y = predict ))
mp01 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
                = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

fit02 <- lm(h01 ~ h.index + sqrt(number) + journals + vip + sqrt(allc)+a,data = train_N)

pretestN <-predict(fit02,test_N)
preN01 <- data.frame("predict" = pretestN, "h01" = test_N$h01)
np01 <- ggplot(data = preN01,mapping = aes(x = h01,y = predict ))
np01 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

fitM_05 <- lm(h05 ~h.index+sqrt(number) + journals + vip + sqrt(allc) + a,data = train_M)
pretestM05 <-predict(fitM_05,test_M)
preM05 <- data.frame("predict" = pretestM05, "h05" = test_M$h05)
mp05 <- ggplot(data = preM05,mapping = aes(x = h05,y = predict ))
mp05 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
         = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

fitN_05 <- lm(h05 ~h.index+sqrt(number) + journals + vip + sqrt(allc)+a ,data = train_N)
pretestN05 <-predict(fitN_05,test_N)
preN05 <- data.frame("predict" = pretestN05, "h05" = test_N$h05)
np05 <- ggplot(data = preN05,mapping = aes(x = h05,y = predict ))
np05 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

fitM_10 <- lm(h10 ~h.index+sqrt(number) + journals + vip + sqrt(allc) + a,data = train_M)
pretestM10 <-predict(fitM_10,test_M)
preM10 <- data.frame("predict" = pretestM10, "h10" = test_M$h10)
mp10 <- ggplot(data = preM10,mapping = aes(x = h10,y = predict ))
mp10 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
         = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)

fitN_10 <- lm(h10 ~h.index+sqrt(number) + journals + vip + sqrt(allc) + a,data = train_N)
pretestN10 <-predict(fitN_10,test_N)
preN10 <- data.frame("predict" = pretestN10, "h10" = test_N$h10)
np10 <- ggplot(data = preN10,mapping = aes(x = h10,y = predict ))
np10 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)


hist(Mindata$h.index,col = "yellow4",ylab = "频数",xlab = "初始h指数",mgp = c(2.2,1,0))
hist(Nindata$h.index,col = "yellow4",ylab = "频数",xlab = "初始h指数",mgp = c(2.2,1,0))


#==============10<=h指数<=20=============================
inter1 <- which(train_M$h.index <=20 & train_M$h.index >= 10)
trainM10 <- train_M[inter1,]

fitm101 <- lm(h01 ~ h.index + sqrt(number) + journals + vip + sqrt(allc)+a,data = trainM10)

fitm105 <- lm(h05 ~ h.index + sqrt(number) + journals + vip + sqrt(allc)+a,data = trainM10)


#==============g指数与r指数预测分析=====================

Mind1 <- M_in00;Mind1$g01 <- M_in01$g.index;Mind1$g05 <- M_in05$g.index;Mind1$g10 <- M_in10$g.index
Mind1$r01 <- M_in01$r.index;Mind1$r05 <- M_in05$r.index;Mind1$r10 <- M_in10$r.index


fitg01 <- lm(g01 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Mind1)
fitg05 <- lm(g05 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Mind1)
fitg10 <- lm(g10 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Mind1)


fitr01 <- lm(r01 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Mind1)
fitr05 <- lm(r05 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Mind1)
fitr10 <- lm(r10 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Mind1)

Nind1 <- N_in00;Nind1$g01 <- N_in01$g.index;Nind1$g05 <- N_in05$g.index;Nind1$g10 <- N_in10$g.index
Nind1$r01 <- N_in01$r.index;Nind1$r05 <- N_in05$r.index;Nind1$r10 <- N_in10$r.index

fitg01N <- lm(g01 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Nind1)
fitg05N <- lm(g05 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Nind1)
fitg10N <- lm(g10 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Nind1)


fitr01N <- lm(r01 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Nind1)
fitr05N <- lm(r05 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Nind1)
fitr10N <- lm(r10 ~ g.index + sqrt(number)+journals + vip + sqrt(allc) +a,data = Nind1)



preMg10 <-predict(fitg10,Mind1,interval = 'prediction')
premg10 <- data.frame( preMg10, "g10" = Mind1$g10)
mg10 <- ggplot(data = premg10,mapping = aes(x = fit,y = g10 ))
mg10 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "预测值",y="真实值") + geom_smooth(col = "red",cex=1)+ geom_line(
                mapping = aes(x=fit,y = upr)) + geom_line(aes(x = fit, y = lwr))+xlim(c(0,125))

preNg10 <-predict(fitg10N,Nind1,interval = 'prediction')
preng10 <- data.frame(preNg10, "g10" = Nind1$g10)
ng10 <- ggplot(data = preng10,mapping = aes(x = fit,y = g10 ))
ng10 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "预测值",y="真实值") + geom_smooth(col = "red",cex=1)+ geom_line(
                mapping = aes(x=fit,y = upr)) + geom_line(aes(x = fit, y = lwr))

preMr10 <-predict(fitr10,Mind1,interval = 'prediction')
premr10 <- data.frame( preMr10, "r10" = Mind1$r10)
mr10 <- ggplot(data = premr10,mapping = aes(x = r10,y = fit ))
mr10 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
                face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1)+ylim(c(0,150))

preNr10 <-predict(fitr10N,Nind1,interval = 'prediction')
prenr10 <- data.frame(preNr10, "r10" = Nind1$r10)
nr10 <- ggplot(data = prenr10,mapping = aes(x = r10,y = fit ))
nr10 + geom_point() + geom_abline(intercept = 0, slope =1,col = "blue",linetype
        = 2,cex=1)+theme(axis.text=element_text(
        face="bold",size=12,colour = 'black'),axis.title=element_text(size = 15)) + 
        labs(x = "真实值",y="预测值") + geom_smooth(col = "red",cex=1) 


#==================a值的变化分析=================================

maval <- data.frame("a00" = M_in00$a,"a01" = M_in01$allc/M_in01$h.index^2,"a05" = M_in05$allc/M_in05$h.index^2,
                    "a10" = M_in10$allc/M_in05$h.index^2)
maval <- maval[-145,]

fitrh <- lm(rhavg ~ number + year1st + vip + avc + allc, data = M_in10)

fitrhn <- lm(rhavg ~ number + year1st + vip + avc + allc, data = N_in10)

#==========================引用频次对数变换========================

logform00 <- datanl(M_py2000)

Nlogform <- datanl(cmp)
Mlogform <- datanl(nd1)


fitlogM <- lm(lgi ~ number + year1st + journals + vip + avc + allc, data = Mlogform)
fitlogN <- lm(lgi ~ number + year1st + journals + vip + avc + allc, data = Nlogform)


#============================非线性最小二乘拟合=======================================

Mlg00 <- datanl(M_py2000)
Mlg01 <- datanl(M_py2001)
Mlg05 <- datanl(M_py2005)

Nlg00 <- datanl(N_py2000)
Nlg01 <- datanl(N_py2001)

Nlg00$allc01<- Nlg01$allc

Mlg00$allc01 <- Mlg01$allc
Mlg00$allc05 <- Mlg05$allc
fitnls <- nls(allc01 ~ allc+log(allc+1)*(a1*(lgi)/(1+exp(-(log(allc+1)-4)))) + a3*((log(allc+1)+1))^2 + a2*lgi,start=list(a1=5,a2=2,a3 = 1.7),data = Mlg00);summary(fitnls)
pred01 <- predict(fitnls,Mlg00)

sumres <- sum((pred01 - Mlg00$allc01)^2)
fitallc <- lm(allc01~ allc + number , data = Mlg00)
sum(fitallc$residuals^2)

fitnls05 <-  nls(allc05 ~ allc+log(allc+1)*(a1*(lgi)/(1+exp(-(log(allc+1)-4)))) + a3*((log(allc+1)+1))^2 + a2*lgi,start=list(a1=5,a2=2,a3 = 1.7),data = Mlg00);summary(fitnls05)
pred05 <- predict(fitnls05,Mlg00)
sum((pred05 - Mlg00$allc05)^2)
sum(fitallc05$residuals^2)

fitallc05 <- lm(allc05 ~ allc + number, data = Mlg00)

fitallc <- lm(allc01~ allc + number , data = Mlg00)
ml2 <- cbind(Mlg00$allc01,fitallc$fitted.values)
plot(Mlg00$allc01,fitallc$fitted.values);abline(0,1)



#==================Predict for a Single Author=========================

cossar00 <- datanl(sepr(cossar,2000))
for (i in 2001:2014){        
        cossar00 <- rbind(cossar00,datanl(sepr(cossar,i)))
} 

cos00 <- cossar00[1:10,]
cos00$allc1 <- cossar00$allc[2:11]
fitnlscc <- nls(allc1 ~ allc+log(allc+1)*(a1*(lgi)/(1+exp(-(log(allc+1)-4)))) + a3*((log(allc+1)+1))^2 + a2*lgi,start=list(a1=1,a2=-2,a3 = 0.7),data = cos00);summary(fitnlscc)

cbind(predict(fitnlscc,cos00),cos00$allc1)

preallc <- function(allc,lgi){
        a1 = 0.59;a2 = -4.3956;a3 = 3.6165
        b <- allc+log(allc+1)*(a1*(lgi)/(1+exp(-(log(allc+1)-4)))) + a3*((log(allc+1)+1))^2 + a2*lgi
        return(b)
}





#=======================引用动力学模型============================

cossp <- cossar

for(i in 1:294){
        for (j in 8:122){
                cossp[i,j] <- sum(cossar[i,8:j])
        }
}

library(rgl)
t1 <- seq(4,4.6,by = 0.01);t3 <- seq(4,8,by = 0.01)
z123 <- matrix(rep(1:length(t1),length(t3)))
dim(z123) <- c(length(t1),length(t3))
for(i in 1:61){
for (j in 1:length(t3)){
z123[i,j] <- CostF(t1[i],t1[i]-3,t3[j],x=3,y=115)
}
}
persp3d(t1,t3,z123,col = 'yellow')



M1 <- data.frame(seq(3,5,by = 0.01),seq(0.5,2.5,by = 0.01))
colnames(M1) <- c('t1','t2')
M1$z <- 1:201
for (i in 1:201){M1[i,3] <- CostF(M1[i,1],M1[i,2],3.3,x=3,y =115)}

t1 <- M1$t1
t2 <- M1$t2
z <- outer(t1,t2,CostF(x=3,y=115))
z <- matrix(rep(1:201,201))
dim(z) <- c(201,201)
for(i in 1:201){
for (j in 1:201){
z[i,j] <- CostF(t1[i],t2[j],3.3,x = 3, y= 115)
}
}
persp3d(t1,t2,z,col = "yellow")
persp3d(t1,t2,z,col = "yellow",xlim = c(3,5),ylim = c(0,3))


t3 <- seq(2,4,by = 0.01)
z13 <- matrix(rep(1:201,201))
dim(z13) <- c(201,201)
for(i in 1:201){
for (j in 1:201){
z13[i,j] <- CostF(t1[i],1.21,t3[j],x = 3, y= 115)
}
}
persp3d(t1,t3,z13,col = "yellow")


chater <- subset(nd1, researcher == "ChaterK")

chat <- chater

for(i in 1:166){
        for (j in 8:122){
                chat[i,j] <- sum(chater[i,8:j])
        }
}

berge <- subset(nd1, researcher == "Berger")
berg <- berge
for(i in 1:65){
        for (j in 8:122){
                berg[i,j] <- sum(berge[i,8:j])
        }
}

azamf <- subset(nd1, researcher == "AzamFarooq ")
aza <- azamf
for(i in 1:148){
        for (j in 8:122){
                aza[i,j] <- sum(azamf[i,8:j])
        }
}



x1 <- 1:18
y1 <- c(99,455,868,1172,1416,1627,1768,1915,2035,2194,2304,2420,2518,2641,2741,2845,2939,3014)

t1 <- s

x2 <- 1:39
y2 <- c(5,18,31,44,60,73,84,98,112,126,142,155,166,173,182,185,187,192,194,198,
        203,204,205,205,207,215,215,215,215,215,216,218,221,222,222,222,223,223,224)


costf <- function(t1,t2,t3,x,y){
        cost <- vector()
        if (length(x) != length(y)){
                stop("Error,x & y must be in the same length!")
        }
        
        for (i in 1:length(x)) {
                cost[i] <- CostF(t1,t2,t3,x=x[i],y = y[i])
        }
        Co <- sum(cost)
        return(Co)
}

costf1 <- function(t1,t2,t3,x,y){
        if(length(x) != length(y)){
                stop("Error,x & y must be in the same length!") 
        }
        ar1 <- rep(NA,length(t1)*length(t2)*length(t3))
        ar1 <- array(ar1,dim = c(length(t1),length(t2),length(t3)),dimnames = c("t1","t2","t3"))
        
        for (i in 1:length(t1)){
                for (j in 1:length(t2)){
                        for (k in 1:length(t3)){
                                ar1[i,j,k] <- costf(t1[i],t2[j],t3[k],x,y)
                        }
                }
        }
        return(ar1)
}

t1 = seq(0.01,6.3,by = 0.01);t2 = 0.4;t3 = seq(0.3,2,by = 0.01)
cost1t3 <- costf1(t1 = t1,t2 = t2,t3 = t3,x1,y1)
persp3d(t1,t3,cost1t2,col = "yellow")

plot(t1,cost1t2[,1,],type = "l",ylab = "Cost",mgp = c(2.5,1,0))
plot(t2,cost1t2[550,,],type = "l",ylab = "Cost",mgp = c(2.5,1,0))
plot(t3,cost1t2[550,,],type = "l",ylab = "Cost",mgp = c(2.5,1,0))


