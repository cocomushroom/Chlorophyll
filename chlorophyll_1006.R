
# 
# doi:10.1093/jxb/ers070
# 
# Chlorophyll a (mg/g)=[(0.1127*A663)-(0.0259*A645)]/g of FW
# Chlorophyll b (mg/g)= )=[(0.229*A645)-(0.0467*A663)]/g of FW
# Total chlorophyll (mg/g)= Chlorophyll a + Chlorophyll b
calculateChlorophyll<- function(data){

    if(ncol(data) != 3){
        stop("Need 3 columns")
    }
    weight <- data[,grep("weight",colnames(data))]
    A663 <- data[,grep("663",colnames(data))]
    A645 <- data[,grep("645",colnames(data))]

    Chl_a <- (0.1127*A663 - 0.0259*A645)/weight
    Chl_b <- (0.229*A645 - 0.0467*A663)/weight
    total <- Chl_a + Chl_b
    return(total)
}


DAY<- c("10", "17", "24")
FUNGI<- c("2T69", "1T38", "1M05")

dataFull0<- read.table("chlorophyll_1006")
dataFull<- apply(dataFull0,2,function(x){
        return( as.numeric(sub("N/A",NA, x) ) )
    })
rownames(dataFull)<- rownames(dataFull0)



dataDate<- vector(length=3, mode="list")
totalChl<- as.data.frame(matrix(nrow=nrow(dataFull), ncol=4, dimnames=list(rownames(dataFull), c(DAY, "index") ) ))
for(i in 1:3){
    dataDate[[i]] <- dataFull[,grep(paste0("day", DAY[i]), colnames(dataFull))]
    totalChl[,i] <- calculateChlorophyll(dataDate[[i]])
}
plot(totalChl[,1])

FungiIndex<- as.factor(c(rep("2T69", 4), rep("1T38", 4), rep("1M05", 4), rep("C", 3)) )
totalChl[,4]<- FungiIndex

png("D1_by_fungi.png")
par(mfrow=c(2,2))
for(i in 1:3){
    plot(as.numeric(totalChl[,4]),totalChl[,i], xaxt="n", xlab="fungi", ylab="Total Chlorophyll", main=paste0("Day_",DAY[i]))
    axis(1, at=as.numeric(totalChl[,4]), labels=totalChl[,4])
}
dev.off()


png("D1_by_date.png")
par(mfrow=c(2,2))
for(i in 1:3){
    rowIndex<- grep(FUNGI[i], rownames(totalChl))
    plot(rep(1:3,each=4), unlist(totalChl[rowIndex,1:3]), xaxt="n", xlab="Day", ylab="Total Chlorophyll", main=paste0(FUNGI[i]), ylim=c(0,0.03))
    axis(1, at=1:3, labels=paste0("Day_",DAY))
}
rowIndex<- grep("C", rownames(totalChl))
plot(rep(1:3,each=3), unlist(totalChl[rowIndex,1:3]), xaxt="n", xlab="Day", ylab="Total Chlorophyll", main=paste0("Control"), ylim=c(0,0.03))
axis(1, at=1:3, labels=paste0("Day_",DAY))

dev.off()


#ANOVA and 2-way ANOVA
data2<- data.frame(chl=unlist(totalChl[,-4]), day=rep(DAY, each=15), fungi=rep(FungiIndex,3) )

summary(aov(chl~day, data=data2))
summary(aov(chl~fungi, data=data2))
summary(aov(chl~day+fungi+day:fungi, data=data2))
