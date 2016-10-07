FUNGI<- c("1M05", "1T38", "2T69")
FUNGI_C<- c(FUNGI, "C")

dataFull0<- read.csv("giaroots_results.csv", header=T)

dataFull<- dataFull0[,-19]

data<- dataFull[,c("Network.Surface.Area","Network.Volume")]
id<- sub("/Volumes/Elements/pictures/(.+_)bottom/(.+)/Img.+JPG", "\\1\\2", dataFull[,"source_id"], ignore.case=T)
id<- sub("0919", "0911", id)

rownames(data)<- id
# 
# d1<- data[grep(0911,id),]
# d2<- data[grep(1005,id),]
# 
# d1_f_index<- as.factor(gsub("0911_([[:alnum:]]+)_?.", "\\1", rownames(d1)) )
# d2_f_index<- as.factor(gsub("1005_([[:alnum:]]+)_?.", "\\1", rownames(d2)) )

## all plots
d_f<- as.factor(gsub("([[:digit:]]_[[:alnum:]]+)_?.", "\\1", rownames(data)) )
# png("D2_area.png")
plot(as.numeric(d_f), data[,1], xaxt="n", xlab="Date_Fungi", ylab="Network Surface Area")
axis(1, at=as.numeric(d_f), label=d_f)
# dev.off()

# png("D2_volume.png")
plot(as.numeric(d_f), data[,2], xaxt="n", xlab="Date_Fungi", ylab="Network Volume")
axis(1, at=as.numeric(d_f), label=d_f)
# dev.off()

# ## old mean method
# mean_area<- vector(length=4) 
# mean_volume<- vector(length=4)
# 
# for(f in 1:4){
# #     f_index<- grep(FUNGI_C[f], d_f)
#     d1<- grep(paste0("0911_",FUNGI_C[f]), id)
#     d2<- grep(paste0("1005_",FUNGI_C[f]), id)
#     mean_area[f]<- mean(data[d2,1])/mean(data[d1,1])
#     mean_volume[f]<- mean(data[d2,2])/mean(data[d1,2])
# }

## Ratio
remove_index<- which(id %in% c("0911_1M05_5","0911_2T69_6") )
data2<- data[-remove_index,]
id2<- rownames(data2)



mean_area<- vector(length=length(id2)/2) 
mean_volume<- vector(length=length(id2)/2)
# 
for(f in 1:4){
#     f_index<- grep(FUNGI_C[f], d_f)
    d1<- grep(paste0("0911_",FUNGI_C[f]), id2)
    d2<- grep(paste0("1005_",FUNGI_C[f]), id2)
    mean_area[d1]<- (data2[d2,1]/data2[d1,1])
    mean_volume[d1]<- (data2[d2,2]/data2[d1,2])
}

f_index<- as.factor(gsub("[[:digit:]]+_([[:alnum:]]+)_?.", "\\1", id2) )[1:18]


png("D2_area_ratio.png")
plot(as.numeric(f_index), mean_area, xaxt="n", xlab="Fungi", ylab="Network Surface Area")
axis(1, at=as.numeric(f_index), label=f_index)
dev.off()

png("D2_volume_ratio.png")
plot(as.numeric(f_index), mean_volume, xaxt="n", xlab="Fungi", ylab="Network Volume")
axis(1, at=as.numeric(f_index), label=f_index)
dev.off()


