sink("/Users/Elena/Desktop/R/Reports/testing", append=TRUE, split=TRUE)

pdf("/Users/Elena/Desktop/R/Reports/db.os.memory.kernel1.pdf")
metrics<-read.table("/Users/Elena/Desktop/R/DATA/db.biz.items.bids.txt", header=TRUE, sep = "");
detection(metrics)

metrics<-read.table("/Users/Elena/Desktop/R/DATA/db.biz.items.listing.txt", header=TRUE, sep = "");
detection(metrics)
dev.off()

