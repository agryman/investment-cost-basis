# read the exported transactions from Quicken as character strings
tfsa <- read.csv("2017-04-08-assante-transactions-table-bella-tfsa.csv", colClasses = "character")

# keep the columns that are relevant to cost basis of shares
tfsa <- tfsa[,c("Date", "Type", "Description", "Shares", "Share.Balance")]

# convert the Type column to a Factor datatype
tfsa$Type <- as.factor(tfsa$Type)
# I want to compute the cost basis so ignore the Payment/Deposit transactions
tfsa <- tfsa[tfsa$Type != "Payment/Deposit",]

# convert the Date column to a Date datatype
tfsa$Date <- as.Date(tfsa$Date)

# remove commas and dollar signs from columns that contain currency and convert them to numeric datatype
tfsa$Shares <- as.numeric(gsub("[$,]", "", tfsa$Shares))
tfsa$Share.Balance <- as.numeric(gsub("[$,]", "", tfsa$Share.Balance))

# the Description field which contains strings like "472.363 shares @ 11.810006"
# remove commas and dollar signs, but this is probably not necessary
tfsa$Description <- gsub("[$,]", "", tfsa$Description)
# extract the share price from the Description because Quicken does not provide it for dividend reinvestment transactions
tfsa$Price <- as.numeric(sub("^.* shares @ ","",tfsa$Description))

# the change in the cost basis due to a transaction is Cost = Shares * Price
tfsa$Cost <- tfsa$Shares * tfsa$Price

# the cost basis is the cumulative cost
tfsa$Cost.Basis <- cumsum(tfsa$Cost)

# the current market value is the value of all the shares
tfsa$Market.Value <- tfsa$Share.Balance * tfsa$Price

# the capital gain is the market value less the cost basis, which is negative in the case of a capital loss
tfsa$Capital.Gain <- tfsa$Market.Value - tfsa$Cost.Basis
