# read the exported transactions from Quicken as character strings
txn <- read.csv("2017-04-18-assante-transactions-table-bella-open.csv", colClasses = "character")

# convert the Type column to a Factor datatype
txn$Type <- as.factor(txn$Type)
# I want to compute the cost basis so ignore the Payment/Deposit transactions
txn <- txn[txn$Type != "Payment/Deposit",]

# convert the Date column to a Date datatype
txn$Date <- as.Date(txn$Date)

# remove commas and dollar signs from columns that contain currency and convert them to numeric datatype
txn$Shares <- as.numeric(gsub("[$,]", "", txn$Shares))
txn$Share.Balance <- as.numeric(gsub("[$,]", "", txn$Share.Balance))

# the Description field which contains strings like "472.363 shares @ 11.810006"
# remove commas and dollar signs, but this is probably not necessary
txn$Description <- gsub("[$,]", "", txn$Description)
# extract the share price from the Description because Quicken does not provide it for dividend reinvestment transactions
txn$Price <- as.numeric(sub("^.* shares @ ","",txn$Description))

# the change in the cost basis due to a transaction is Cost = Shares * Price
txn$Cost <- txn$Shares * txn$Price

# the cost basis is the cumulative cost of each security
# txn$Cost.Basis <- cumsum(txn$Cost) # sum over securities

# sort the rows by Date
txn <- txn[order(txn$Date),]
txn$Security.Payee <- as.factor(txn$Security.Payee)
txn$Cost.Basis <- NA
for (security in levels(txn$Security.Payee)) txn[txn$Security.Payee == security, "Cost.Basis"] <- cumsum(txn[txn$Security.Payee == security, "Cost"])

# the current market value is the value of all the shares
txn$Market.Value <- txn$Share.Balance * txn$Price

# the capital gain is the market value less the cost basis, which is negative in the case of a capital loss
txn$Capital.Gain <- txn$Market.Value - txn$Cost.Basis

# write the augmented file out
write.csv(txn, file = "2017-04-18-assante-transactions-table-bella-open-out.csv", row.names = FALSE)