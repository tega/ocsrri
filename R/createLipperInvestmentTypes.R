#' Create a data.frame with the information on which investment types are in Lipper
#'
#' Create a data.frame with the information on which investment types are in Lipper. This
#' function is mainly used to identify the investment objects whose SRRI is available in the
#' Lipper database.
#'
#' @param asOfDate a valid date used to extract the situation as of asOfDate.
#'
#' @return a data frame with the columns "Value", "Description", "DownloadFromLipper"  
#' and "Investment Category". The column "Description" contains the name of the investment type
#' while "DownwloadFromLipper" contains 0 or 1 depending on the availability of the give 
#' investment type in the Lipper database.
#'
#' @examples
#' data.df <- createLipperInvestmentTypes(as.Date("2021-08-30"))
#'
#' @export

createLipperInvestmentTypes <- function(asOfDate) {

  result.l <- list()

  invType.df <- ocutil::stringToDataFrame(
      "Value,Description,InLipper,Investment Category
          #------/-------------/--------------------/--------------------- 
          840,Cash Accounts,0,Accounts
          850,Invest Accounts,0,Accounts
          870,Margin Accounts,0,Accounts
          1390,Receivables Dividends,0,Accounts
          1400,Receivables Interests,0,Accounts
          1410,Receivables Withholding Taxes,0,Accounts
          1420,Receivables Security Transactions,0,Accounts
          1430,Receivables Subscriptions,0,Accounts
          1440,Receivables Others,0,Accounts
          1450,Payables Management Fees,0,Accounts
          1460,Payables Custody Account Charges,0,Accounts
          1470,Payables Security Transactions,0,Accounts
          1480,Payables Taxes,0,Accounts
          1490,Payables Redemptions,0,Accounts
          1500,Payables Others,0,Accounts
          1510,Payables Advisory Fees,0,Accounts
          10101,Private cash accounts,0,Accounts
          170,Mid & Long Term Notes,0,Bonds
          175,Credit-Linked Notes,0,Bonds
          180,Mid Term CDs,0,Bonds
          190,Ordinary Bonds,0,Bonds
          192,Rating-Linked Bonds,0,Bonds
          195,Bonds Subordinated,0,Bonds
          200,Treasury Bonds,0,Bonds
          205,Zero Bonds,0,Bonds
          210,Agency Bonds,0,Bonds
          215,Bonds Participation Warrants,0,Bonds
          220,Municipal Bonds,0,Bonds
          230,Corporate Bonds,0,Bonds
          240,Inst Bonds,0,Bonds
          250,Mortgage Backed Securities,0,Bonds
          255,Inflation-Linked Bonds,0,Bonds
          260,Option Bonds,0,Bonds
          265,Floater,0,Bonds
          267,Fix-To-Float,0,Bonds
          270,Deposit Certificates,0,Bonds
          280,Savings Certificates,0,Bonds
          290,Promissory Notes,0,Bonds
          295,Govt. Bonds,0,Bonds
          300,Lottery Bonds,0,Bonds
          305,Other Bonds,0,Bonds
          1060,Mortgage Bonds,0,Bonds
          1110,Asset Backed Securities,0,Bonds
          10000,Default Bonds,0,Bonds
          10001,CDO,0,Bonds
          10003,Perpetual Bonds,0,Bonds
          1050,Other Commodities,0,Commodities
          310,Convertible Bonds,0,Convertibles
          320,Convertible Subordinated,0,Convertibles
          1160,Credit Default Swaps,0,Credit Derivatives
          1162,CDS Single Name,0,Credit Derivatives
          1164,CDS Index,0,Credit Derivatives
          480,Stock Forwards,0,Forwards
          490,Currency Forwards,0,Forwards
          495,Currency Forwards NDF,0,Forwards
          500,Index Forwards,0,Forwards
          510,Precious Metal Forwards,0,Forwards
          520,Interest Rate Forwards,0,Forwards
          530,Other Forwards,0,Forwards
          330,Stock Funds,1,Funds
          340,Bond Funds,1,Funds
          350,Convertible Bond Funds,1,Funds
          360,Money Market Funds,1,Funds
          370,Precious Metal Funds,1,Funds
          380,Mortgage Funds,1,Funds
          390,Real Estate Funds,1,Funds
          395,Private Equity Funds,1,Funds
          400,Other Funds,1,Funds
          402,Hedge Funds,1,Funds
          404,Portfolio Funds,1,Funds
          1150,Exchange Traded Funds,1,Funds
          10002,ETF,1,Funds
          10102,L/S Credit,1,Funds
          10103,Emerging Bonds Local,1,Funds
          10104,HY Short Term EUR,1,Funds
          10105,Emerging Bonds Short Term,1,Funds
          10106,Bonds CHF,1,Funds
          10107,Emerging Bonds Gov.,1,Funds
          10108,Convertible Bonds Global,1,Funds
          10109,Equities Switzerland,1,Funds
          10110,Equities Global,1,Funds
          10111,Equities CH High Dvd.,1,Funds
          10112,Equities CH SM+MID,1,Funds
          10113,Equities USA,1,Funds
          10114,Equities World,1,Funds
          10115,Equities Europe,1,Funds
          10117,ABS Return Equities,1,Funds
          10118,Merger Arbitrage,1,Funds
          10119,ABS Return Multi Asset,1,Funds
          10120,Equities Emerging,1,Funds
          10121,Short Term Bonds EUR,1,Funds
          10122,High Yield Bonds,1,Funds
          540,Stock Futures,0,Futures
          550,Currency Futures,0,Futures
          560,Index Futures,0,Futures
          570,Precious Metal Futures,0,Futures
          580,Interest Rate Futures,0,Futures
          585,Bond Futures,0,Futures
          590,Other Futures,0,Futures
          595,Futures Liquidity,0,Futures
          1040,Commodity Futures,0,Futures
          1350,Volatility Futures,0,Futures
          1360,Dividend Futures,0,Futures
          10100,Contract for difference,0,Futures
          325,Reverse Convertibles,0,Hybrids
          720,GROI,0,Hybrids
          730,PIP,0,Hybrids
          735,GOAL,0,Hybrids
          740,PEP,0,Hybrids
          745,TORO,0,Hybrids
          750,Other Hybrids,0,Hybrids
          1120,Index Certificates,0,Hybrids
          1190,Discount Certificates,0,Hybrids
          1200,Bonus Certificates,0,Hybrids
          1210,Capped Bonus Certificates,0,Hybrids
          1220,Outperformance Certificates,0,Hybrids
          1230,Capped Outperformance Certificates,0,Hybrids
          1240,Capped Outperformance Bonus Certificates,0,Hybrids
          1250,Barrier Discount Certificates,0,Hybrids
          1260,Reverse Bonus Certificates,0,Hybrids
          1270,Capped Reverse Bonus Certificates,0,Hybrids
          1280,Tracker Certificates,0,Hybrids
          1290,Twin Win Certificates,0,Hybrids
          1300,Capped Twin Win Certificates,0,Hybrids
          1310,Outperformance Bonus Certificates,0,Hybrids
          1380,Exchange Traded Commodity,0,Hybrids
          115,Call Loan,0,Loans
          125,Fixed Loan,0,Loans
          135,Fiduciary Fixed Loan,0,Loans
          141,Fiduciary Call Loan,0,Loans
          143,Credits,0,Loans
          147,Loans,0,Loans
          980,Policy Loans,0,Loans
          990,Other Loans,0,Loans
          1170,Uncertified Loans,0,Loans
          50,Foreign Book Accounts,0,Money Market
          60,Dom Book Accounts,0,Money Market
          70,Treasury Bills,0,Money Market
          80,Commercial Papers,0,Money Market
          90,Bankers Acceptances,0,Money Market
          100,Short Term CDs,0,Money Market
          110,Call Deposits,0,Money Market
          120,Fixed Deposits,0,Money Market
          130,Fixed Fiduciary Deposits,0,Money Market
          140,Fiduciary Call Deposits,0,Money Market
          150,Short Term Notes,0,Money Market
          155,Cash,0,Money Market
          160,Other MM Instruments,0,Money Market
          1320,Technical Currency,0,Money Market
          950,Mortgages Variable,0,Mortgages
          960,Mortgages Fixed,0,Mortgages
          970,Mortgages Portfolio,0,Mortgages
          660,Stock Options,0,Options
          670,Currency Options,0,Options
          680,Index Options,0,Options
          690,Precious Metal Options,0,Options
          700,Interest Rate Options,0,Options
          710,Other Options,0,Options
          1090,Options on Bonds,0,Options
          1100,Options on Commodities,0,Options
          1130,Options on Futures,0,Options
          1370,Volatility Options,0,Options
          900,Policies,0,Others
          910,Others,0,Others
          920,Pro Memoria,0,Others
          930,Participations,0,Others
          1180,Guarantees,0,Others
          760,Gold,0,Precious Metals
          770,Iridium,0,Precious Metals
          780,Palladium,0,Precious Metals
          790,Platinum,0,Precious Metals
          800,Rhodium,0,Precious Metals
          810,Silver,0,Precious Metals
          820,Ruthenium,0,Precious Metals
          830,Other Precious Metals,0,Precious Metals
          940,Real Estate,0,Real Estate
          1010,Repos,0,Repos
          1020,Reverse Repos,0,Repos
          1030,Other Repos,0,Repos
          10,Ordinary Stocks,0,Stocks
          20,Preferred Stocks,0,Stocks
          25,Rights,0,Stocks
          30,Non-voting Stocks,0,Stocks
          35,Private Equities,0,Stocks
          37,Dividend Right Certificates,0,Stocks
          40,Other Stocks,0,Stocks
          45,Stocks Pro Memoria,0,Stocks
          47,ADR,0,Stocks
          415,Fund Investment Trusts,0,Stocks
          417,Real Estate Investment Trusts,0,Stocks
          1140,Stock Certificates,0,Stocks
          10116,World Equities,0,Stocks
          880,Interest Rate Swap Fixed,0,Swaps
          885,Interest Rate Swap Floating,0,Swaps
          890,Currency Swap Fixed,0,Swaps
          895,Currency Swap Floating,0,Swaps
          896,Total Return Swap Equity,0,Swaps
          897,Total Return Swap Fixed,0,Swaps
          898,Total Return Swap Floating,0,Swaps
          410,Stock Investment Trusts,1,Trusts
          420,Mortgage Investment Trusts,1,Trusts
          430,Property Investment Trusts,1,Trusts
          440,MMkt Investment Trusts,1,Trusts
          450,Bond Investment Trusts,1,Trusts
          460,Conv Bond Investment Trusts,1,Trusts
          470,Other Investment Trusts,1,Trusts
          0,Unknown,0,Unknown
          10123,Hybrid,0,Unknown
          10124,Private Debt,0,Unknown
          600,Stock Warrants,0,Warrants
          610,Currency Warrants,0,Warrants
          620,Index Warrants,0,Warrants
          630,Precious Metal Warrants,0,Warrants
          640,Interest Rate Warrants,0,Warrants
          650,Other Warrants,0,Warrants
          1070,Warrants on Bonds,0,Warrants
          1080,Warrants on Commodities,0,Warrants"

  )
  result.l[["2010-01-01"]] <- invType.df

  invType.df <- ocutil::stringToDataFrame(
      "Value,Description,InLipper,Investment Category
          #------/-------------/--------------------/--------------------- 
          840,Cash Accounts,0,Accounts
          850,Invest Accounts,0,Accounts
          870,Margin Accounts,0,Accounts
          1390,Receivables Dividends,0,Accounts
          1400,Receivables Interests,0,Accounts
          1410,Receivables Withholding Taxes,0,Accounts
          1420,Receivables Security Transactions,0,Accounts
          1430,Receivables Subscriptions,0,Accounts
          1440,Receivables Others,0,Accounts
          1450,Payables Management Fees,0,Accounts
          1460,Payables Custody Account Charges,0,Accounts
          1470,Payables Security Transactions,0,Accounts
          1480,Payables Taxes,0,Accounts
          1490,Payables Redemptions,0,Accounts
          1500,Payables Others,0,Accounts
          1510,Payables Advisory Fees,0,Accounts
          10101,Private cash accounts,0,Accounts
          170,Mid & Long Term Notes,0,Bonds
          175,Credit-Linked Notes,0,Bonds
          180,Mid Term CDs,0,Bonds
          190,Ordinary Bonds,0,Bonds
          192,Rating-Linked Bonds,0,Bonds
          195,Bonds Subordinated,0,Bonds
          200,Treasury Bonds,0,Bonds
          205,Zero Bonds,0,Bonds
          210,Agency Bonds,0,Bonds
          215,Bonds Participation Warrants,0,Bonds
          220,Municipal Bonds,0,Bonds
          230,Corporate Bonds,0,Bonds
          240,Inst Bonds,0,Bonds
          250,Mortgage Backed Securities,0,Bonds
          255,Inflation-Linked Bonds,0,Bonds
          260,Option Bonds,0,Bonds
          265,Floater,0,Bonds
          267,Fix-To-Float,0,Bonds
          270,Deposit Certificates,0,Bonds
          280,Savings Certificates,0,Bonds
          290,Promissory Notes,0,Bonds
          295,Govt. Bonds,0,Bonds
          300,Lottery Bonds,0,Bonds
          305,Other Bonds,0,Bonds
          1060,Mortgage Bonds,0,Bonds
          1110,Asset Backed Securities,0,Bonds
          10000,Default Bonds,0,Bonds
          10001,CDO,0,Bonds
          10003,Perpetual Bonds,0,Bonds
          1050,Other Commodities,0,Commodities
          310,Convertible Bonds,0,Convertibles
          320,Convertible Subordinated,0,Convertibles
          1160,Credit Default Swaps,0,Credit Derivatives
          1162,CDS Single Name,0,Credit Derivatives
          1164,CDS Index,0,Credit Derivatives
          480,Stock Forwards,0,Forwards
          490,Currency Forwards,0,Forwards
          495,Currency Forwards NDF,0,Forwards
          500,Index Forwards,0,Forwards
          510,Precious Metal Forwards,0,Forwards
          520,Interest Rate Forwards,0,Forwards
          530,Other Forwards,0,Forwards
          330,Stock Funds,1,Funds
          340,Bond Funds,1,Funds
          350,Convertible Bond Funds,1,Funds
          360,Money Market Funds,1,Funds
          370,Precious Metal Funds,1,Funds
          380,Mortgage Funds,1,Funds
          390,Real Estate Funds,1,Funds
          395,Private Equity Funds,1,Funds
          400,Other Funds,1,Funds
          402,Hedge Funds,1,Funds
          404,Portfolio Funds,1,Funds
          1150,Exchange Traded Funds,1,Funds
          10002,ETF,1,Funds
          10102,L/S Credit,1,Funds
          10103,Emerging Bonds Local,1,Funds
          10104,HY Short Term EUR,1,Funds
          10105,Emerging Bonds Short Term,1,Funds
          10106,Bonds CHF,1,Funds
          10107,Emerging Bonds Gov.,1,Funds
          10108,Convertible Bonds Global,1,Funds
          10109,Equities Switzerland,1,Funds
          10110,Equities Global,1,Funds
          10111,Equities CH High Dvd.,1,Funds
          10112,Equities CH SM+MID,1,Funds
          10113,Equities USA,1,Funds
          10114,Equities World,1,Funds
          10115,Equities Europe,1,Funds
          10117,ABS Return Equities Funds,1,Funds
          10118,Merger Arbitrage,1,Funds
          10119,ABS Return Multi Asset,1,Funds
          10120,Equities Emerging,1,Funds
          10121,Short Term Bonds EUR,1,Funds
          10122,High Yield Bonds,1,Funds
          540,Stock Futures,0,Futures
          550,Currency Futures,0,Futures
          560,Index Futures,0,Futures
          570,Precious Metal Futures,0,Futures
          580,Interest Rate Futures,0,Futures
          585,Bond Futures,0,Futures
          590,Other Futures,0,Futures
          595,Futures Liquidity,0,Futures
          1040,Commodity Futures,0,Futures
          1350,Volatility Futures,0,Futures
          1360,Dividend Futures,0,Futures
          10100,Contract for difference,0,Futures
          325,Reverse Convertibles,0,Hybrids
          720,GROI,0,Hybrids
          730,PIP,0,Hybrids
          735,GOAL,0,Hybrids
          740,PEP,0,Hybrids
          745,TORO,0,Hybrids
          750,Other Hybrids,0,Hybrids
          1120,Index Certificates,0,Hybrids
          1190,Discount Certificates,0,Hybrids
          1200,Bonus Certificates,0,Hybrids
          1210,Capped Bonus Certificates,0,Hybrids
          1220,Outperformance Certificates,0,Hybrids
          1230,Capped Outperformance Certificates,0,Hybrids
          1240,Capped Outperformance Bonus Certificates,0,Hybrids
          1250,Barrier Discount Certificates,0,Hybrids
          1260,Reverse Bonus Certificates,0,Hybrids
          1270,Capped Reverse Bonus Certificates,0,Hybrids
          1280,Tracker Certificates,0,Hybrids
          1290,Twin Win Certificates,0,Hybrids
          1300,Capped Twin Win Certificates,0,Hybrids
          1310,Outperformance Bonus Certificates,0,Hybrids
          1380,Exchange Traded Commodity,0,Hybrids
          115,Call Loan,0,Loans
          125,Fixed Loan,0,Loans
          135,Fiduciary Fixed Loan,0,Loans
          141,Fiduciary Call Loan,0,Loans
          143,Credits,0,Loans
          147,Loans,0,Loans
          980,Policy Loans,0,Loans
          990,Other Loans,0,Loans
          1170,Uncertified Loans,0,Loans
          50,Foreign Book Accounts,0,Money Market
          60,Dom Book Accounts,0,Money Market
          70,Treasury Bills,0,Money Market
          80,Commercial Papers,0,Money Market
          90,Bankers Acceptances,0,Money Market
          100,Short Term CDs,0,Money Market
          110,Call Deposits,0,Money Market
          120,Fixed Deposits,0,Money Market
          130,Fixed Fiduciary Deposits,0,Money Market
          140,Fiduciary Call Deposits,0,Money Market
          150,Short Term Notes,0,Money Market
          155,Cash,0,Money Market
          160,Other MM Instruments,0,Money Market
          1320,Technical Currency,0,Money Market
          950,Mortgages Variable,0,Mortgages
          960,Mortgages Fixed,0,Mortgages
          970,Mortgages Portfolio,0,Mortgages
          660,Stock Options,0,Options
          670,Currency Options,0,Options
          680,Index Options,0,Options
          690,Precious Metal Options,0,Options
          700,Interest Rate Options,0,Options
          710,Other Options,0,Options
          1090,Options on Bonds,0,Options
          1100,Options on Commodities,0,Options
          1130,Options on Futures,0,Options
          1370,Volatility Options,0,Options
          900,Policies,0,Others
          910,Others,0,Others
          920,Pro Memoria,0,Others
          930,Participations,0,Others
          1180,Guarantees,0,Others
          760,Gold,0,Precious Metals
          770,Iridium,0,Precious Metals
          780,Palladium,0,Precious Metals
          790,Platinum,0,Precious Metals
          800,Rhodium,0,Precious Metals
          810,Silver,0,Precious Metals
          820,Ruthenium,0,Precious Metals
          830,Other Precious Metals,0,Precious Metals
          940,Real Estate,0,Real Estate
          1010,Repos,0,Repos
          1020,Reverse Repos,0,Repos
          1030,Other Repos,0,Repos
          10,Ordinary Stocks,0,Stocks
          20,Preferred Stocks,0,Stocks
          25,Rights,0,Stocks
          30,Non-voting Stocks,0,Stocks
          35,Private Equities,0,Stocks
          37,Dividend Right Certificates,0,Stocks
          40,Other Stocks,0,Stocks
          45,Stocks Pro Memoria,0,Stocks
          47,ADR,0,Stocks
          415,Fund Investment Trusts,0,Stocks
          417,Real Estate Investment Trusts,0,Stocks
          1140,Stock Certificates,0,Stocks
          10116,World Equities,0,Stocks
          880,Interest Rate Swap Fixed,0,Swaps
          885,Interest Rate Swap Floating,0,Swaps
          890,Currency Swap Fixed,0,Swaps
          895,Currency Swap Floating,0,Swaps
          896,Total Return Swap Equity,0,Swaps
          897,Total Return Swap Fixed,0,Swaps
          898,Total Return Swap Floating,0,Swaps
          410,Stock Investment Trusts,1,Trusts
          420,Mortgage Investment Trusts,1,Trusts
          430,Property Investment Trusts,1,Trusts
          440,MMkt Investment Trusts,1,Trusts
          450,Bond Investment Trusts,1,Trusts
          460,Conv Bond Investment Trusts,1,Trusts
          470,Other Investment Trusts,1,Trusts
          0,Unknown,0,Unknown
          10123,Hybrid,0,Unknown
          10124,Private Debt,0,Unknown
          600,Stock Warrants,0,Warrants
          610,Currency Warrants,0,Warrants
          620,Index Warrants,0,Warrants
          630,Precious Metal Warrants,0,Warrants
          640,Interest Rate Warrants,0,Warrants
          650,Other Warrants,0,Warrants
          1070,Warrants on Bonds,0,Warrants
          1080,Warrants on Commodities,0,Warrants"

  )

  result.l[["2021-11-12"]] <- invType.df

  isValidDate.v <- names(result.l) <= as.character(asOfDate)
  if (!any(isValidDate.v)) stop("Error: the asOfDate is out of range")

  validDate.v <- names(result.l)[isValidDate.v]
  desired.df <- result.l[[ validDate.v[length(validDate.v)] ]]
  return(desired.df)
}
