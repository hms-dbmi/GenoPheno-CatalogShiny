install.packages('rsconnect')
library( rsconnect )
rsconnect::setAccountInfo(name='avillachlab',
                          token='1FAC8309DFDAAF16DD9E11B061A55BCA',
                          secret='UKEqrmncJb8VFGXC57b8HeqxdFnyEY06/uRLl097')
rsconnect::deployApp('/Users/alba/Desktop/geno-pheno-CatalogShinyOnline/', account = 'avillachlab')

#if you remove the app from shinyIo then before deploying it again you have 
#to remove the folder "rsconnect"