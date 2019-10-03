install.packages('rsconnect')
library( rsconnect )
rsconnect::setAccountInfo(name='testestest',
                          token='C1137C7EAE5DD22EF7F81A211B89FD58',
                          secret='p7vU0syZOdGfA0oUKIodjkHP5BJ/XnbJ3YkiSk5M')
rsconnect::deployApp('/Users/alba/Desktop/biobankCatalogShinyOnline/')

#if you remove the app from shinyIo then before deploying it again you have 
#to remove the folder "rsconnect"