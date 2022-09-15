# To install the package on R :

if(!require(devtools)){
    install.packages("devtools")
    library(devtools)
}
install_github("gaussdarwin/vgswrc1")

# To access the package from your library :

library(vgswrc)

# To understand how the package works go to the manual section.
# You can acces the two function manuals in R like this:

?vg_eq()

?vg_pred()

