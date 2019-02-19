library(googlesheets)
gs_ls()

name = gs_title('mastertotal')
data = gs_read(ss=name)
