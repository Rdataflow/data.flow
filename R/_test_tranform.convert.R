.test_transform.convert<-function(){


    d<-fread("C:/temp/Daten/Holzproduktion.csv")[,unit:="m3"][]
    key<-names(d)[-7]
    ##
    conv<-data.table(d[1:6,1],conv=1.9/((1:6)%%7+1))
    ##
    conv[]
    d[]
    d[conv,on=names(d)[1]]
    ##
    d.conv<-d[conv,on=names(d)[1]][,c("unit","value","conv"):= list("t",value*conv,NULL)][]
    f<-funion(d,d.conv)


    return(f)
}
