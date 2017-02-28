.test_compare.datacubes<-function(){



    new<-fread("C:/temp/test/new.csv")
    old<-fread("C:/temp/test/old.csv")
    changed<-fread("C:/temp/test/changed.csv")
    a<-compare.datacubes(new,old)


    return(a)
}
