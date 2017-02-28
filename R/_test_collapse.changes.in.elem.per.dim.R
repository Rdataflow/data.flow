.test_collapse.changes.in.elem.per.dim<-function(){

    # test#1
    input<-list(Jahr=list(add=c(2014:2016),del=c(2009)),Forstzone=list(add=c("Bern","Zürich","Genf"),del=character()),Kanton=list(add=character(),del=character()))
    output<-"Jahr(+2014+2015+2016-2009)Forstzone(+Bern+Zürich+Genf)"
    test<-collapse.changes.in.elem.per.dim(input)
    stopifnot(test==output)
    ######
    return()

    # demo
    new<-fread("C:/temp/test/new.csv")
    old<-fread("C:/temp/test/old.csv")
    changed<-fread("C:/temp/test/changed.csv")
    a<-compare.datacubes(new,old)

    collapse.changes.in.elem.per.dim(a$elem)

}
