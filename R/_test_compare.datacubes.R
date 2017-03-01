.test_compare.datacubes<-function(){



    new <- fread("tests/data/new.csv")
    old <- fread("tests/data/old.csv")
    changed <- fread("tests/data/changed.csv")

    a <- compare.datacubes(new, old)


    return(a)
}
