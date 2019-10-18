forLoop [] f = return()
forLoop (x:xs) f = (f x) >> forLoop xs f