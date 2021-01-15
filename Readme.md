## Running the provided code
To run any of the tests 
```
runhaskell Main.hs ./tests/t#.4TH
```
And compare to turned in .out file for the each test.

## Situations/Assumptions

I changed one of the tests given to us namely this one:
from
```
evalOut "." ([Id "x"], "") `shouldBe` ([],"x")
```
to
```
evalOut "." ([Id "x"], "") `shouldBe` ([],"\"x\"")
```
I did this because I think it made the output type more clear. For example If we print something like 12 STR . 
with the given "." evalout
```
12 
```
but I think it makes more sense if we get 
``` 
"12" 
```
So i changed the given evalout from 
```
evalOut "." (Id x:tl, out) = (tl, out ++ x)
```
to 
```
evalOut "." (Id x:tl, out) = (tl, out ++ show x)
```
this adds the quotes and adds clarity to the Conversion cases. Though I can see that it might be redundant for strings I felt the clarity was more important.