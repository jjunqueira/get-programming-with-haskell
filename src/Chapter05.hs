module Chapter05 where

getRequestURL host apiKey resource id = host ++
  "/" ++
  resource ++
  "/" ++
  id ++
  "?token=" ++
  apiKey

genHostRequestBuilder host =
  (\apiKey resource id -> getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey =
  (\resource id -> hostBuilder apiKey resource id)

genResourceRequestBuilder hostBuilder apiKey resource =
  (\id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk311"

add4 a b c d = a + b + c + d

addXto3 x = (\b c d -> add4 x b c d)

addXYto2 x y = (\c d -> add4 x y c d)

mystery = add4 3

anotherMystery = add4 2 3

exampleUrlBuilder2 = getRequestURL "http://example.com"

myExampleUrlBuilder2 = exampleUrlBuilder2 "1337hAsk311"

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

subtract2 = flip (-) 2
