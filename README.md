# scrapbook
This is cli that collect posts of site that is wrote in config yaml using feed or scraping.


## Usage (WIP)

```haskell
>> import Control.Lens ((^.))
>> import Data.Maybe
>> conf <- fromJust <$> readConfig "example/sites.yaml"
>> [site] = fromJust $ mapM toSite (conf ^. #sites)
>> (Right posts) <- collect $ fetch site
>> collect $ writeFeed (fromJust $ conf ^. #feed) posts
Right ()
```
