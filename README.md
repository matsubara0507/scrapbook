# scrapbook
This is cli that collect posts of site that is wrote in config yaml using feed or scraping.


## Usage (WIP)

```haskell
>> import Control.Lens ((^.))
>> import Data.Maybe
>> conf <- fromJust <$> readConfig "example/sites.yaml"
>> (Right posts) <- collect . fmap concat $ mapM (fetch . toSite) (conf ^. #sites)
>> collect $ writeFeed "example" (fromJust $ conf ^. #feed) posts
Right ()
```

### Command

```
scrapbook.EXE [options] [input-file]
  -o DIR                --output=DIR                 Write output to DIR instead of stdout.
  -t FORMAT, -w FORMAT  --to=FORMAT, --write=FORMAT  Specify output format. default is `markdown`.
```
