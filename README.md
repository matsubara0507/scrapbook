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
scrapbook [options] [input-file]
  -o DIR                --output=DIR                 Write output to DIR instead of stdout.
  -t FORMAT, -w FORMAT  --to=FORMAT, --write=FORMAT  Specify output format. default is `feed`.
                        --version                    Show version
```

## Example

see [matsuara0507/scrapbook-example](https://github.com/matsubara0507/scrapbook-example)
