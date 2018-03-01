# scrapbook
This is cli that collect posts of site that is wrote in config yaml using feed or scraping.


## Usage (WIP)

```haskell
>> conf <- fromJust <$> readConfig "example/sites.yaml"
>> [site] = fromJust $ mapM toSite conf
>> collect $ fetch site
Right [...]
```
