# scrapbook

[![Hackage](https://img.shields.io/hackage/v/scrapbook.svg?style=flat)](https://hackage.haskell.org/package/scrapbook)
![Build Application](https://github.com/matsubara0507/scrapbook/workflows/Build%20Application/badge.svg)
[![](https://images.microbadger.com/badges/image/matsubara0507/scrapbook.svg)](https://microbadger.com/images/matsubara0507/scrapbook "Get your own image badge on microbadger.com")

This is cli tool that collect posts of site that is wrote in config yaml using feed or scraping.

## Usage

1. clone this repository or add `scrapbook` package to `extra-deps` in `stack.yaml`
2. run `stack install`

e.g.

```
$ stack exec -- scrapbook -o "example" example/sites.yaml
```

### Docker

build docker image:

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/scrapbook . --build-arg local_bin_path=./bin
```

### Command

```
scrapbook [options] [input-file]
  -o DIR                --output=DIR                 Write output to DIR instead of stdout.
  -t FORMAT, -w FORMAT  --to=FORMAT, --write=FORMAT  Specify output format. default is `feed`.
                        --version                    Show version
```

### GHCi

```haskell
>> import Control.Lens ((^.))
>> import Data.Maybe
>> conf <- fromJust <$> readConfig "example/sites.yaml"
>> (Right posts) <- collect . fmap concat $ mapM (fetch . toSite) (conf ^. #sites)
>> collect $ writeFeed "example" (fromJust $ conf ^. #feed) posts
Right ()
```

## Example

see [matsuara0507/scrapbook-example](https://github.com/matsubara0507/scrapbook-example)

## Documentation

How to write config yaml file.

```yaml
# configuration for generating Atom feed (Optional)
feed:
  ## write as site title to Atom feed
  title: "Sample Site Posts"
  ## write as site url to Atom feed
  baseUrl: "https://example.com"
  ## file name (Optional)
  ### if nothing, use same name from input file
  name: atom.xml

# Haskeller's site configuration
sites:
    ## Title of site
  - title: "ひげメモ"
    ## Author of site
    author: matsubara0507
    ## URL of site
    url: https://matsubara0507.github.io
    ## Feed url of site
    ### there are several field to set feed url
    ### `feed` is basic field. This field auto branch to Atom or RSS 2.0.
    feed: https://matsubara0507.github.io/feed
  - title: "Kuro's Blog"
    author: "Hiroyuki Kurokawa"
    url: http://kurokawh.blogspot.com/
    ### `atom` is for Atom feed.  
    atom:
      ### feed url of Atom
      url: http://kurokawh.blogspot.com/feeds/posts/default
      ### set attr as constraint for link on each entry of Atom feed (Optional)
      ### if nothing, choice head. if set multiple attr, conjunction.
      linkAttrs:
        rel: alternate
  - title: "あどけない話"
    author: "kazu-yamamoto"
    url: http://d.hatena.ne.jp/kazu-yamamoto
    ### `rss` is for RSS 2.0 feed.
    ### set feed url.
    rss: http://d.hatena.ne.jp/kazu-yamamoto/rss2
```
