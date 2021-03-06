+++
title = "Clean URLs with Hakyll"
slug = "hakyll-clean-urls"
date = "2015-08-30T00:00:00Z"
publishdate = "2015-08-30T00:00:00Z"
+++

The URLs generated by [Hakyll][hakyll], by default also include a
`.html` extension. I have never been a fan of this. When things in
reality are driven by the `Content-Type` header, it is absolutely
redundant.

Hakyll provides all the utilities with which we can get cleaner URLs,
like [this page's](.). For this, I rely on the fact that most of the
web servers automatically serve `/foo/index.html` for the URL
`/foo/`. To generate *clean* paths, I define a custom route -
`cleanRoute`:

```haskell
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident
```

This can now be used in in rule definition:

```haskell
  match "pages/*" $ do
         route   $ cleanRoute
         -- the compiler follows
```

With this, a path say `/pages/about.html` will be generated as
`/pages/about/index.html`, hence solving the generation problem. We
are only partially done though. The links that Hakyll generates will
also include the `/index.html` suffix in every URL. To get rid of that
we define a set of functions:

```haskell
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
```

`cleanIndexUrls` and `cleanIndexHtmls` strip out `/index.html` from
all the anchor tags and complete text respectively. These can be used
over a page's compiler like this:

```haskell
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
            >>= cleanIndexUrls -- cleanup href in all anchor tags.
```

> This functionality is being used by [this blog][my-compiler] and
> [irneh/workforpizza][workforpizza] off which this blog is actually
> based.

[hakyll]: http://jaspervdj.be/hakyll/
[my-compiler]: https://github.com/crodjer/rohanjain.in/blob/master/site.hs
[workforpizza]: https://github.com/irneh/workforpizza/blob/master/site.hs
