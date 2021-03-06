+++
title = "Sitemap with Hakyll"
slug = "hakyll-sitemap"
date = "2015-08-30T00:00:00Z"
publishdate = "2015-08-30T00:00:00Z"
+++

Its fairly trivial to configure [Hakyll][hakyll] to generate
sitemaps. Sitemaps helps search engines websites. Similar to any
typical html page, create a template - `templates/sitemap.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
$for(entries)$
    <url>
        <loc>$host$$url$</loc>
        <changefreq>weekly</changefreq>
        $if(lastmod)$<lastmod>$lastmod$</lastmod>$endif$
        <priority>0.8</priority>
    </url>
$endfor$
</urlset>
```

Then, using the templates, create a rule which uses all the pages from
the site as entries. Here is what I do:

```haskell
  create ["sitemap.xml"] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*/*"
           pages <- loadAll "pages/*"
           let allPosts = (return (pages ++ posts))
           let sitemapCtx = mconcat
                            [ listField "entries" pageCtx allPosts
                            , constField "host" host
                            , defaultContext
                            ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
            >>= cleanIndexHtmls
```

This generates [/sitemap.xml](/sitemap.xml) which can be submitted to
search engines for indexing.

[hakyll]: http://jaspervdj.be/hakyll/
[my-compiler]: https://github.com/crodjer/rohanjain.in/blob/master/site.hs
