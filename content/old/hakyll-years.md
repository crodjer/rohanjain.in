+++
title = "Categorizing archives by year with Hakyll"
slug = "hakyll-years"
date = "2015-08-31T00:00:00Z"
publishdate = "2015-08-31T00:00:00Z"
+++

Recently, I decided to categorize my posts under directories named by
the year they were created in. For example, this post is placed at:
`posts/2015/hakyll-years.mkd`. I decided to utilize this structure to
also group how archives are listed. Inspired from
[Hakyll's tag functionality](http://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Tags.html)
I wrote my own group by years functionality. This post tries to
explain that. Also, I don't see why a similar logic cannot be used to
do simple pagination.

First, like tags, we need to build the list of years. `buildYears`
(like `buildTags`) will do that:

```haskell
buildYears :: MonadMetadata m => Pattern -> m [(Year, Int)]
buildYears pattern = do
    ids <- getMatches pattern
    return . frequency . (map getYear) $ ids
  where
    frequency xs =  M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

getYear :: Identifier -> Year
getYear = takeBaseName . takeDirectory . toFilePath

```
This now can be used in the site `Rules`:

```haskell
  years <- buildYears "posts/*/*"
```

With this, you have a list of years available for other rules to use.
Now, we build an index of posts by years (similar to `tagsRules`):

```haskell
  forM_ years $ \(year, _)->
      create [yearId year] $ do
         route   idRoute
         compile $ do
           -- Only the posts published in 'year'
           posts <- recentFirst =<< loadAll (fromGlob $ "posts/" ++ year ++"/*")
           let postsCtx = mconcat
                          [ listField "posts" postCtx (return posts)
                          , constField "title" ("Posts published in " ++ year)
                          , defaultContext
                          ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" postsCtx
            >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

yearPath :: Year -> FilePath
yearPath year = "archive/" ++ year ++ "/index.html"

yearId :: Year -> Identifier
yearId = fromFilePath . yearPath
```

We now have year wise archives which can be accessed through paths
like: `/archive/2015/`. Similar to tags, we need to support browsing
through posts by years. `renderYears` does that:

```haskell
-- Extra blaze related imports
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


renderYears :: [(Year, Int)] -> Compiler String
renderYears years = do
  years' <- forM (reverse . sort $ years) $ \(year, count) -> do
      route' <- getRoute $ yearId year
      return (year, route', count)
  return . intercalate ", " $ map makeLink years'

  where
    makeLink (year, route', count) =
      (renderHtml (H.a ! A.href (yearUrl year) $ toHtml year)) ++
      " (" ++ show count ++ ")"
    yearUrl = toValue . toUrl . yearPath
```

Add this to a pages context like this:

```haskell
  create ["index.html"] $ do
         route   idRoute
         compile $ do
           -- rest of the compiler context
           let indexCtx = mconcat
                          [ -- some context
                          , field "years" (\_ -> renderYears years)
                          , defaultContext
                          ]
           -- rest of the compiler
```

`$years$` will be available in the template which will link to year
wise archives with their corresponding per year count.

> See this functionality being used by [this blog][my-compiler].

Also, you'd notice that years here could easily have been various
categories you may want to have in your blog.

[my-compiler]: https://github.com/crodjer/rohanjain.in/blob/master/site.hs
