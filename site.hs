--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Data.List              (isSuffixOf, isPrefixOf, isInfixOf)
import           System.FilePath.Posix  (takeBaseName, takeDirectory,
                                         (</>), takeFileName)
import           Control.Monad (forM_)

--------------------------------------------------------------------------------
host::String
host = "http://www.rohanjain.in"

defaultTitle::String
defaultTitle = "The Scroll"

myFeedConfiguration:: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle = "Rohan's Weblog"
    , feedDescription = ""
    , feedAuthorName = "Rohan Jain"
    , feedAuthorEmail = "crodjer@gmail.com"
    , feedRoot = host
    }

copyFiles :: [Pattern]
copyFiles = [ "static/img/*"
            , "static/js/*"
            , "404.html"
            , "robots.txt"
            , "favicon.ico"
            , ".htaccess"
            ]

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh' _site/ blog:~/www/hakyll"
    , ignoreFile = ignoreFile'
    }

    where
        ignoreFile' path
            | "~" `isPrefixOf` fileName = True
            | ".#" `isPrefixOf` fileName = True
            | "~" `isSuffixOf` fileName = True
            | ".swp" `isSuffixOf` fileName = True
            | ".git" `isInfixOf` path = True
            | "_cache" `isInfixOf` path = True
            | otherwise = False
            where
                fileName = takeFileName path

main :: IO ()
main = hakyllWith config site

site :: Rules ()
site = do
  forM_ copyFiles $ \pattern->
      match pattern $ do
         route   idRoute
         compile copyFileCompiler

  match "static/css/*" $ do
         route   idRoute
         compile compressCssCompiler

  tags <- buildTags "posts/*/*" (fromCapture "tags/*.html")
  let postCtx = mconcat [tagsField "tags" tags, pageCtx]

  match "posts/*/*" $ do
        route   $ postCleanRoute
        compile $ pandocCompiler
           >>= loadAndApplyTemplate "templates/post-content.html" postCtx
           >>= saveSnapshot "content"
           >>= loadAndApplyTemplate "templates/post.html" postCtx
           >>= loadAndApplyTemplate "templates/default.html" postCtx
           >>= relativizeUrls
           >>= cleanIndexUrls

  match "pages/*" $ do
         route   $ cleanRoute `composeRoutes` (gsubRoute "pages/" (const ""))
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["index.html"] $ do
         route   idRoute
         compile $ do
           posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*/*"
           let indexCtx = mconcat
                          [ listField "posts" postCtx (return posts)
                          , constField "title" "Recent posts"
                          , field "tags" (\_ -> renderTagCloud 100 300 tags)
                          , defaultContext
                          ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["archive.html"] $ do
         route   cleanRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*/*"
           let postsCtx = mconcat
                   [ listField "posts" postCtx (return posts)
                   , constField "title" "Posts"
                   , defaultContext
                   ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" postsCtx
            >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  tagsRules tags $ \tag pattern -> do
         let title = "Posts tagged " ++ tag

         -- Copied from posts, need to refactor
         route cleanRoute
         compile $ do
           posts <- recentFirst =<< loadAll pattern
           let postsCtx = mconcat
                          [ listField "posts" postCtx (return posts)
                          , constField "title" title
                          , defaultContext
                          ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" postsCtx
            >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

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

  create ["feed.xml"] $ do
         route   idRoute
         compile $ do
           let feedCtx = pageCtx `mappend` bodyField "description"
           posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*/*" "content"
           renderAtom myFeedConfiguration feedCtx posts
             >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

pageCtx :: Context String
pageCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , modificationTimeField "lastmod" "%Y-%m-%d"
    , dateField "updated" "%Y-%m-%dT%H:%M:%SZ"
    , constField "host" host
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

-- custom routes
--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = trimmedCleanRoute 0

postCleanRoute :: Routes
postCleanRoute = trimmedCleanRoute 11
 `composeRoutes` (gsubRoute "posts/[0-9]{4}/" (const ""))

trimmedCleanRoute :: Int -> Routes
trimmedCleanRoute c = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> drop c (takeBaseName p)
                                 </> "index.html"
                            where p = toFilePath ident

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"
