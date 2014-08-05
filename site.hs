--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Data.List              (isSuffixOf, isPrefixOf)
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
            | ".git" `isPrefixOf` fileName = True
            | "~" `isSuffixOf` fileName = True
            | ".swp" `isSuffixOf` fileName = True
            | "_deploy" `isSuffixOf` fileName = True
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

  match (fromList ["about.rst", "contact.markdown"]) $ do
         route   cleanRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "posts/*" $ do
         route   $ postCleanRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post-content.html" (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls
            >>= cleanIndexUrls

  match "pages/*" $ do
         route   $ cleanRoute `composeRoutes` (gsubRoute "pages/" (const ""))
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["index.html"] $ do
         route   idRoute
         compile $ do
           posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
           let indexCtx = mconcat
                          [ listField "posts" (postCtx tags) (return posts)
                          , constField "title" defaultTitle
                          , field "tags" (\_ -> renderTagCloud 100 300 tags)
                          , defaultContext
                          ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["posts.html"] $ do
         route   cleanRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*"
           let postsCtx = mconcat
                   [ listField "posts" (postCtx tags) (return posts)
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
                          [ listField "posts" (postCtx tags) (return posts)
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
           posts <- recentFirst =<< loadAll "posts/*"
           pages <- loadAll "pages/*"

           let allPosts = (return (pages ++ posts))
           let sitemapCtx = mconcat
                            [ listField "entries" (postCtx tags) allPosts
                            , constField "host" host
                            , defaultContext
                            ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
            >>= cleanIndexHtmls

  create ["feed.xml"] $ do
         route   idRoute
         compile $ do
           let feedCtx = (postCtx tags) `mappend` bodyField "description"
           posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*" "content"
           renderAtom myFeedConfiguration feedCtx posts
             >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ tagsField "tags" tags
    , pageCtx
    ]

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

trimmedCleanRoute :: Int -> Routes
trimmedCleanRoute c = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> drop c (takeBaseName p)
                                 </> "index.html"
                            where p = toFilePath ident

postCleanRoute :: Routes
postCleanRoute = trimmedCleanRoute 11 `composeRoutes` (gsubRoute "posts/" (const ""))

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
