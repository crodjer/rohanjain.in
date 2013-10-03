--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List              (sortBy, isSuffixOf,
                                         isPrefixOf)
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

copyFiles = [ "static/img/*"
            , "static/js/*"
            , "404.html"
            , "CNAME"
            , "robots.txt"
            , "favicon.ico"
            , ".htaccess" ]

config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh' _site/ blog:~/www/hakyll"
    , ignoreFile = ignoreFile'
    }

    where
        ignoreFile' path
            | "~" `isPrefixOf` fileName = True
            | ".git" `isPrefixOf` fileName = True
            | ".swp" `isSuffixOf` fileName = True
            | "_deploy" `isSuffixOf` fileName = True
            | otherwise = False
            where
                fileName = takeFileName path

main :: IO ()
main = hakyll site

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
                
  match "posts/*" $ do
         route   $ postCleanRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post-content.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
           let indexCtx =
                   listField "posts" postCtx (return posts) `mappend`
                   constField "title" defaultTitle          `mappend`
                   defaultContext

           makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["posts.html"] $ do
         route   cleanRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*"
           let postsCtx =
                   listField "posts" postCtx (return posts) `mappend`
                   constField "title" "Posts"               `mappend`
                   defaultContext

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

           let sitemapCtx =
                   listField "entries" postCtx (return (pages ++ posts)) `mappend`
                   constField "host" host `mappend`
                   defaultContext

           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
            >>= cleanIndexHtmls

  create ["feed.xml"] $ do
         route   idRoute
         compile $ do
           let feedCtx = postCtx `mappend` bodyField "description"
           posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*" "content"
           renderAtom myFeedConfiguration feedCtx posts
             >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "lastmod" "%Y-%m-%d" `mappend`
    dateField "updated" "%Y-%m-%dT%H:%M:%SZ" `mappend`
    constField "host" host `mappend`

    defaultContext

pageCtx :: Context String
pageCtx = postCtx

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
