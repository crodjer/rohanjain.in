{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty, mconcat)
import Control.Monad (forM_)
import System.FilePath

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy javascript
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy images
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy favicon
    match "favicon.ico" $ do
      route   idRoute
      compile copyFileCompiler

    -- Copy robots.txt
    match "robots.txt" $ do
      route   idRoute
      compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setRoot `composeRoutes` cleanURL
        compile $ pageCompiler
            >>> arr (setField "host" host)
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"


    {-match  "./posts.html" $ route $ setRoot `composeRoutes` cleanURL-}
    {-create "./posts.html" $ constA mempty-}
        {->>> arr (setField "title" "What I write")-}
        {->>> requireAllA "posts/*" postList-}
        {->>> applyTemplateCompiler "templates/posts.html"-}
        {->>> applyTemplateCompiler "templates/default.html"-}

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "What I write")
        >>> requireAllA "posts/*" postList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Render some static pages
    forM_ markUpPages $ \p ->
        match p $ do
            route   $ setRoot `composeRoutes` cleanURL
            compile $ pageCompiler
                >>> arr (setField "host" host)
                >>> applyTemplateCompiler "templates/default.html"

    -- Sitemap
    match  "sitemap.xml" $ route idRoute
    create "sitemap.xml" $ constA mempty
        >>> arr (setField "host" host)
        >>> requireAllA "posts/*" postListSitemap
        >>> requireAllA "pages/*" pageListSitemap
        >>> applyTemplateCompiler "templates/sitemap.xml"

    -- Read templates
    match "templates/*" $ compile templateCompiler

    where
        host = "http://www.rohanjain.in"
        markUpPages = [ "pages/*.md"
                      , "pages/*.mkd"
                      , "pages/*.mkdn"
                      , "pages/*.mdown"
                      , "pages/*.markdown"
                      , "pages/*.pandoc"
                      , "pages/*.pdc"
                      , "pages/*.lhs" ]

-- custom routes
--------------------------------------------------------------------------------

setRoot :: Routes
setRoot = customRoute stripTopDir

stripTopDir :: Identifier a -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath

cleanURL :: Routes
cleanURL = customRoute fileToDirectory

fileToDirectory :: Identifier a -> FilePath
fileToDirectory = (flip combine) "index.html" . dropExtension . toFilePath

toIndex :: Routes
toIndex = customRoute fileToIndex

fileToIndex :: Identifier a -> FilePath
fileToIndex = (flip combine) "index.html" . dropFileName . toFilePath

-- misc functions
--------------------------------------------------------------------------------
stripIndexLink :: Page a -> Page a
stripIndexLink = changeField "url" dropFileName

buildTitle :: Page a -> Page a
buildTitle = renderField "title" "pagetitle" (++ " - Ethan Schoonover")

postList :: Compiler (Page String, [Page String]) (Page String)
postList = buildList "posts" "templates/postitem.html"

postListSitemap :: Compiler (Page String, [Page String]) (Page String)
postListSitemap = buildList "posts" "templates/postsitemap.xml"

pageListSitemap :: Compiler (Page String, [Page String]) (Page String)
pageListSitemap = buildList "pages" "templates/pagesitemap.xml"

buildList :: String -> Identifier Template -> Compiler (Page String, [Page String]) (Page String)
buildList field template = setFieldA field $
    arr (reverse . chronological)
        >>> arr (map stripIndexLink)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody


config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh' \
                      \_site/* blog:~/www/hakyll"
    }
