{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)
import Control.Monad (forM_)

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
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler


    -- Render posts list
    match  "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "What crodjer writes")
        >>> setFieldPageList (reverse . chronological)
                "templates/postitem.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "What crodjer writes")
        >>> setFieldPageList (take 5 . reverse . chronological)
                "templates/postitem.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Render some static pages
    forM_ ["about.mkd"] $ \p ->
        match p $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Sitemap
    match  "sitemap.xml" $ route idRoute
    create "sitemap.xml" $ constA mempty
        >>> arr (setField "host" host)
        >>> setFieldPageList (reverse . chronological)
                "templates/postsitemap.xml" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/sitemap.xml"
        >>> relativizeUrlsCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler

    where
        host = "http://www.rohanjain.in"

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh' \
                      \_site/* blog:~/www/hakyll"
    }
