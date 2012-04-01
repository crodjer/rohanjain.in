{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)

import Hakyll

main :: IO ()
main = hakyll $ do
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
        >>> requireAllA "posts/*" addPostListHtml
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "What crodjer writes")
        >>> requireAllA "posts/*" (id *** arr (take 5 . reverse . chronological ) >>> addPostListHtml)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Sitemap
    match  "sitemap.xml" $ route idRoute
    create "sitemap.xml" $ constA mempty
        >>> arr (setField "host" host)
        >>> requireAllA "posts/*" addPostListSitemap
        >>> applyTemplateCompiler "templates/sitemap.xml"
        >>> relativizeUrlsCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler

    where
        host = "http://www.rohanjain.in"

-- | Auxiliary compiler: generate a post list from a list of given posts and
-- template, and add it to the current page under @$posts@
--
addPostList :: Identifier Template -> Compiler (Page b, [Page String]) (Page b)
addPostList template = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

addPostListHtml :: Compiler (Page String, [Page String]) (Page String)
addPostListHtml = addPostList "templates/postitem.html"

addPostListSitemap :: Compiler (Page String, [Page String]) (Page String)
addPostListSitemap = addPostList "templates/postsitemap.xml"
