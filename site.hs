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
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "What crodjer writes")
        >>> requireAllA "posts/*" (id *** arr (take 5 . reverse . chronological ) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Render Atom feed
    match  "atom.xml" $ route idRoute
    create "atom.xml" $
        requireAll_ "posts/*" >>> renderAtom feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Rohan's Blog"
    , feedDescription = "A blog about hacking and sometimes personal"
    , feedAuthorName  = "Rohan Jain"
    , feedRoot        = "http://www.rohanjain.in"
    }
