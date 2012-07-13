{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), arr, (***))
import Data.Monoid (mempty, mconcat)
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad (forM_)
import System.FilePath
import Text.Pandoc (writerTableOfContents, writerTemplate, writerStandalone)

import Hakyll

main :: IO ()
main = hakyllWith config $ do

    -- Compress CSS
    match "static/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy root files and directories
    forM_ copyFiles $ \p ->
        match p $ do
            route   idRoute
            compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ postRoute `composeRoutes` cleanURL
        compile $ defaultCompiler
            >>> arr (setField "host" host)
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> renderModificationTime "lastmod" "%Y-%m-%d"
            >>> renderModificationTime "updated" "%Y-%m-%dT%H:%M:%SZ"
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> arr (copyBodyToField "description")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"


    match  "./posts.html" $ route  cleanURL
    create "./posts.html" $ constA mempty
        >>> arr (setField "title" defaultTitle)
        >>> requireAllA "posts/*" postList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" defaultTitle)
        >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
        >>> requireAllA "posts/*" ( id *** arr (latest 3) >>> postList )
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"


    forM_ staticPages $ \p ->
        match p $ do
            route   $ setRoot `composeRoutes` cleanURL
            compile $ defaultCompiler
                >>> arr (setField "host" host)
                >>> renderModificationTime "lastmod" "%Y-%m-%d"
                >>> applyTemplateCompiler "templates/default.html"

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Sitemap
    match  "sitemap.xml" $ route idRoute
    create "sitemap.xml" $ constA mempty
        >>> arr (setField "host" host)
        >>> requireAllA "posts/*" postListSitemap
        >>> requireAllA "pages/*" pageListSitemap
        >>> applyTemplateCompiler "templates/sitemap.xml"

    -- Read templates
    match "templates/*" $ compile templateCompiler

    match  "feed.xml" $ route idRoute
    create "feed.xml" $
        requireAll_ "posts/*"
            >>> arr (reverse . chronological)
            >>> arr (map stripIndexLink)
            >>> renderAtom myFeedConfiguration

    where
        staticPages = [ "pages/*.md"
                      , "pages/*.mkd"
                      , "pages/*.mkdn"
                      , "pages/*.mdown"
                      , "pages/*.markdown"
                      , "pages/*.pandoc"
                      , "pages/*.pdc"
                      , "pages/*.lhs" 
                      , "pages/*.html" ]

        copyFiles = [ "static/img/*"
                    , "static/js/*"
                    , "404.html"
                    , "robots.txt"
                    , "favicon.ico"
                    , ".htaccess" ]

-- compilers
--------------------------------------------------------------------------------
defaultCompiler :: Compiler Resource (Page String)
defaultCompiler = pageCompilerWith
                    defaultHakyllParserState
                    defaultHakyllWriterOptions
                    { writerTableOfContents = True
                    , writerTemplate = tocTemplate
                    , writerStandalone = True }

    where
        tocTemplate = "$if(toc)$" ++ toc ++  "\n$endif$\n$body$"
        toc = "<strong id=\"$idprefix$TOC\">Table of contents</strong>$toc$"

-- custom routes
--------------------------------------------------------------------------------

postRoute :: Routes
postRoute = customRoute $ drop 11 . stripTopDir

setRoot :: Routes
setRoot = customRoute stripTopDir

stripTopDir :: Identifier a -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath

cleanURL :: Routes
cleanURL = customRoute fileToDirectory

fileToDirectory :: Identifier a -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

toIndex :: Routes
toIndex = customRoute fileToIndex

fileToIndex :: Identifier a -> FilePath
fileToIndex = flip combine "index.html" . dropFileName . toFilePath

-- misc functions
--------------------------------------------------------------------------------
renderTagCloud' :: Compiler (Tags String) String
renderTagCloud' = renderTagCloud tagIdentifier 100 160

renderTagList' :: Compiler (Tags String) String
renderTagList' = renderTagList tagIdentifier

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> arr (map stripIndexLink)
        >>> pageListCompiler recentFirst "templates/postitem.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

latest:: Int -> [Page b] -> [Page b]
latest n = take n . reverse . chronological

stripIndexLink :: Page a -> Page a
stripIndexLink = changeField "url" dropFileName

postList :: Compiler (Page String, [Page String]) (Page String)
postList = buildList "posts" "templates/postitem.html"

postListSitemap :: Compiler (Page String, [Page String]) (Page String)
postListSitemap = buildList "posts" "templates/postsitemap.xml"

pageListSitemap :: Compiler (Page String, [Page String]) (Page String)
pageListSitemap = buildList "pages" "templates/postsitemap.xml"

buildList :: String -> Identifier Template -> Compiler (Page String, [Page String]) (Page String)
buildList field template = setFieldA field $
    arr (reverse . chronological)
        >>> arr (map stripIndexLink)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody


config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --delete --checksum -ave 'ssh' _site/ blog:~/www/hakyll"
    , ignoreFile = ignoreFile'
    }

    where
        ignoreFile' path
            | "~" `isPrefixOf` fileName = True
            | ".swp" `isSuffixOf` fileName = True
            | otherwise = False
            where
                fileName = takeFileName path

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
