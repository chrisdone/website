--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import           Control.Exception
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List
import           System.FilePath
import           Data.String
import           Control.Monad.IO.Class
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main =
  finally
  (hakyllWith defaultConfiguration $ do
     match "posts/*" $ do
       route $ niceRoute
       compile $
         pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
         loadAndApplyTemplate "templates/default.html" postCtx >>=
         relativizeUrls
     create ["index.html"] $ do
       route idRoute
       compile
         (makeItem "" >>=
          loadAndApplyTemplate "templates/index.html" defaultContext >>=
          loadAndApplyTemplate
            "templates/default.html"
            (constField "title" "Chris Done's Homepage" <> defaultContext))
     create ["posts.html"] $ do
       route indexRoute
       compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         let archiveCtx =
               listField
                 "posts"
                 (dateField "date" "%Y-%m-%d" <> postCtx)
                 (return posts) <>
               constField "title" "Archives" <>
               defaultContext
         makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
           loadAndApplyTemplate "templates/default.html" archiveCtx >>=
           relativizeUrls >>=
           cleanIndexUrls
     match "templates/*" $ compile templateBodyCompiler
     create ["rss.xml"] $ do
         route idRoute
         compile $ do
             let feedCtx = postCtx--  `mappend`
                     -- constField "description" "This is the post description"

             posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
             renderRss myFeedConfiguration feedCtx posts)
  (do T.putStrLn "Rewriting links in rss.xml"
      string <- T.readFile "_site/rss.xml"
      T.writeFile "_site/rss.xml" (T.replace "/index.html" "" string)
      )


postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d" <> defaultContext

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </>  (takeBaseName p)
                                 </> "index.html"
                           where p = toFilePath ident

indexRoute :: Routes
indexRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> (takeBaseName p)
                                 </> "index.html"
                           where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Chris Done's Blog"
    , feedDescription = "Blog all about programming, especially in Haskell since 2008!"
    , feedAuthorName  = "Chris Done"
    , feedAuthorEmail = "blog@chrisdone.com"
    , feedRoot        = "https://chrisdone.com"
    }
