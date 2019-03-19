--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Data.List
import System.FilePath
import Data.String
import Control.Monad.IO.Class
import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith defaultConfiguration $ do
    match "posts/*" $ do
      route $ niceRoute
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        relativizeUrls
    create ["posts.html"] $ do
      route indexRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" (dateField "date" "%Y-%m-%d" `mappend` postCtx ) (return posts) `mappend`
              constField "title" "Archives" `mappend`

              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
          loadAndApplyTemplate "templates/default.html" archiveCtx >>=
          relativizeUrls >>=
          cleanIndexUrls
    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

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
