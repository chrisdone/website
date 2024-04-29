---
date: 2009-02-10
title: Applicative and ConfigFile, HSQL
description: Applicative and ConfigFile, HSQL
author: Chris Done
tags: haskell
---


I've been noticing some totally sweet uses of Applicative cropping up in my day to day coding. I figured I'd show off some neat uses of it I had recently.

## ConfigFile

Also worth mentioning is the ConfigFile library which is pretty sweet.

The idea is to run in the ErrorT monad to pull specific configuration items from the file. Here an example taken from the documentation reads the configuration file into cp and uses the get function to extract values from sections.

``` haskell
import Data.ConfigFile
import Control.Monad.Error

main = do
          rv <- runErrorT $
              do
              cp <- join $ liftIO $ readfile empty "/etc/passwd"
              let x = cp
              liftIO $ putStrLn "In the test"
              nb <- get x "DEFAULT" "nobody"
              liftIO $ putStrLn nb
              foo <- get x "DEFAULT" "foo"
              liftIO $ putStrLn foo
              return "done"
          print rv
```

This is pretty flexible and allows us to make one awesome abstraction. Make an Applicative instance!

``` haskell
instance Monad m => Applicative (ErrorT C.CPError m) where
    pure = return; (<*>) = ap
```

And this lets me write the config parser in a really nice way:

``` haskell
getConf :: FilePath -> IO (Either (C.CPErrorData,String) Blog)
getConf filePath = runErrorT $ do
  contents <- liftIO $ readFile filePath
  config <- C.readstring C.emptyCP contents
  let get = C.get config "BLOG"
  Blog <$> get "name" <*> get "root"
       <*> (read <$> get "css") <*> get "entries" <*> get "html"
       <*> get "author"
       <*> return False
       <*> get "date"
```

Double checking the definition of Blog, we can see where everything fits:

``` haskell
data Blog = Blog
    { blogName     :: String -- e.g. Chris Done's Blog
    , blogRoot     :: String -- /blog
    , blogCSS      :: [String] -- e.g. ["style.css","highlight.css"]
    , blogEntries  :: FilePath
    , blogHtml     :: FilePath
    , blogAuthor   :: String
    , blogForce    :: Bool
    , blogDate     :: String -- date format e.g.
                             -- "%A %d %b, %Y" makes "Tuesday 10 Feb, 2009"
    } deriving (Read,Show)
```

We see that I use return False for blogForce (an option to force refreshing of all pages regardless of modification date), and that is because I don't want it to be specified in the configuration file.

Of course, not only is this more pretty, we can also make values optional by making an Alternative instance. We see a real use of this in my Lojbot source:

``` haskell
instance Monad m => Alternative (ErrorT C.CPError m) where
    empty = mzero; (<|>) = mplus
```

And now in the readConfig function, we see it's slightly more complex than Blogination’s; I'm taking from multiple sections. Do you see the res function?

``` haskell
readConfig :: String -> IO (Either (C.CPErrorData,String) Config)
readConfig filePath = runErrorT $ do
  config <- join $ liftIO $ C.readfile C.emptyCP filePath
  let irc = C.get config "IRCBOT"
      port = C.get config "IRCBOT" "port"
      misc = C.get config "MISC"
      res k f = misc k <|> liftIO (getDataFileName f)
  Config <$> irc "nick" <*>  irc "nickservpass" <*> irc "server" <*> port
         <*> (irc "chans" >>= tryGet "invalid channel list")
         <*> (irc "log" >>= tryGet "invalid log specification")
         <*> res "jbov" "jbovlaste.db"
         <*> res "mlismu" "fatci.txt"
         <*> (misc "mode" >>= tryGet "invalid mode")
         <*> (read <$> irc "interval")
      where tryGet msg = list (fail msg) (return . fst . head) . reads
``` haskell

The res function either pulls an optional setting from the MISC section, or alternatively uses the getDataFileName function to find out a filename held in our cabal package.

``` haskell
      $Haskell$
      res k f = misc k <|> liftIO (getDataFileName f)
```

Isn't that totally sweet?

## HSQL

Of course, I've used this pattern elsewhere; in HSQL it works out nicely, too, and I have added some fancier bits to it:

A straight-forward function to grab a field value from a Statement (which is like a row in HSQL).

``` haskell
field :: (SqlBind a) => String -> Statement -> IO a
field = flip MySQL.getFieldValue
```

Another function to read a Haskell value from an SQL field:

``` haskell
readfield :: Read a => String -> Statement -> IO a
readfield f = fmap read . flip MySQL.getFieldValue f
```

Here is the nice part.

Originally, I would pull fields from a statement using the monadic field, e.g.

``` haskell
getOrder :: Statement -> IO Order
getOrder s = do id <- field "id"
                name <- field "name"
-- etc..
                return $ Order id name -- ...
```

But then I realised I could make this into a use of applicative. You see the pattern, right?

Here I define an instance of Applicative for the ReaderT monad transformer, so that I can stick a Statement in it to be used later on when extracting fields.

``` haskell
type StatementM = ReaderT Statement IO
instance Monad m => Applicative (ReaderT a m) where
    (<*>) = ap; pure = return

```

getFields is just an alias for runReaderT giving us the explicit StatementM type:

``` haskell
getFields :: StatementM a -> Statement -> IO a
getFields = runReaderT
```

Finally, rather than typing out field statement "blah" or readfield all the time, I realised I could abstract that by putting the Statement in the ReaderT and then define some operators which work with this statement under the hood (I ain't apologising for use of unicode operators :-P):

``` haskell
a · b = a <*> getField b; infixl 4 ·
a ·. b = a <*> (read <$> getField b); infixl 4 ·.
a ← b = a <$> getField b; infixl 4 ←
getField b = do s <- ask; lift $ field b s
```

You can see that they are basically just performing the same as <$> and <*> except that they work with the Statement value so we don't have to write it out. Here it is in use in a real project:

``` haskell
getOrderItem :: Statement -> IO OrderItem
getOrderItem = getFields $
  OrderItem ← "id" · "name" · "added" · "price" · "quantity"
            · "size" · "size_metric"

getOrder :: Statement -> IO Order
getOrder = getFields $
  Order ← "id" · "name" · "items" · "order_total" · "delivery"
        · "total" · "created" · "card" · "billto" · "shipto"
        · "level" · "completed" · "code"
``` haskell

Looks gorgeous, right? I figured I could do this for the ConfigFile stuff, but thought better of it because it needs to be slightly more flexible. The reason this abstraction works so well for HSQL is that all the fields are extracted in the same way.

We can see tonnes of other uses for Control.Applicative elsewhere. Formlets, of course, are a prime example. It also works nicely in monadic parsers like Parsec. I expect tonnes of other uses of it cropping up in my daily coding.
