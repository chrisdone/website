---
date: 2008-11-14
title: "Haskell Formlets: Composable web form construction and validation"
description: "Haskell Formlets: Composable web form construction and validation"
author: Chris Done
tags: haskell
---

**Note**: This is an archive of an old 2008 post from an old blog.

I think we all saw formlets some months ago when Chris Eidhof posted a
blog entry about Formlets in Haskell. For a reminder, a brief
description follows. Then I will jump straight into examples.

## Description of a formlet

A formlet contains information about how to render itself in mark-up
language, whether values provided to it are valid, what error to show
when those values are invalid, or, if valid, the formlet returns a new
value. Perhaps as a loose model, we can enumerate these as:

* Presentation
* Validation
* Parsing
* Failure
* Success

It is, therefore, a composition of five properties present in web
forms. I contend that keeping these properties specified in the same
place and therefore automatically consistent with eachother, is
something we want, as developers.

A formlet is self-contained and composable. By ‘self-contained’, this
means that all the data needed for a formlet is contained inside its
definition. By ‘composable’, this means that formlets can be used
together without influencing eachother, and that I can make new,
valid, formlets out of existing ones. Composability is something which
Haskell is exceedingly good at[^1], as we will see. The Haskell Formlets
library provides us, very concisely, with a way to use formlets. This
entry discusses some examples of this library. I hope to convince the
reader that this is an excellent way to develop web forms.

## Example 1: A user registration form

Suppose I have a formlet that is a user registration form, called
register. The registration form takes a username and a password, and
the password must be entered twice, in two fields, for confirmation. I
might compose this formlet from two other formlets; user and pass. The
user formlet may simply display a text field, and checks that the
field is not empty. The pass formlet, on the other hand, ought to be
composed of two password entry formlets. Each of those sub-formlets
will perform the task of checking that the password is valid (such as
ensuring that it is greater than six characters in length), and the
password formlet merely needs to check that each of the values
returned from these two are equal.

Let us convince ourselves that we can indeed express this, using
Haskell.

Before defining the formlets, we'll define a type to be returned:

``` haskell
data Registration = Registration { username :: String
                                 , password :: String }
```

### Description of a simple validating formlet

Firstly, for the register formlet I have added type annotations. Types
in Haskell help us understand the behaviour of our code, and here is a
good example. It is a form which displays mark-up of type Html
(provided by Text.XHtml.Strict), it is intended to be ran in some
monad m (or instance of Applicative), and returns a Registration
value.

``` haskell
register :: (Applicative m,Monad m) => Form Html m Registration
```

Next, we can instantly see that register is composed of user and pass.

``` haskell
register = Registration <$> user <*> pass
```

But what are user and pass? I will explain user; the meaning of pass
can then be inferred. We've defined user as displaying a text input
box, with no value i.e. Nothing. The value provided from this form
element is then checked against what is called a Failing[^2], which
ensures that a value is valid, or displays an error.

``` haskell
user :: (Applicative m,Monad m) => Form Html m String
user = F.input Nothing `F.check` F.ensure valid error where
    valid = (>=3). length
    error = "Username must be three characters or longer."

pass :: (Applicative m,Monad m) => Form Html m String
pass = F.password Nothing `F.check` F.ensure valid error where
    valid = (>=6). length
    error = "Password must be six characters or longer."
```

We can study ensure's type briefly:

``` haskell
ensure :: Show a => (a -> Bool) -> String -> a -> Failing a
```

It takes a validating function (e.g. valid), an error message, and a
value to validate. It either returns a Failure with the error message,
or a Success with the value.

Now, the type of check should make sense to us, and this is very
lovely:

``` haskell
check :: Monad m => Form xml m a -> (a -> Failing b) -> Form xml m b
```

We can see that it takes a formlet returning a, it takes a function
which validates a and returns b, finally producing a formlet which
returns b. What we have, now, is a function which wraps a validation
around a formlet, producing a new formlet.

### Correction of the registration formlet

Of course, the definition of pass is insufficient. It needs to display
two fields, and check them both, and ensure that their values are
equal. Let us correct this by defining a new formlet,
passConfirmed. The name means that this password has been confirmed by
the user, by their entering twice. Here is the definition[^3]:

``` haskell
passConfirmed :: (Applicative m,Monad m) => Form Html m String
passConfirmed = fst <$> (passwords `F.check` F.ensure equal error) where
    passwords = (,) <$> pass <*> pass
    equal (a,b) = a == b
    error = "The entered passwords do not match!"
```

Let us study what has been written here, using types to help us:

``` haskell
passConfirmed :: (Applicative m,Monad m) => Form Html m String
passConfirmed = fst <$> validPasswords where

    validPasswords :: (Applicative m,Monad m) => Form Html m (String,String)
    validPasswords = passwords `F.check` F.ensure equal error

    passwords :: (Applicative m,Monad m) => Form Html m (String,String)
    passwords = (,) <$> pass <*> pass

    equal :: (String,String) -> Bool
    equal (a,b) = a == b

    error = "The entered passwords do not match!"
```

The validPasswords is what could be described as a wrapper around
passwords, which validates the values, and simply returns them if
valid (i.e. equal). passwords simply takes two valid passwords, and
puts them in a tuple.

Here we must recognise something special! Composability, ladies and
gentlemen! Our passConfirmed formlet does not have to care about
whether the passwords are six characters or longer, because the pass
formlets have done this for us!

We can now go back to our register formlet and update the code:

``` haskell
register :: (Applicative m,Monad m) => Form Html m Registration
register = Registration <$> user <*> passConfirmed
```

### Adding labels: wrapping mark-up

Right now, despite being very lovely, the display of our widgets would
be less than presentable. There are no labels on the form inputs!
Neither the user nor the pass are labelled. They are also not
paragraphed, in the mark-up. Therefore let us write a function which
will take a formlet and stick a label and a paragraph around it.

``` haskell
label l = F.plug (\xhtml -> X.p << (X.label << (l ++ ": ") +++ xhtml))
```

What does the plug function do? Let us study its type:

``` haskell
plug :: (Monad m, Plus xml) => (xml -> xml1) -> Form xml m a -> Form
xml1 m a
```

It's quite simple, of course. It takes a function which transforms
some mark-up, a form, and returns a new form with the changed mark-up.

Therefore our label function simply wraps some XHTML around the
existing formlet. Now, we can use it.

``` haskell
user :: (Applicative m,Monad m) => Form Html m String
user = input `F.check` F.ensure valid error where
    input = "Username" `label` F.input Nothing
    valid = (>=3). length
    error = "Username must be three characters or longer."
```

We now have a proper label for the user input field. We ought to now
confirm that this is indeed the case by running our code. ...Finally!

### Running a Formlet

In order to run our formlet, we need to use the runFormState
function. Studying its type, we see that it takes an environment, a
prefix for the form's element names, and the formlet itself. Finally,
it returns a tuple of the return success or failure of the form,
mark-up which ought to be displayed, and the form content type, which
we are not interested in.

``` haskell
runFormState :: Monad m
             => Env
             -> String
             -> Form xml m a
             -> (m (Failing a),m xml,FormContentType)

type Env = [(String, Either String File)]
```

We are interested only in the String form of values in the
environment.

Let us confirm that our code outputs what has been described:

``` haskell
*Register> :load "/var/www/chrisdone/blog/db/static/Register.hs"
[1 of 1] Compiling Register ( Register.hs, interpreted )
Ok, modules loaded: Register.
*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["chris","mypassword","mypassword"]
*Register> let (result,xml,_) = runFormState env "" register
*Register> putStrLn . X.prettyHtmlFragment =<< xml
<p>
   <label>
      Username:
   </label>
   <input type="text" name="input0" id="input0" value="chris" />
</p>
<input type="password" name="input1" id="input1" value="mypassword" />
<input type="password" name="input2" id="input2" value="mypassword" />

*Register>
```

Here we have provided runFormState with a basic environment; providing
a value for the username and passwords. The result demonstrates that
our label wrapper does indeed work as described.

Finally, we can add labels to our password inputs:

``` haskell
passConfirmed :: (Applicative m,Monad m) => Form Html m String
passConfirmed = fst <$> passwords `F.check` F.ensure equal error where
    passwords = (,) <$> pass "Password" <*> pass "Password (confirm)"
    equal (a,b) = a == b
    error = "The entered passwords do not match!"

pass :: (Applicative m,Monad m) => String -> Form Html m String
pass caption = input `F.check` F.ensure valid error where
    input = caption `label` F.password Nothing
    valid = (>=6). length
    error = "Password must be six characters or longer."
```

We have simply added an extra argument to our pass formlet which takes
an argument for what label to display next to it. Thus we have the
following output:

``` haskell
Prelude> :load "/var/www/chrisdone/blog/db/static/Register.hs"
[1 of 1] Compiling Register ( Register.hs, interpreted )
Ok, modules loaded: Register.
*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["chris","mypassword","mypassword"]
*Register> let (result,xml,_) = runFormState env "" register
*Register> putStrLn . X.prettyHtmlFragment =<< xml
<p>
   <label>
      Username:
   </label>
   <input type="text" name="input0" id="input0" value="chris" />
</p>
<p>
   <label>
      Password:
   </label>
   <input type="password" name="input1" id="input1" value="mypassword" />
</p>
<p>
   <label>
      Password (confirm):
   </label>
   <input type="password" name="input2" id="input2" value="mypassword" />
</p>

*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["chris","mypassword","mypa"]
*Register> let (result,xml,_) = runFormState env "" register in result
Failure ["Password must be six characters or longer."]
*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["chris","mypassword","mypassword-"]
*Register> let (result,xml,_) = runFormState env "" register in result
Failure ["The entered passwords do not match!"]
*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["chris","mypassword","mypassword"]
*Register> let (result,xml,_) = runFormState env "" register in result
Success (Registration {regUser = "chris", regPass = "mypassword"})
```

We now have a full, presentable, composable, validating formlet.

### Complete Haskell source

Below is the source code in full for the reader to read, test and/or
run:

``` haskell
module Register where

import Control.Applicative
import Network.CGI
import Text.Formlets
import Text.XHtml.Strict.Formlets (Form)
import qualified Text.XHtml.Strict.Formlets as F
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((<<),(+++))

data Registration = Registration { regUser :: String
                                 , regPass :: String }
                                 deriving Show

register :: Form Html IO Registration
register = Registration <$> user <*> passConfirmed

user :: (Applicative m,Monad m) => Form Html m String
user = input `F.check` F.ensure valid error where
    input = "Username" `label` F.input Nothing
    valid = (>=3). length
    error = "Username must be three characters or longer."

passConfirmed :: (Applicative m,Monad m) => Form Html m String
passConfirmed = fst <$> passwords `F.check` F.ensure equal error where
    passwords = (,) <$> pass "Password" <*> pass "Password (confirm)"
    equal (a,b) = a == b
    error = "The entered passwords do not match!"

pass :: (Applicative m,Monad m) => String -> Form Html m String
pass caption = input `F.check` F.ensure valid error where
    input = caption `label` F.password Nothing
    valid = (>=6). length
    error = "Password must be six characters or longer."

label :: (X.HTML xml,Monad m,Plus xml)
      => String -> Form xml m a -> Form Html m a
label l = F.plug (\xhtml -> X.p << (X.label << (l ++ ": ") +++ xhtml))
```

Keen eyes will notice that I have changed the type of register:

``` haskell
register :: Form Html IO Registration
```

I have set the monad type to IO. This is merely because we are testing
it in GHCi, which defaults to the IO monad, which is just dandy for
our purposes.[^4]

## Example 2: User registration with monadic validation

We have so far covered how to develop a formlet from an informal
description, improve it and then test it in our Haskell prompt. We
have addressed the four points previously mentioned, i.e. we have
worked with how formlets are presented in mark-up, we have worked with
how they validate input, how they fail on invalid input, and how they
succeed on valid input. Furthermore, we have demonstrated to
ourselves, albiet in a limited way, that all of these things can be
composed.

So far, our validation has been pure. A formlet takes a value and
validates it purely functionally. This is completely
acceptable. Indeed, initially the Haskell Formlets library was only
pure. However, we now have access to monadic validation! Yes! We shall
briefly discuss why this is a good, and then demonstrate with an
example.

Consider a customer database; we want to use this database in our
registration form. Suppose we want to check to see if the username
already exists in the database. If it does, the form fails, otherwise
it returns a registration which can then be sent off to some other
function we don't care about right now. This means that when
validation occurs, it ought to have the ability to behave differently
for the same input. We need to impurely talk to the database in order
to complete the validation. But why do the formlets need to be impure?
Why not run the whole form, and then compare the returned values
against a database? Because then the whole model of composability is
broken. It is no longer a formlet that validates inputs. It is a
formlet that validates some inputs, and then breaks the abstraction
when it gets a bit hairy. We want to be able to have a username
formlet that has everything necessary in its description contained
within its definition. We are now going to experiment with this
notion.

Let us continue from where we left off from Example 1.

### Create a simple database to use

For this example, we'll play it safe and use Sqlite3.

``` haskell
import Database.HDBC
import Database.HDBC.Sqlite3
```

We shall create a customer table with some entries to work with, with
md5 hashed passwords:

``` haskell
Prelude> :set prompt "Haskell> "
Haskell> :set -XOverloadedStrings
Haskell> :m + Data.Digest.Pure.MD5 Data.ByteString.Lazy.Char8
Haskell> md5 "haskell4life"
Loading package syb ... linking ... done.
Loading package base-3.0.3.0 ... linking ... done.
Loading package array-0.2.0.0 ... linking ... done.
Loading package containers-0.2.0.0 ... linking ... done.
Loading package bytestring-0.9.1.4 ... linking ... done.
Loading package binary-0.4.4 ... linking ... done.
Loading package pureMD5-0.2.4 ... linking ... done.
8afba4d177e7162510b9edf0139d1e8c
Haskell> md5 "expert programmer"
c60ce6776f224600bb33e463aa68d138
Haskell>
```

The OverloadedStrings option lets us write string literals which can
be provided as String or ByteString.[^5]

``` haskell
chris@chrisdesktop:/var/www/chrisdone/blog/db/static$ sqlite3 --version
3.4.2
chris@chrisdesktop:/var/www/chrisdone/blog/db/static$ sqlite3 customer.db
SQLite version 3.4.2
Enter ".help" for instructions
sqlite> CREATE TABLE `customer` (`id` INTEGER PRIMARY_KEY
AUTO_INCREMENT, `username` TINY BLOB, `password` BLOB);
sqlite> INSERT INTO `customer` (`username`, `password`) VALUES
('Peyton `Simon` Jones', '085b1b14b7212a61ab8bffe89b02c254');
sqlite> INSERT INTO `customer` (`username`, `password`) VALUES
('Christopher Done', 'c60ce6776f224600bb33e463aa68d138');
sqlite> SELECT * FROM `customer`;
|Christopher Done|c60ce6776f224600bb33e463aa68d138
|Peyton `Simon` Jones|085b1b14b7212a61ab8bffe89b02c254
sqlite> .quit
chris@chrisdesktop:/var/www/chrisdone/blog/db/static$
And now ensuring we have access from Haskell's library:
Haskell> :load "/var/www/chrisdone/blog/db/static/Register.hs"
[1 of 1] Compiling Register ( Register.hs, interpreted )
Ok, modules loaded: Register.
Haskell> con <- connectSqlite3 "customer.db"
Haskell> getTables con
["customer"]
Haskell> quickQuery con "SELECT * FROM `customer`" []
[[SqlNull,SqlString "Christopher Done",
  SqlString "c60ce6776f224600bb33e463aa68d138"],
[SqlNull,SqlString "Peyton `Simon` Jones",
 SqlString "085b1b14b7212a61ab8bffe89b02c254"]]
Haskell> quickQuery con "SELECT * FROM `customer`
WHERE `username` LIKE 'Christopher Done';" []
[[SqlNull,SqlString "Christopher Done",
  SqlString "c60ce6776f224600bb33e463aa68d138"]]
Haskell>
```

### Validating against an SQL database

Let us now return to studying how to impurely validate in a
formlet. We notice that the types thus far have all talked about a
monad type, which we have ignored so far. Now we can make use of it:

``` haskell
checkM :: Monad m => Form xml m a -> (a -> m (Failing b)) -> Form xml m b
ensureM :: (Monad m, Show a) => (a -> m Bool) -> String -> a -> m (Failing a)
```

These functions are monadic equivalents of check and
ensure. Therefore, with this in mind, let us consider our definition
of user.

``` haskell
user :: (Applicative m,Monad m) => Form Html m String
user = input `F.check` F.ensure valid error where
    input = "Username" `label` F.input Nothing
    valid = (>=3). length
    error = "Username must be three characters or longer."
```

I think that this formlet is a fine piece of code. Indeed, we do not
need to change it at all. Instead, let us define a new formlet which
wraps around this one, called uniqueUser:

``` haskell
uniqueUser :: Connection -> Form Html IO String
```

In order to query against the database we will need to pass the
connection handle along. Or will we? I propose a ReaderT monad!

``` haskell
import Control.Monad.Reader
type SQLM = ReaderT Connection IO
```

Our computation also needs to be an instance of Applicative to work
with Formlets, so we define an instance for all ReaderT types:

``` haskell
instance Monad m => Applicative (ReaderT s m) where
    pure = return; (<*>) = ap
```

We'll define a simple utility function which will take the connection
from the monad and use it for a query:

``` haskell
query :: String -> [SqlValue] -> SQLM [[SqlValue]]
query statement values = do
  conn <- ask
  liftIO $ quickQuery conn statement values
```

A demonstration might be something like:

``` haskell
*Register> runReaderT (query "SELECT * FROM `customer`" []) con
[[SqlNull,SqlString "Christopher Done",
  SqlString "c60ce6776f224600bb33e463aa68d138"],
 [SqlNull,SqlString "Peyton `Simon` Jones",
 SqlString "085b1b14b7212a61ab8bffe89b02c254"]]
```

Therefore we can make a very neat definition of uniqueUser:

``` haskell
uniqueUser :: Form Html SQLM String
uniqueUser = user `F.checkM` F.ensureM valid error where
valid name = null <$> query statement [toSql name]
statement = "SELECT * FROM `customer` WHERE `username` LIKE ?"
error = "Username already exists in the database!"
```

I warn the reader of potential heart failure, excretions or other
effects due to viewing the next section. Let us now test our monadic
validating formlet:

``` haskell
*Register> :load "/var/www/chrisdone/blog/db/static/Register.hs"
[1 of 1] Compiling Register ( Register.hs, interpreted )
Ok, modules loaded: Register.
*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["chris","mypassword","mypassword"]
*Register> con <- connectSqlite3 "customer.db"
*Register> let (result,xml,_) = runFormState env "" register
           in runReaderT result con
Success (Registration {regUser = "chris", regPass = "mypassword"})
*Register> let env = map (("input"++) . show) [0..] `zip`
                     map Left ["christopher done","mypassword","mypassword"]
*Register> let (result,xml,_) = runFormState env "" register
           in runReaderT result con
Failure ["Username already exists in the database!"]
*Register>
```

I shall summarise what has been covered in this section. We have
familiarised ourselves with monadic version of check and ensure. We
have happily combined pure and impure formlets, and kept each formlet
contained, doing one thing well. 'Tis the beauty of abstraction in
programming at its most clear. We have established how to run a
formlet with a monad such as ReaderT, and we can see how this might be
done with StateT, for instance.

## Example 3: Custom form inputs

Earlier, we touched upon producing custom mark-up for a formlet, by
wrapping around an existing one. We will now create our own (simple)
form input from scratch; that which is not provided in the
Text.XHtml.Formlets library; a checkbox.

A checkbox can only return two values, checked or unchecked, therefore
Bool is completely satisfactory as a return value for our formlet. To
help us think about the type, let's look at an existing form input's
type:

``` haskell
radio :: Monad m
      => [(String,String)] -> Maybe String -> XHtmlForm m String
```

It takes a list of (name,value) pairs, a default value, and returns a
choice of radio buttons.

It is worth noting that the type XHtmlForm is simply an alias for
specifying the Text.XHtml.Strict.html type, defined as type XHtmlForm
m a = Form Html m a.

Our type should be as follows:

``` haskell
checkbox :: Monad m
         => String -> Maybe String -> XHtmlForm m String
```

Take, a caption for the checkbox, maybe a default value, and return a
checkbox which returns "yes" or "no".

To implement a form input, we need the input' function:

``` haskell
input' :: Monad m
       => (String -> String -> xml) -> Maybe String -> Form xml m String
```

input' takes a function that takes a name for the element in markup,
and a value, input' also takes maybe default value for the element,
and finally returns a formlet.

``` haskell
checkbox :: Monad m => String -> Maybe String -> XHtmlForm m String
checkbox caption def = input' box def where
    box name value = X.input ! ([X.thetype "checkbox", X.name name] ++ checked)
                     +++ caption
        where checked | value == "yes" || value == "on" = [X.checked]
                      | otherwise  = []
```

It is quite simple. There is nothing fancy to be done. Merely produce
the correct markup. We can test it in GHCi to confirm:

``` haskell
*Register> let env = map (("input"++) . show) [0..] `zip` map Left ["yes"]
*Register> let (result,xml,_) =
             runFormState env "" (checkbox "Send me spam?" Nothing)
           in do r <- result; x <- xml; return (r,x)
(Success "yes",<input type="checkbox"
name="input0" checked="checked" />Send me spam?)
*Register> let env = map (("input"++) . show) [0..] `zip` map Left ["no"]
*Register> let (result,xml,_) =
             runFormState env "" (checkbox "Send me spam?" Nothing)
           in do r <- result; x <- xml; return (r,x)
(Success "no",<input type="checkbox"
name="input0"/>Send me spam?)
*Register>
```

Of course, we have only demonstrated a very simple example. However,
most form elements are simple. The point here is that one can use
arbitary markup in a formlet and then compose that with any
other. Consider a clever Javascript colour selector, and such things
like that.

## Formlets in a real CGI program

We have now covered the lovely features of the Haskell
Formlets. Finally, I have put our demonstration code into practise in
a live server application which simply accepts registrations and lists
the usernames. You can view the page and view the raw source code or a
syntax highlighted version. Forgive the messy code; it was written in
about ten minutes; the code around it isn't really important. It is
the formlets themselves.

**UPDATE: 11 April 09:** Also, I have an example of using formlets in a
real project with time constraints etc.

## Summary

I think I have covered, quite fully, the idea of formlets. Formlets are composable pieces which contain (1) presentation, (2) validation, (3) parsing, (4) success and (5) failure. Formlets are useful because these five points are kept in one place and thus consistent, without any manual ‘synching’. We can compose formlets, and a formlet’s definition contains all the required information. We can customise the presentation of existing formlets, or create our own new input methods. A self-contained formlet can validate with and perform side-effects, in a safe, composable manner. Formlets parse validated values into proper program values (such as the Registration type). Formlets are an excellent example of the kind of abstractions that Haskellers use all the time.

## Notes

Please email me about any typing mistakes or inconsistencies that you
notice.

[^1]: Formlets, Parsec, Text.XHtml, the various monads, are some of my
      favourite examples.

[^2]: This is a type, defined in Control.Applicative.Error, with a
      definition that will make clear to you why it is used:

         data Failing a = Success a | Failure [ErrorMsg]

[^3]: Which I wrote in one go, compiled, and found that it worked
      first time. I am clearly reaching Haskell Satori.

[^4]: One could easily leave it ambiguous, using the
      NoMonomorphismRestriction pragma, thus allowing us to use register in
      the IO monad, or the CGI monad, etc.

[^5]: “GHC supports overloaded string literals. Normally a string
      literal has type String, but with overloaded string literals enabled
      (with `-XOverloadedStrings`) a string literal has type `(IsString a) =>
      a`.” See the GHC documentation for `-XOverloadedStrings` for more
      information.
