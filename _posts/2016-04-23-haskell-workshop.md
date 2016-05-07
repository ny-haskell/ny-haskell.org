---
layout: page
date: 2016-03-26 21:00:00 -0400
title: Haskell Workshop on Reflex FRP
author: Kat Chuang
github_username: katychuang
authorurl: http://twitter.com/katychuang
gravatar: 0c64f5404b8787a038e8f6fa9522224d
category: events
---

Today we learned how to get started with a Reflex and Reflex-Dom with Ali Abrar.

<!--excerpt-->

First, install Reflex platform.

```
$ git clone git@github.com:reflex-frp/reflex-platform.git
$ ./try-reflex
```

During the installation we chatted with Ali and Ryan (creator of ReflexFRP) to understand more about the background of this library and how it compares to other open source tools available.

### How does reflex compare to other frameworks?

There's two parts: reflex, reflex-dom

**Reflex** gives frp functionalities. has datatypes (or building blocks)
deals with interactive applications

**Reflex-dom** is a binding to web browsers api that is based on reflex. Takes callback driven, highly stateful thing that browser supports and turns it into reflex instead.

There are very few FRP dom specific comparisons for use in the browser. Elm is similar but not Haskell so you can't use libraries found in Hackage. 

### How does reflex compare to reactjs?

Reactjs inspired approach is not purely frp, limited with inherent Javascript limitations.

The advantage of using ghcjs is that it gives you access to all of hackage. You can use any haskell library, and so with Reflex-Dom you have access on the front end to all the same stuff as backend applications.

Reflex itself can be used for a wide number of applications (websites, games, etc). Reflex-Dom can be used to produce javascript files. For example full stack web applications, or single web applications.

You can also use reflex-dom to create a native binary. Can produce web-gtk or similar variants, and also produce a distributable.

### **Documentation Resources**

Here's how you can look for documentation.

* [Hackage](https://hackage.haskell.org/package/reflex-dom)
* [Quick Ref for Reflex][2]
* [Quick Ref for Reflex-Dom][3]

---

### **Basic Tutorial on compiling with ghcjs**

After everyone installed reflex-platform, we began with coding.

**Step 1** Create a blank file: `workshop.hs`

**Step 2** Imports `import Reflex.Dom` at the top

**Step 3** Write a main function

mainWidget will take over main body of html page. 

Once you have mainwidget you can write the html DOM element widgets. The most basic widget is `text`.  Text is the lowest level. It won't put it in a p tag or div tag or anything else. It is just plain text that is shown in the browser.

{% highlight haskell %}
-- workshop.hs
-- entry point for any haskell application is the main function
main :: IO ()
main = mainWidget $ text "hello world"
{% endhighlight %}

A page could be any level of complexity (login page, dashboard, etc.) but for now we start with compiling the basic text. 

**Step 4** In your Nix shell, type `ghcjs workshop.hs`

It creates a folder *workshop.jsexe* with a bunch of js files. Index.html just loads the javascript files that the compiler used. Open the html file in a browser and you'll see what it produces. You can open up the chrome inspector to look at specific DOM tags produced.

#### What are the files produced by ghcjs?
* index.html is the main file to view in the browser
* manifest.webapp is for firefox offline apps (untested)
* all.js is the main file. That's the only one you need for production.. the other js files are combined into that file.


#### What is the build system doing when you type ghcjs?

Ghcjs can also work with the regular cabal system using `cabal --configure ghcjs`. When using reflex-platform, it'll be handled with nix by default so you can make a cabal file and edit the *workon* script to customize for the cabal file instead of the default file provided.

Tip: make reflex-platform a submodule in git so that the version is locked down. (similar to docker but more declarative) so that things may break but you're not forced to update builds.

---

### API Client Tutorial

We're going to use NASA's api, available at [https://api.nasa.gov/api.html#apod](https://api.nasa.gov/api.html#apod)

Let's design a login page with one text input for the API key.

**Text Input with Reflex-Dom**

`textInput` returns a text input. it has a value, dynamic events (key press, focused, etc) but we're only interested in the value right now. it's a dynamic value. We read [the docs](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Input.html) and [quickref][3] to find out the type signatures.

`_textInput_value` gives us back a `Dynamic t String` type

`dynText` takes a `Dynamic String` and returns a dom element.

{% highlight haskell %}
dynText ::      Dynamic String -> m ()
{% endhighlight %}

Adding this to the top ofy our file `{-# LANGUAGUE ScopedTypeVariables #-}`  lets us write the type signatures in workshop.hs.

---

Now with the api key input, we want a button also. Clicking on the button fires an event.

{% highlight haskell %}
button :: String -> m (Event())
{% endhighlight %}

 We include the button along with the text input on our form.

{% highlight haskell %}
workshop :: MonadWidget t m => m ()
workshop = do

  text "hello world"

  t <- textInput def --def provides default input
  let apiKey = _textInput_value t

  --doesn't have event yet, just a static button
  b <- button "Send Request"

  -- this says what happens after button fires, see below for notes
  let apiKeyEvent = tagDyn apiKey b

  --return unit because mainWidget requires the type return
  return () 
{% endhighlight %}

Compile this file with `ghcjs workshop.hs` again.

Now we have defined a dynamic string and an event. When the event fires we want to take the string input and make a request with that value and need the feature that supports that. Looking at reflex quickref again...

{% highlight haskell %}
tagDyn :: Dynamic a -> Event b -> Event a
{% endhighlight %}

tagDyn says it takes value of dynamic and an event to create another event. In our example, it takes the button clicking event to trigger the next event that we'll define next.

> **what is t?** t represents the timeline, it comes from math theory.

---

**Working with API Requests**

Now we want to construct a request with the NASA api. Ali reminds us again:

* **events** tell you when value changes
* **behaviors** are lines that always have a value

We want to draw a line between dots, something will change value when the button is clicked.  We're going to use `holdDyn` in this example. It takes an initial value (a) and takes an event.

{% highlight haskell %}
holdDyn :: a -> Event a -> m (Dynamic a)
{% endhighlight %}

{% highlight haskell %}

-- using forall tells compiler that all the t and m are the same
workshop' :: forall t m. MonadWidget t m => m ()
workshop' = do
  text "hello world"
  t <- textInput def
  let apiKey = _textInput_value t

  b :: Event t () <- button "Send Request" 

  let apiKeyEvent :: Event t String = tagDyn apiKey b

  submittedApiKey :: Dynamic t String <- holdDyn "EMPTY" apiKeyEvent 

  dynText submittedApiKey
  
  return ()
{% endhighlight %}


> **When do you use let vs <- binding when defining variables?** 
> You use let when defining values where as you bind monads. ConstDyn for example doesn't have m so it's not monadic. 

When looking at quick ref, `m` means you need an arrow thing. Another one is tagDyn doesn't have monads (it's a pure function) so you can use let. Results of a let doesn't impact the reflex-dom program at all.

{% highlight haskell %}
constDyn :: a -> Dynamic a
{% endhighlight %}

> Tips:
> 
> * A monad is good for when you need a behavior.  
> * A pure function doesn't have a side effect so it's like text.
> * At type level - bind arrow strips off monad. m() type: the bind gets rid of the m and gives you something.

<!-- To answer the question you want to ask yourself, do I care when this thing runs. For layout it depends on where in your do block because that affects where it ends up on the page. wholeDyn is something where you care where it starts so it's about is this something that happens at a particular moment (that's when you need a bind) -->

---

**Enter key pressed**

Reacting to user interactions require dynamic events. Let's look at reflex dom quick ref again to see the types.

{% highlight haskell %}
textInputGetEnter :: TextInput -> Event ()
{% endhighlight %}

{% highlight haskell %}
let apiKeyEnterEvent :: Event t String = 
                        tagDyn apiKey (textInputGetEnter t)
{% endhighlight %}

We have two events that trigger something, but we only need one of them so we use a function that picks a default option from the pair. We use `leftmost` to combine two events and returns one.

{% highlight haskell %}
leftmost :: [Event a] -> Event a
{% endhighlight %}


> Tip: monoid basically lets you combine instances. unit `()` is also a monoid
> 
> `<>`` says if you have 2 events and the thing inside is monoidal then you can treat inside as monoidal. If they fire simultaneously and will concatenate everything side.

**Constructing API requests**

xhr lets you construct a request. `performRequestAsync` makes the xhr request, transforming the event to another event using a function with the xhr request inside. `fmap` is the gold old fashioned fmap from where ever you might have learned it... so combining the two looks like the following: 

{% highlight haskell %}
  let req :: Event t XhrRequest = fmap apiKeyToXhrRequest apiKeyEvent
{% endhighlight %}

We want _xhrResponse_responseText from XhrResponse so let's convert the type. We'll have to import data.text since it doesn't come with prelude. 

{% highlight haskell %}
import data.text 
--(you can import qualified to avoid conflicts from prelude)
{% endhighlight %}

> Tip: You can use ++ or <> to concatenate strings

We're only interested in value if a response is returned, so we can make use of the Maybe type.  fmap lets us apply functor to inside the event, event also has a maybe functor instance.


{% highlight haskell %}
-- two events
workshop :: forall t m. MonadWidget t m => m ()
workshop = do
  text "hello world"
  t <- textInput def
  let apiKey = _textInput_value t

  b :: Event t () <- button "Send Request" 

  let apiKeyButtonEvent :: Event t String = tagDyn apiKey b
      apiKeyEnterEvent :: Event t String = tagDyn apiKey (textInputGetEnter t)

      -- if both events fire simultaneously, the 1st listed is picked
      apiKeyEvent :: Event t String = leftmost [ apiKeyButtonEvent
                                               , apiKeyEnterEvent 
                                               ]
  
  submittedApiKey :: Dynamic t String <- holdDyn "NO STRING SUBMITTED" apiKeyEvent
  dynText submittedApiKey

  --take string and turn it into an XhrRequest (metho, url, config)
  let req :: Event t XhrRequest = fmap apiKeyToXhrRequest apiKeyEvent

  -- bind response
  rsp :: Event t XhrResponse <- performRequestAsync req

  -- this is created after response, so it's an event type. 
  let rspText :: Event t (Maybe T.Text) = fmap _xhrResponse_responseText rsp
      rspString :: Event t String = fmap (\rt -> T.unpack $ fromMaybe T.empty rt) rspText

  -- expects a dynamic string but we have Event t (Maybe T.Text)
  holdDyn "" rspString

  return () 

-- function builds the request
apiKeyToXhrRequest :: String -> XhrRequest
apiKeyToXhrRequest k = XhrRequest { _xhrRequest_method = "GET"
                          , _xhrRequest_url = "https://api.nasa.gov/planetary/apod?api_key=" ++ k
                          , _xhrRequest_config = def
                          }


{% endhighlight %}
---

**Parsing JSON**

Finally, let's use Aeson to parse json data. We only need `FromJSON` type to decode. We can use `decodeXhrResponse` from Reflex to decode the response. 

`show` will take value from something and turn it into some sort of string output

> Tip If the aeson parser returns `Nothing`, it probably indicates a typo in the Nasapicture data type.

{% highlight haskell %}
import Reflex.Dom
import qualified Data.Text as T
import Data.Maybe
import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map

data NasaPicture = NasaPicture { copyright :: String
                               , date :: String
                               , explanation :: String
                               , hdurl :: String
                               , media_type :: String
                               , service_version :: String
                               , title :: String
                               , url :: String
                               }
                               deriving (Show, Generic)

instance FromJSON NasaPicture

workshop :: forall t m. MonadWidget t m => m ()
workshop = do
  text "hello world"
  t <- textInput def --def provides default input
  let apiKey = _textInput_value t

  b :: Event t () <- button "Send Request" 

  let apiKeyButtonEvent :: Event t String = tagDyn apiKey b
      apiKeyEnterEvent :: Event t String = tagDyn apiKey (textInputGetEnter t)

      apiKeyEvent :: Event t String = leftmost [ apiKeyButtonEvent
                                               , apiKeyEnterEvent 
                                               ]

  submittedApiKey :: Dynamic t String <- holdDyn "NO STRING SUBMITTED" apiKeyEvent
  dynText submittedApiKey

  let req :: Event t XhrRequest = fmap apiKeyToXhrRequest apiKeyEvent
  rsp :: Event t XhrResponse <- performRequestAsync req
  let rspText :: Event t (Maybe T.Text) = fmap _xhrResponse_responseText rsp
      rspString :: Event t String = fmapMaybe (\mt -> fmap T.unpack mt) rspText

  respDyn <- holdDyn "No Response" rspString

  -- have to tell it what the blog of text should represent
  let decoded :: Event t (Maybe NasaPicture) = fmap decodeXhrResponse rsp

  dynPic :: Dynamic t (Maybe NasaPicture) <- holdDyn Nothing decoded
  dynPicString <- mapDyn show dynPic

  -- if event you could fmap
  -- but it's a dynamic so mapDyn takes function and map to an event

  imgAttrs :: Dynamic t (Map.Map String String) <- forDyn dynPic $ \np -> 
    case np of
        Nothing -> Map.empty
        Just pic -> Map.singleton "src" (url pic) 
        -- create a map, takes a key & value with just that pair
        -- reflex gives you syntactic sugar which can be 
        -- Just pic -> "src" =: url pic

  elDynAttr "img" imgAttrs $ return ()

  return ()

apiKeyToXhrRequest :: String -> XhrRequest
apiKeyToXhrRequest k = XhrRequest { _xhrRequest_method = "GET"
                          , _xhrRequest_url = "https://api.nasa.gov/planetary/apod?api_key=" ++ k
                          , _xhrRequest_config = def
                          }

{% endhighlight %}
---

> Tips: map, forMap
> map and formap are similar but different patterns, different order
>  
> `import Data.Map` .. and
> sometimes you have to import both `Data.Map (Map)` or `Data.Map as Map`

**Closing advice:**

* read quick refs for [Reflex][2] and [Reflex-Dom][3]
* look at subreddit [reflexfrp](http://reddit.com/r/reflexfrp)
* irc channel #reflex-frp
* [ny-haskell slack](http://ny-haskell.org/slack)

[2]: https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
[3]: https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md
