---
title: Hosting Static Web Projects With Hakyll And Nix
date: 2022-08-31
---

I got this blog setup a while ago so I figure I should finally get around to
writing a post. And what better topic than the blog setup itself!

First, a bit of background. I setup the Nix part of the site based mostly on
Rebecca Skinner's [excellent blog
post](https://rebeccaskinner.net/posts/2021-06-06-nixifying-a-hakyll-blog.html)
and the hakyll part based mostly on the default template and some of the
official tutorials. However, a few contributions are my own. The largest of
which is the projects setup. Currently I only have two projects hosted here, but
the way I've set it up it will be dead simple to add more.

For the complete code discussed here, see the [GitHub
repository](https://github.com/ElderEphemera/elderephemera.github.io/tree/57b6ccb0bcc51cab90dcdf82f865d1d2b19b2acd).

## The Plan

I had some static web projects that I wanted to host on the same site as my
blog. Originally I was lazy and just copied the build result into the site
repository. Obviously this was a huge pain though, so I was looking for a better
solution. I wanted something that would take my projects from other repositories
and automatically build them when the site is built, including the output in the
site. Ideally there would also be a good way to access the projects for the main
site too.

## The Nix Side

Once I was using Nix, the first step of this was obvious. As long as the
projects were buildable with Nix, all that is needed to automatically fetch and
build them is one of the fetchers like `fetchFromGitHub`. For each project,
we'll create a Nix file in "projects/" containing something like the following:

```nix
let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "four";
    rev = "e11c35df3a84c0c7fee288e7ef6ca174f4a84dbb";
    sha256 = "0did54cf6skvwspkcpisah6lh44yvywhccf6146kdm9q397pnz3h";
  };
in
  (import repo).build.web
```

The next step then, is to put all of the projects together. Luckily Nixpkgs has
a super handy builder called `linkFarm` that just combines several derivations
into one. We'll put this in "projects/default.nix".

```nix
let
  projectNames = [ "recipe-bookmarks" "four" ];
  importProject = name: import (./. + "/${name}.nix");

  projects = map (name: {
    inherit name;
    path = importProject name;
  }) projectNames;

in pkgs.linkFarm "elderephemera.github.io/projects" projects
```

All that's left for the Nix part is excluding the actual "projects/" folder from
the source of the "nix/" folder and copying over the generated files in with the
source. All that's needed for this is adding a single line to the
`gitignoreSourcePure` call and one more line in `buildPhase`.

```nix
let
  ...
  
  projects = import ./projects { inherit pkgs; }; # Import projects/default.nix

  site = pkgs.stdenv.mkDerivation {
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ...
      "projects"
    ] ./.;

    ...

    buildPhase = ''
      cp -r ${projects} projects # Copy projects output into src/projects
      ${builder}/bin/build-site build
    '';
    
    ...
```

## Hakyll Time

With Nix doing all the heavy lifting the Hakyll side is really simple. In fact
from the perspective of the site builder, the project files are just ordinary
files in the source tree so all we need to simply host them is a rule that
copies them over verbatim.

```haskell
main :: IO ()
main = hakyll $ do
  ...
  
  match "projects/**" $ do
    route   idRoute
    compile copyFileCompiler
```

Simple as that!

## The Projects Page

So now the projects are hosted on the site, but there's still a problem. There's
no way to get to them! We need a projects page. While we're at it, let's make it
spiffy with links to GitHub and descriptions of each project. Again, we'll
figure out the Nix step first so that we will know precisely what the Hakyll
step has to do.

I wanted to keep all the configuration for each project in the
"projects/foo.nix" file. Originally, I just had a short description for each
project, but I decided I also wanted proper external links and proper titles
(instead of just re-using the name of the Nix file). Here's what I wound up
with:

```nix
{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "four";
    rev = "e11c35df3a84c0c7fee288e7ef6ca174f4a84dbb";
    sha256 = "0did54cf6skvwspkcpisah6lh44yvywhccf6146kdm9q397pnz3h";
  };
in {
  name = "four";
  path = (import repo).build.web;
  info = {
    title = "Four";
    link = "four/index.html";
    description = ''
      Something approaching a 2048-esque puzzle game. But it doesn't quite
      achieve the "puzzle" or even the "game" part. Originally created for the
      <a href="https://itch.io/jam/not-a-game-jam-game-jam">Not A Game Jam Game
      Jam</a>.
    '';
    badges = [
      {
        name = "GitHub";
        link = "https://github.com/ElderEphemera/four";
        image = "GitHub-Mark-Light-32px.png";
      }
      {
        name = "itch.io";
        link = "https://elderephemera.itch.io/four";
        image = "itchio-textless-white.svg";
      }
    ];
  };
}
```

Now, in the "projects/default.nix" derivations we need to generate some kind of
file for the site builder to read. I chose to use JSON for this just because
it's already supported by Haskell (through aeson) and Nix (with
`builtins.toJSON`). Initially, I had planned to use YAML with Hakyll's built-in
metadata header feature, but it turns out it only supports a subset of YAML and
doesn't work at all for nested data.

The final version of "projects/default.nix" ended up looking like this:

```nix
{ pkgs ? import <nixpkgs> {}
}:

let
  projectNames = [ "recipe-bookmarks" "four" ];
  importProject = name: import (./. + "/${name}.nix") { inherit pkgs; };

  infoFile = info: pkgs.writeText "info.json" (builtins.toJSON info);

  projects = map (name:
    let project = importProject name;
    in {
      inherit (project) name;
      path = pkgs.linkFarm
        "elderephemera.github.io/projects/${project.name}" [
          { name = "info.json"; path = infoFile project.info; }
          { name = "content"; path = project.path; }
        ];
    }) projectNames;

in pkgs.linkFarm "elderephemera.github.io/projects" projects
```

Here we use a second `linkFarm` call to separate out the content of the project
from the info file. We also used `writeText` to generate the JSON file. And
that's all we need to do to get our Nix builder to give the Hakyll builder
everything it needs.

## JSON Context

Now comes the hardest part. In order to get the data from the JSON file into a
template we need to provide Hakyll with a `Context`. Which is defined as:

```haskell
newtype Context a = Context
    { unContext :: String -> [String] -> Item a -> Compiler ContextField
    }
```

While Hakyll has some functions for creating `Context`s, none of them really fit
our purposes. So we'll have to dig in and deal directly with this datatype. The
`unContext` field takes three arguments, the requested field name, field
arguments, and an `Item` containing the required data, in this case a `Value`
from `aeson`. So we'll start off our context like this.

```haskell
jsonCtx :: Context Value
jsonCtx = Context $ \name _ (Item _ meta) -> ...
```

We aren't going to provide functions so, following the functions in
`Hakyll.Web.Template.Context`, we just ignore the arguments. Likewise for the
`itemIdentifier` part of the `Item`.

Now we've run into a bit of a problem. If we want to provide full support for
JSON contexts, we'll need nested fields such as `"foo.bar.baz"`, but Hakyll
doesn't support anything like this. Luckily, it *is* somewhat liberal with the
names of fields, so `$foo.bar.baz$` will parse just fine and be passed to our
`Context` where we can break it into parts ourselves. A quick and dirty function
to split on `'.'`s will do.

```haskell
splitName ('.':cs) = "" : splitName cs
splitName (c:cs) = let n:ns = splitName cs in (c:n):ns
splitName [] = [""]
```

Now that we've got the path to the value, we can use that to get the value
itself with a function `getField :: Value -> [String] -> Maybe (Compiler
ContextField)`.

```haskell
getField (Object obj) (n:ns)
  | Just val <- obj !? T.pack n = getField val ns
getField (Object obj) [] = Just
  $ ListField jsonCtx <$> traverse (makeItem . uncurry object) (toList obj)
getField (Array arr) [] = Just
  $ ListField jsonCtx <$> traverse makeItem (toList arr)
getField (String txt) [] = Just . pure . StringField $ T.unpack txt
getField (Number num) [] = Just . pure . StringField $ show num
getField (Bool True) [] = Just $ pure EmptyField
getField (Bool False) [] = Just $ noResult "Field is false"
getField Null [] = Just $ noResult "Field is null"
getField _ _ = Nothing

object key value = Object $ "key" .= key <> "value" .= value 
```

There's a lot going on with this function, but the recursive case is fairly
simple, and most of the base cases are just replicating what the builtin
contexts for booleans and lists and such do. One thing to note is that for
object fields we treat them as lists of key-value pairs. This seems like the
most sensible thing to do as it lets us use objects with Hakyll's `$for(list)$`
constructs.

Finally, we have to display an error if the field was not found in the
`Value`. Again mimicking Hakyll's own contexts:

```haskell
failure json = noResult $ "Tried JSON context " ++ BS.unpack (encode json)
```

Putting it all together, we get:

```haskell
jsonCtx :: Context Value
jsonCtx = Context $ \name _ (Item _ meta) ->
  fromMaybe (failure meta) . getField meta $ splitName name
  where
    getField (Object obj) (n:ns)
      | Just val <- obj !? T.pack n = getField val ns
    getField (Object obj) [] = Just
      $ ListField jsonCtx <$> traverse (makeItem . uncurry object) (toList obj)
    getField (Array arr) [] = Just
      $ ListField jsonCtx <$> traverse makeItem (toList arr)
    getField (String txt) [] = Just . pure . StringField $ T.unpack txt
    getField (Number num) [] = Just . pure . StringField $ show num
    getField (Bool True) [] = Just $ pure EmptyField
    getField (Bool False) [] = Just $ noResult "Field is false"
    getField Null [] = Just $ noResult "Field is null"
    getField _ _ = Nothing

    object key value = Object $ "key" .= key <> "value" .= value 

    splitName ('.':cs) = "" : splitName cs
    splitName (c:cs) = let n:ns = splitName cs in (c:n):ns
    splitName [] = [""]

    failure json = noResult $ "Tried JSON context " ++ BS.unpack (encode json)
```

## The Template

Now the project page template. If you're familiar with Hakyll templates there's
not too much going on here. My projects page template looks like this:

```html
---
title: Projects
---

<p>
  Below is a list of my projects that are hosted on this site. For other
  projects, check out <a href="https://github.com/ElderEphemera">my GitHub
  page</a>.
</p>

<ul id="projectlist">
  $for(projects)$
    <li>
      <div class="project-header">
        <h2 class="project-name"><a href="/projects/$link$">$title$</a></h2>
        <div class="project-badges">
          $for(badges)$
            <a href="$link$" title="$name$">
              <img src="/images/$image$" alt="$name$" height="32"/>
            </a>
          $endfor$
        </div>
      </div>
      <p class="project-description">$description$</p>
    </li>
  $endfor$
</ul>
```

The difficult part is getting all the Hakyll `Rules` right. We'll replace our
simple `match "projects/**"` rule from earlier with 3 new rules.

```haskell
main :: IO ()
main = hakyll $ do
  ...

  match "projects.html" $ do
    route   idRoute
    compile $ do
      let projects = traverse (either fail pure . traverse eitherDecode)
            =<< loadAll "projects/*/info.json"
          projectsContext =
               listField "projects" jsonCtx projects
            <> defaultContext
      getResourceBody
        >>= applyAsTemplate projectsContext
        >>= loadAndApplyTemplate "templates/default.html" projectsContext
        >>= relativizeUrls

  match "projects/*/info.json" $ compile getResourceLBS

  match "projects/*/content/**" $ do
    route   $ gsubRoute "content/" mempty
    compile copyFileCompiler
```

Even though the `info.json` files don't directly correspond to a file, we still
need a rule for them so that Hakyll can track the dependencies. It would be
cleaner if the decoding step could be put into that rule, but `Value` does not
have the instances Hakyll needs to cache it and the multitude of orphan
instances just aren't worth it. So instead we shove all the decoding bits
(`traverse (either fail pure . traverse eitherDecode)`) into the `projects.html`
rule.

One last quick thing to notice is that we use `gsubRoute "content/" mempty` to
simplify the paths of the actual project files.

## Wrap up

All in all, I'm very happy with the way things turned out. The projects page is
clean and updating or adding projects is a breeze. Using Nix also made building
and deploying the site with GitHub actions much easier. One downside is that, in
using Nix, we end up throwing away Hakyll's caching and making some of it's
other features harder to use. I think this is a fair trade off though, for
everything Nix brings to the table. You can check out the finished product
[here](/projects).
