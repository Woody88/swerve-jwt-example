{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.14/src/packages.dhall sha256:462b0c3503e621142518935bf779555eef593317785f74bd5607bd7ebddf6036

let overrides =   
    { metadata = upstream.metadata // { version = "v0.14.0-rc2" }
    , prelude =
        upstream.prelude // { version = "cfd87116c9dd1eeeb75cf83ffaeac0224f159ed9" }
    , record =
        upstream.record // { version = "ps-0.14" }
    , simple-json =
        upstream.simple-json // { version = "ps-0.14" }
    , typelevel-prelude =
        upstream.typelevel-prelude // { version = "ps-0.14" }
    , variant = 
        upstream.variant // { repo = "https://github.com/JordanMartinez/purescript-variant.git", version = "polykindsUpdate" }
    , heterogeneous = 
        upstream.heterogeneous // { repo = "https://github.com/JordanMartinez/purescript-heterogeneous.git", version = "polykindsUpdate" }
    , typedenv = 
        upstream.typedenv // { version = "ps-0.14" }
    }

let additions =
  { debugged =
        { dependencies =
            [ "console"
            , "effect"
            , "prelude"
            , "strings"
            , "record"
            , "ordered-collections"
            , "either"
            , "tuples"
            , "lists"
            , "arrays"
            , "bifunctors"
            , "generics-rep"
            , "datetime"
            , "enums"
            ]
        , repo =
            "https://github.com/hdgarrood/purescript-debugged.git"
        , version =
            "master"
        } 
    , wai-logger =
        { dependencies =
            [ "console"
            , "effect"
            , "node-process"
            , "numbers"
            , "record-format"
            , "wai"
            ]
        , repo =
            "https://github.com/Woody88/purescript-wai-logger.git"
        , version =
            "master"
        } 
    , node-jwt =
      { dependencies =
            [ "aff"
            , "aff-promise"
            , "console"
            , "effect"
            , "foreign-generic"
            , "generics-rep"
            , "newtype"
            , "psci-support"
            , "options"
            ]
      , repo =
          "https://github.com/gaku-sei/purescript-node-jwt.git"
      , version =
          "master"
      }
    , swerve =
      { dependencies =
          [ "console"
          , "effect"
          , "form-urlencoded"
          , "heterogeneous"
          , "http-media"
          , "http-types"
          , "media-types"
          , "psci-support"
          , "record-format"
          , "simple-json"
          , "typelevel-prelude"
          , "wai"
          , "warp"
          ]
      , repo =
          "https://github.com/Woody88/purescript-swerve.git"
      , version =
          "dsl-ps-0.14-fix"
      }
    , warp =
      { dependencies =
        [ "node-fs-aff"
        , "node-net"
        , "node-url"
        , "wai"
        ]
      , repo =
          "https://github.com/Woody88/purescript-warp.git"
      , version =
          "master"
      }
    , wai =
        { dependencies =
            [ "http-types"
            , "node-buffer"
            , "node-http"
            , "node-net"
            , "node-streams"
            , "node-url"
            ]
        , repo =
            "https://github.com/Woody88/purescript-wai.git"
        , version =
            "master"
        }
    , http-types =
        { dependencies =
            [ "console"
            , "effect"
            , "psci-support"
            , "tuples"
            , "unicode"
            , "uri"
            ]
        , repo =
            "https://github.com/Woody88/purescript-http-types.git"
        , version =
            "master"
        }
    , http-media =
        { dependencies =
            [ "console"
            , "effect"
            , "exceptions"
            , "foldable-traversable"
            , "maybe"
            , "newtype"
            , "numbers"
            , "ordered-collections"
            , "proxy"
            , "strings"
            , "stringutils"
            , "unicode"
            ]
        , repo =
            "https://github.com/Woody88/purescript-http-media.git"
        , version =
            "master"
        }
  }

in  upstream // overrides // additions
