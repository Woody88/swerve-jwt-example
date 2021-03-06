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
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201223/packages.dhall sha256:a1a8b096175f841c4fef64c9b605fb0d691229241fd2233f6cf46e213de8a185

let overrides =
        { metadata = upstream.metadata // { version = "v0.14.0-rc4" }
        , prelude = upstream.prelude //  { version = "cfd87116c9dd1eeeb75cf83ffaeac0224f159ed9" }
        , record = upstream.record // { version = "ps-0.14" }
        , simple-json = upstream.simple-json // { version = "ps-0.14" }
        , typelevel-prelude = upstream.typelevel-prelude // { version = "master" }
        , type-equality = upstream.type-equality // { version = "master" }
        , variant = 
            upstream.variant // { repo = "https://github.com/JordanMartinez/purescript-variant.git"
                                , version = "polykindsUpdate"
                                }
        , heterogeneous = 
            upstream.heterogeneous // { repo = "https://github.com/Woody88/purescript-heterogeneous.git"
                                , version = "polykindsUpdate"
                                }
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
            "https://github.com/Woody88/purescript-debugged.git"
        , version =
            "ps-0.14"
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
            "ps-0.14"
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
            [ "arrays"
            , "b64"
            , "console"
            , "debugged"
            , "effect"
            , "form-urlencoded"
            , "http-media"
            , "http-types"
            , "psci-support"
            , "simple-json"
            , "node-http"
            , "wai"
            , "warp"
            ]
      , repo =
          "https://github.com/Woody88/purescript-swerve.git"
      , version =
          "swerve-server"
      }
    , warp =
        { dependencies =
          [ "console", "effect", "node-fs-aff", "generics-rep", "wai" ]
        , repo = "https://github.com/Woody88/purescript-warp.git"
        , version = "master"
        }
    , wai =
        { dependencies = [ "effect", "aff", "http-types", "node-net" ]
        , repo = "https://github.com/Woody88/purescript-wai.git"
        , version = "master"
        }
    , http-types =
        { dependencies = [ "tuples", "unicode", "generics-rep" ]
        , repo = "https://github.com/Woody88/purescript-http-types.git"
        , version = "master"
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
            , "strings"
            , "stringutils"
            , "unicode"
            ]
        , repo =
            "https://github.com/Woody88/purescript-http-media.git"
        , version =
            "ps-0.14"
        }
  }

in  upstream // overrides // additions
