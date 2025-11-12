# os-string-compat

[![GitHub Actions status](https://github.com/Anteproperispomenon/os-string-compat/actions/workflows/haskell.yml/badge.svg)](https://github.com/Anteproperispomenon/os-string-compat/actions/workflows/haskell.yml)
[![GitHub Actions status](https://github.com/Anteproperispomenon/os-string-compat/actions/workflows/haskell_cabal.yml/badge.svg)](https://github.com/Anteproperispomenon/os-string-compat/actions/workflows/haskell_cabal.yml)

## Intro and Purpose

This is a compatibility layer over [os-string](https://hackage.haskell.org/package/os-string)
and (older versions of) [filepath](https://hackage.haskell.org/package/filepath). Notably,
in versions `1.4.100 <= v < 1.5` of  `filepath`, the `newtype` `OsString` was defined in
the `filepath` package itself. However, you *can* import these versions of `filepath`
alongside `os-string`, which *also* defines a `newtype` `OsString`. In fact, in versions
`1.4.200` onwards of `filepath` it *recommends* importing `OsString` via `os-string`
instead of via `filepath`.

The problem is, you can't use functions from `os-string` on `OsString`s from `filepath`.
This may not seem like a problem at first, but `os-string` adds *a lot* of functions for
working directly on `OsString`s.
This isn't a problem if you have a version of `filepath` that's newer than `1.5`, but some
packages *require* that `filepath` be older than `1.5`. If you're writing a package that
depends on such a package, you can't use any of the new functions from `os-string` directly.

Fortunately, in both `os-string` and older versions of `filepath`, `OsString` is ultimately
just a `newtype` over `ShortByteString`[^1], so you can coerce between the two definitions of 
`OsString`[^2]. That's what this package does: it exposes new versions of functions from
`os-string` that are just coerced to work on `OsString`/`OsPath` from `1.4.100+`.

Note that this package works for both pre-1.5 versions of `filepath` *and* post-1.5 versions
of `filepath`. On newer versions, it just re-exports the functions[^3] from `os-string`, since
`filepath` 1.5+ depends on `os-string`. Therefore, you can import `os-string-compat` instead
of `os-string` and you can use the functions regardless of which version of `filepath` you
have. If you're writing a library, you may want to depend on `os-string-compat` instead of
`os-string`, since it'll enable you to put more lenient bounds on `filepath`.

## Usage

### Stack

If this package still isn't available on Hackage (i.e. if the link 
<https://hackage.haskell.org/package/os-string-compat> doesn't lead to
a valid package), you'll have to import this package by adding it to
your `stack.yaml` file. You'll also have to do this if you want to
use an exact *revision* of the package.
e.g.

```yaml
extra-deps:
- git: https://github.com/Anteproperispomenon/os-string-compat
  commit: 3ae529a9c9c0417d6188c2eaaa27693940412c9c # keep this up-to-date
```

If this package **is** available on Hackage, you may still have to add
it to the `extra-deps` field in `stack.yaml`, but you won't have to 
give an exact commit. You can just say e.g.

```yaml
extra-deps:
- os-string-compat-1.0.0
```

### Cabal

If this package has been added to Hackage, you should be able
to run `cabal update` and then add `os-string-compat >= 1.0.0`
to the `build-depends` field in your `<package_name>.cabal` file.

If it still isn't available on Hackage, then you'll probably have
to download, build, and install the package yourself. It's been
a while since I last used Cabal directly, so I'm not sure of the
exact process. Alternatively, you can just use Stack instead of
Cabal and follow the process above. 

### Using the Modules

After that, instead of importing `System.OsString` etc, you import `System.OsString.Compat`
etc... That is, you use the same module name, but add `.Compat` to the end. Note that this
package doesn't include compatibility modules for `System.OsString.Data.ByteString.Short` or
`System.OsString.Data.ByteString.Short.Word16`, since you can just import them directly
without needing to worry about compatibility with `filepath`; when they were included
in `filepath`, they were known as "System.Os*Path*.Data.ByteString.Short" etc..., so their
module names won't clash.

## To-Do

Possibly add more versions of GHC to GitHub Actions workflows. Also look into how to
use GitHub Actions to automate building documentation/packages for Hackage. Also,
figure out how to cache dependencies for the Stack workflow.

## Footnotes

[^1]: Which is itself a `newtype` over `ByteArray`.
[^2]: Yes, there is a difference between Windows `OsString` and POSIX `OsString`,
      but so long as you coerce from `WindowsString` to `WindowsString` or 
      `PosixString` to `PosixString`, you should be okay.
[^3]: There are a few functions that have different definitions depending on the
      version of `os-string`, or only exist in newer versions of `os-string`.
      For the former, such as `length`, this package exports its own version
      based on newer versions of `length` from `os-string`. That way, its
      behaviour is not dependent on the version of `os-string` used. For the
      latter, more obscure functions just aren't exported if the version of
      `os-string` isn't new enough, while others (especially functions that
      the test suites rely on) are defined here, with their definitions just
      copied from newer versions of `os-string`.
