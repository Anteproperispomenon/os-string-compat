# os-string-compat

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
of `filepath`. On newer versions, it just re-exports the functions from `os-string`, since
`filepath` 1.5+ depends on `os-string`. Therefore, you can import `os-string-compat` instead
of `os-string` and you can use the functions regardless of which version of `filepath` you
have. If you're writing a library, you may want to depend on `os-string-compat` instead of
`os-string`, since it'll enable you to put more lenient bounds on `filepath`.

## Usage

At the moment, you'll have to import this package by adding it to your `stack.yaml` file.
e.g.

```yaml
extra-deps:
- git: https://github.com/Anteproperispomenon/os-string-compat
  commit: 7c469fedffdd598cbe6f7eee6a6b08d6ae831e7c # keep this up-to-date
```

After that, instead of importing `System.OsString` etc, you import `System.OsString.Compat`
etc... That is, you use the same module name, but add `.Compat` to the end. Note that this
package doesn't include compatibility modules for `System.OsString.Data.ByteString.Short` or
`System.OsString.Data.ByteString.Short.Word16`, since you can just import them directly
without needing to worry about compatibility with `filepath`; when they were included
in `filepath`, they were known as "System.Os*Path*.Data.ByteString.Short" etc..., so their
module names won't clash.

## To-Do

Figure out a way to automatically test this package with multiple different
versions of `filepath` and `os-string`. I've added tests from `os-string`,
and I was able to find an issue that way, but testing it by manually changing
the versions of `filepath` and `os-string` in `stack.yaml` is very long and
tedious.

## Footnotes

[^1]: Which is itself a `newtype` over `ByteArray`.
[^2]: Yes, there is a difference between Windows `OsString` and POSIX `OsString`,
      but so long as you coerce from `WindowsString` to `WindowsString` or 
      `PosixString` to `PosixString`, you should be okay.