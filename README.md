# life-sync

[![Hackage](https://img.shields.io/hackage/v/life-sync.svg)](https://hackage.haskell.org/package/life-sync)
[![Build status](https://secure.travis-ci.org/kowainik/life-sync.svg)](https://travis-ci.org/kowainik/life-sync)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kowainik/life-sync/blob/master/LICENSE)
[![Stackage LTS](http://stackage.org/package/life-sync/badge/lts)](http://stackage.org/lts/package/life-sync)
[![Stackage Nightly](http://stackage.org/package/life-sync/badge/nightly)](http://stackage.org/nightly/package/life-sync)

`life-sync` is CLI tool that makes it easier to synchronize `dotfiles`
repository with personal configs across multiple machines.

## Motivation

You might have some configuration files with different settings for your system.
For example:

1. Preferred settings for your editors (Spacemacs, Vim, etc.).
2. Useful bash aliases and other miscellaneous shell settings.
3. Git configuration.

And much more! But sometimes you start working from a new fresh machine without
your settings, like in these situations:

1. You bought a new PC or laptop.
2. You might reinstall your system on your working machine.
3. You were given a new laptop at work.

Every time this happens, you need to walk through the tedious process of copying
your data again. It's a well-known practice to
[store configs in `dotfiles` GitHub repository](https://dotfiles.github.io/).
And `life-sync` makes it much easier to maintain this repository! With a single
command, your can copy every file and directory from `dotfiles` repository to
your machine. Or update your remote `dotfiles` repository after multiple local
changes to different files.

## Installation

Installation process can be done with one simple command:

```shell
$ cabal install life-sync
```

or

```shell
$ stack install life-sync-1.0
```

You can turn on the bash auto-completion by running the following command:

```
$ source <(life --bash-completion-script `which life`)
```

## Usage

After installing `life-sync` you need to call command `life` with specified options:

```
$ life --help
Usage: life COMMAND
  life-sync synchronize your personal configs

Available options:
  -h,--help                Show this help text

Available commands:
  init                     Initialize GitHub repository named 'dotfiles' if you
                           don't have one.
  add                      Add file or directory to the life configuration.
  remove                   Remove file or directory from the life configuration.
  push                     Updates GitHub repository from local state and push
                           the latest version.
  pull                     Updates local state of '.life' and 'dotfiles' from
                           GitHub repository.
```

> **NOTE:** make sure you configured SSH for your Github.

If some command takes a path to a file or a directory as an argument, the path
should be specifed relative to the home directory.

`life-sync` keeps the structure of your `dotfiles` repository in its own file
called `.life` which is stored in `dotfiles` repository as well.

You can see an example of `dotfiles` repository maintained by `life-sync` here:

* [ChShersh/dotfiles](https://github.com/ChShersh/dotfiles)

## Examples

### Create `dotfiles` repository for the first time

```
$ life init MyGithubName
```

### Track new file or directory

To track a file:

```
$ life add -f path/to/file/relative/from/home
```

To track a directory:

```
$ life add -d path/to/dir/relative/from/home
```

To stop tracking some file, use `life remove` command instead.

### Push all changes to remote repository

```
$ life push
```

### Pull all changes from remote repository

To pull every file and directory:

```
$ life pull ChShersh
```

To pull everything except some files or some directories:

```
$ life pull ChShersh --no-file some/file --no-dir some/dir
```
