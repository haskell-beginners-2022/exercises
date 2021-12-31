# exercises

[![GitHub CI](https://github.com/haskell-beginners-2022/exercises/workflows/CI/badge.svg)](https://github.com/haskell-beginners-2022/exercises/actions)
[![Hackage](https://img.shields.io/hackage/v/exercises.svg?logo=haskell)](https://hackage.haskell.org/package/exercises)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Exercises for the Haskell Beginners 2022 course. The course itself can be found here:

* [Haskell Beginners 2022 Course](https://github.com/haskell-beginners-2022/course-plan)

This repository contains a complete Haskell project. The project
comprises four Haskell files (modules) in the `src/` directory. Each
module provides exercises for an individual lecture and has the
corresponding name (e.g. `Lecture1.hs`).

## Deadlines

These exercises were created specifically for the Haskell Beginners
2022 course. You can expect to get feedback on your solutions in you
meet the following deadlines:

* `Lecture1.hs`: Jan 17, 2022, 23:59:59 GMT
* `Lecture2.hs`: Jan 24, 2022, 23:59:59 GMT
* `Lecture3.hs`: Jan 31, 2022, 23:59:59 GMT
* `Lecture4.hs`: Feb  7, 2022, 23:59:59 GMT

## Working with the course

This section contains instructions about setting up the development
environment and preparing the exercises repository.

### First time

1. [Fork the `exercises` repository](https://docs.github.com/en/free-pro-team@latest/github/getting-started-with-github/fork-a-repo).
2. Enable GitHub Actions for your forked repository.
    * Visit: `https://github.com/<YOUR_GITHUB_USERNAME>/exercises/actions`
3. Clone your forked repository.
4. Enter the `exercises` directory and add the original repository as a `course` remote.

    ```shell
    git remote add course https://github.com/haskell-beginners-2022/exercises
    ```

    You can verify that everything is done correctly by running the
    `git remote -v` command. The output of this command will looks
    similar to the below:

    ```shell
    course https://github.com/haskell-beginners-2022/exercises (fetch)
    course https://github.com/haskell-beginners-2022/exercises (push)
    origin git@github.com:chshersh/exercises.git (fetch)
    origin git@github.com:chshersh/exercises.git (push)
    ```

### Asking for feedback

When you finished implementing exercises for a particular lecture,
create a Pull Request to **your fork**. The repository already
contains PR template with the prefilled text and mentions all current
mentors of the course.

> ℹ️**NOTE:** Open Pull Request to **your fork** and not this
> repository. We can't merge solutions to this repo. But if you open
> PRs to your repository, you can eventually merge all the solutions
> and enjoy gree all-passing CI 🍏

### Updating your fork

The course content (exercises, tests, configuration, etc.) might
change after you forked the course. To get the latest updates, follow
the below instructions:

1. Switch to your `main` branch locally and make sure it's in sync
   with the latest version of your fork on GitHub.

    ```shell
    git checkout main
    git pull --rebase --prune
    ```

2. Fetch all the course changes and save them locally.

    ```shell
    git fetch course main
    git rebase course/main
    ```

    > NOTE: This stage may require you to resolve conflicts.

3. Push local changes to your own fork.

    ```shell
    git push origin main --force
    ```

## Installing Haskell

Follow the below instructions to configure the Haskell development
environment.

### Haskell Toolchain

To develop in Haskell, you need to install `ghcup`, `ghc` and `cabal`.

1. Install [ghcup](https://www.haskell.org/ghcup/) and follow `ghcup`
   instructions for successful installation (remember to restart your
   terminal afterwards to avoid an `unknown ghcup command` error on
   the next step).
2. Install the recommended version of the Haskell compiler — GHC — and the
   [Cabal](https://www.haskell.org/cabal/) build tool. After you install
   `ghcup`, it is easy to install the rest with a few commands from your
   terminal, if these tools are not yet installed.

    ```shell
    ghcup install ghc 8.10.7
    ghcup set ghc 8.10.7
    ghcup install cabal 3.6.2.0
    ```

	You can verify that everything is installed correctly by running
    the following commands:

    ```shell
    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 8.10.7
    $ cabal --version
    cabal-install version 3.6.2.0
    compiled using version 3.6.2.0 of the Cabal library
    ```

4. Run `cabal update` to fetch the latest info about Haskell packages.

### Haskell IDE

If you don't have any IDE preferences, we recommend installing
[Visual Studio Code](https://code.visualstudio.com/download) with the
[Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
The mentioned plugin would give you everything required to immediately start coding with Haskell.

## How to build and test?

To compile the entire project, run the following command from your terminal:

```shell
make build
```

To run tests for a specific lecture only (e.g. the first one), use the
following command:

```shell
make test-lecture1
```

## Acknowledgement

This course is inspired by
[Learn4Haskell](https://github.com/kowainik/learn4haskell) authored by
[@vrom911](https://github.com/vrom911) and
[@chshersh](https://github.com/chshersh).
