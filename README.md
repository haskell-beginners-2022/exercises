# exercises

[![GitHub CI](https://github.com/haskell-beginners-2022/exercises/workflows/CI/badge.svg)](https://github.com/haskell-beginners-2022/exercises/actions)
[![Hackage](https://img.shields.io/hackage/v/exercises.svg?logo=haskell)](https://hackage.haskell.org/package/exercises)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

> :warning: The course authors no longer provide reviews of the solutions.
> However, all the learning materials are free and publicly available.
> You can learn Haskell on your own or with the help of others.

Exercises for the Haskell Beginners 2022 course. The course itself can be found here:

* [Haskell Beginners 2022 Course](https://github.com/haskell-beginners-2022/course-plan)

This repository contains a complete Haskell project. The project
comprises four Haskell files (modules) in the `src/` directory. Each
module provides exercises for an individual lecture and has the
corresponding name (e.g. `Lecture1.hs`).

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
    `git remote -v` command. The output of this command will look
    similar to the below:

    ```shell
    course https://github.com/haskell-beginners-2022/exercises (fetch)
    course https://github.com/haskell-beginners-2022/exercises (push)
    origin git@github.com:chshersh/exercises.git (fetch)
    origin git@github.com:chshersh/exercises.git (push)
    ```

### Asking for feedback

Implement your solutions in a separate branch (not `main`). You can
run the following command to create a new branch and switch to it at
the same time:

```shell
git checkout -b lecture-1-solutions
```

When you have finished implementing exercises for a particular lecture,
create a Pull Request to **your fork**. The repository already
contains PR template with the prefilled text and mentions all current
mentors of the course.

> ℹ️**NOTE:** Open Pull Request to **your fork** and not this
> repository. We can't merge solutions to this repo. But if you open
> PRs to your repository, you can eventually merge all the solutions
> and enjoy green all-passing CI 🍏

To open a PR to your fork, you need to change _base repository_ to
your own repository, as shown on the screenshot below:

![PR to fork example](https://user-images.githubusercontent.com/4276606/147921946-e9b84424-e76f-4f7a-8976-e33564ae1532.png)

After you change, the PR view will change accordingly:

![Final PR to fork](https://user-images.githubusercontent.com/4276606/147922107-78f80f23-e98c-47f8-8cb3-d20a8b2f771d.png)

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
    ghcup install ghc 9.2.5
    ghcup set ghc 9.2.5
    ghcup install cabal 3.8.1.0
    ```

	You can verify that everything is installed correctly by running
    the following commands:

    ```shell
    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 9.2.5
    $ cabal --version
    cabal-install version 3.8.1.0
    compiled using version 3.8.1.0 of the Cabal library
    ```

4. Run `cabal update` to fetch the latest info about Haskell packages.

### Haskell IDE

If you don't have any IDE preferences, we recommend installing
[Visual Studio Code](https://code.visualstudio.com/download) with the
[Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
The mentioned plugin would give you everything required to immediately start coding with Haskell.

## How to build and test?

There're two ways to build this project: using either `cabal` or
`stack` build tools. Using `cabal` is the recommended way. However, if
it doesn't work, you may want to use `stack`.

### Cabal

To compile the entire project, run the following command from your terminal:

```shell
make build
```

To run tests for a specific lecture only (e.g. the first one), use the
following command:

```shell
make test-lecture1
```

You can also run tests only for a single function. For example, to run
tests for the `strSum` function, execute the following command:

```shell
cabal run exercises-test --enable-tests -- -m "strSum"
```

### Stack

Use the [official `stack` installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to install `stack`.

To build the project with `stack`, run the following command:

```shell
stack build --test --no-run-tests
```

To run tests for the first lecture, run the following commands:

```shell
stack test :doctest-lecture1
stack test :exercises-test --test-arguments='-m "Lecture 1"'
```

And to tests a specific function, use:

```shell
stack test :exercises-test --test-arguments='-m "strSum"'
```
