# Git Cuk

[![Hackage](https://img.shields.io/hackage/v/git-cuk.svg)](https://hackage.haskell.org/package/git-cuk)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/git-cuk/badge/lts)](http://stackage.org/lts/package/git-cuk)
[![Stackage Nightly](http://stackage.org/package/git-cuk/badge/nightly)](http://stackage.org/nightly/package/git-cuk)
[![Build status](https://secure.travis-ci.org/siapbantu/git-cuk.svg)](https://travis-ci.org/siapbantu/git-cuk)

**Siapbantu Git Workflow Helper Tool.**

You can find the description of the workflow here:

`git-cuk`  provides the `cuk` binary with a convenient command-line interface to improve the interaction with [`git`][git] in a compatible way with the described working methods. It saves time for people who use this workflow on a daily basis, helps beginners expand their insight of the core VCS processes and makes collaboration between team members easier during development.

## Getting started

### Prerequisites

To start using `cuk` make sure that you have the following tools installed on your machine:

* [ `git`][git] — `cuk` is a wrapper around `git`

### Installation

There are several methods to install the `cuk` tool. You can choose the one that you are most comfortable with.

#### Download from releases

You can download the `cuk` binary directly from the GitHub releases:

* [`cuk` releases](https://github.com/siapbantu/git-cuk/releases)

After downloading, make it executable and copy it to a convenient location, for example:

```shell
chmod +x cuk-linux
mv cuk-linux ~/.local/bin/cuk
```

#### Build from source

> **NOTE:** the project is written in Haskell, so you need to have one of the Haskell build tools installed.
You need to follow these steps:

1. Clone the repository from GitHub

    ```shell
    git clone https://github.com/siapbantu/git-cuk.git
    ```
2. Step into the directory

    ```shell
    cd git-cuk
    ```

3. Install the project with one of the build tools
  * [Cabal](https://www.haskell.org/cabal/users-guide/)
     ```shell
        cabal new-install git-cuk
     ```
    **Note:** make sure you have `~/.cabal/bin` in your $PATH
  * [Stack](https://docs.haskellstack.org/en/stable/README/)
     ```shell
        stack install git-cuk
     ```
4. Make sure that `cuk` is installed:

    ```shell
    cuk --version
    ```

#### macOS package manager

Currently, this method of installation is not supported. See [this issue](https://github.com/siapbantu/git-cuk/issues/1) for more details or if you want to help.

#### Ubuntu package manager

Currently, this method of installation is not supported. See [this issue](https://github.com/siapbantu/git-cuk/issues/2) for more details or if you want to help.

### Setting up

Follow the steps below to configure `cuk` :

1. Specify your GitHub login in the global `.gitconfig`
```shell
git config --global user.login <your_login>
```
2. **This step is only required if you want to use `cuk` with private repositories**.
    1. [Create OAuth token on GitHub.](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
    2. Copy the generated token.
    3. Export token as an environment variable
        ```shell
        export GITHUB_TOKEN=<paste_generated_token_here>
        ```

## Usage

The best way to demonstrate the power of the `cuk` tool on a day-to-day basis with our workflow is to go through the entire workflow step by step, solving an ordinary problem of the typical [`git`][git] user.

> Here we assume that you work with `origin` remote with the main branch set to `master`.
### cuk hop

When you want to start working on a new issue, you usually want to make sure you're using the latest version of your project. As a `git` user you may use the following commands:

```shell
git checkout master
git pull --rebase --prune
```

With `cuk` you can just:

```shell
cuk hop
```

### cuk issue

Now you need to decide which issue you want to work on. You can use the `cuk issue` command to see the full list of all open issues. After choosing the number of the issue, let's say 42, call `cuk issue 42` to see the details of that issue.

### cuk new

Start your work in a new branch. According to our workflow, branch names should have the following form:

```
<user_login>/<issue_number>-<short_issue_description>
```

With `git` you can create a branch using the following command:

```shell
git checkout -b my-login/42-short-desc
```

`cuk` allows you to accomplish this task in an easier manner:

```shell
cuk new 42
```

It uses the issue title to generate a short description.

### cuk commit

After finishing your work on that issue, you need to commit your changes. With `git` you would do the following:

```shell
git add .
git commit -m "[#42] Implement my feature
Resolves #42"
```

With `cuk` you need only to specify the text of the commit to get the same result:

```shell
cuk commit "Implement my feature"
```

Note that you don't need to keep in mind the current issue number. However, if you want to refresh the context about the issue, use the `cuk current` command.

### cuk push

After committing your changes locally, you need to push them to the remote repository. It's usually a good practice to push only the current branch.

The `git` command for this is a little bit verbose:

```shell
git push -u origin my-login/42-short-desc
```

`cuk` allows you to save several keystrokes:

```shell
cuk push
```

### cuk sync

After opening the pull request, some of the reviewers suggested changes that you applied as commits to the remote branch via GitHub interface. Now you need to sync your local branch with the remote one.

With `git` you can do the following:

```shell
git pull --rebase origin my-login/42-short-desc
```

However, with `cuk` you can just:

```shell
cuk sync
```

### cuk fresh

While you were waiting for the second round of reviews, another pull request was merged to the `master` branch. Now you need to apply the new `master` changes to your local branch.

With `git` you can do the following:

```shell
git fetch origin master
git rebase origin/master
```

Again, with `cuk` you can do better:

```shell
cuk fresh
```

### cuk fix

Now you need to make changes to your work locally according to the code review and push them to the remote repository.

`git` requires from you to do several steps to accomplish this simple task:

```shell
git add .
git commit -m "Fix after review"
git push origin my-login/42-short-desc
```

`cuk` helps you with this as well:

```shell
cuk fix
```

### cuk amend

Oops, you've just realised that you have made a typo in your work! So you fixed the typo. But now you want to update the remote branch without creating a new unnecessary commit.

With `git` you can do the following:

```shell
git commit -a --amend --no-edit
git push origin my-login/42-short-desc --force
```

With `cuk` you can simply:

```shell
cuk amend
```

### cuk resolve

Hooray, your PR just got merged! It's time to clean your local repository and start working on a new issue!

With `git` you would do the following:

```shell
git checkout master
git pull --rebase --prune
git branch -D my-login/42-short-desc
```

With `cuk` you can finish your work faster:

```shell
cuk resolve
```

[git]: https://git-scm.com/
