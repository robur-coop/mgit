  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="romain@robur.coop"
  $ export GIT_AUTHOR_DATE="1790005365 +0200"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="romain@robur.coop"
  $ export GIT_COMMITTER_DATE="1790005365 +0200"

  $ mkdir repo
  $ cd repo
  $ git init -q 2> /dev/null
  $ git config init.defaultBranch main
  $ git checkout -b main -q
  $ echo "Hello World!" > foo
  $ mkdir dir
  $ echo "Git rocks!" > dir/bar
  $ chmod +x dir/bar
  $ ln -s foo link
  $ git add foo dir/bar link
  $ git commit -q -m "first"

  $ echo "Hello World!" | git hash-object --stdin
  980a0d5f19a64b4b30a87d4206aade58726b60e3
  $ echo "Hello World!" | mgit.obj hash-object blob
  980a0d5f19a64b4b30a87d4206aade58726b60e3

  $ git ls-tree HEAD
  040000 tree 67268262b1b5eed5a156ef6111f460b4167edf15	dir
  100644 blob 980a0d5f19a64b4b30a87d4206aade58726b60e3	foo
  120000 blob 19102815663d23f8b75a47e7a01965dcdc96468c	link
  $ git ls-tree HEAD | git mktree > expected
  $ git ls-tree HEAD | mgit.obj mktree > got
  $ diff expected got
  $ git rev-parse HEAD^{tree} > head-tree
  $ diff head-tree got

  $ git cat-file tree HEAD^{tree} | mgit.obj cat-tree
  040000 tree 67268262b1b5eed5a156ef6111f460b4167edf15	dir
  100644 blob 980a0d5f19a64b4b30a87d4206aade58726b60e3	foo
  120000 blob 19102815663d23f8b75a47e7a01965dcdc96468c	link

  $ TREE=$(git rev-parse HEAD^{tree})
  $ printf "first\n" | git commit-tree $TREE > expected
We need to include \n...
  $ mgit.obj commit-tree $TREE -m "first
  > " > got
  $ diff expected got
  $ diff expected <(git rev-parse HEAD)
  $ echo "bar" > foo
  $ git add foo
  $ git commit -q -m "second"
  $ TREE=$(git rev-parse HEAD^{tree})
  $ PARENT=$(git rev-parse HEAD^)
  $ printf "second\n" | git commit-tree $TREE -p $PARENT > expected
  $ mgit.obj commit-tree $TREE -p $PARENT -m "second
  > " > got
  $ diff expected got

  $ git cat-file commit HEAD | mgit.obj iso commit
  iso ok
  $ git cat-file tree HEAD^{tree} | mgit.obj iso tree
  iso ok
  $ git cat-file tree HEAD^{tree}:dir | mgit.obj iso tree
  iso ok

  $ git tag -a -m "an annotated tag" v1
  $ git cat-file tag v1 | mgit.obj iso tag
  iso ok
