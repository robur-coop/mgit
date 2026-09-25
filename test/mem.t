  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export MGIT_DATE=1790353452
  $ export ADD=$(pwd)/add.ml
  $ git init -q --bare repo.git 2> /dev/null
  $ git clone -q repo.git work 2> /dev/null
  $ cd work
  $ git checkout -q -b main

  $ printf "foo %d\n" 1 > foo
  $ mkdir -p dir
  $ printf "bar %d\n" 1 > dir/bar
  $ git add -A
  $ GIT_AUTHOR_DATE="$(ocaml $ADD $MGIT_DATE 1) +0000" \
  > GIT_COMMITTER_DATE="$(ocaml $ADD $MGIT_DATE 1) +0000" \
  > git commit -q -m "Commit 1"

  $ printf "foo %d\n" 2 > foo
  $ mkdir -p dir
  $ printf "bar %d\n" 2 > dir/bar
  $ git add -A
  $ GIT_AUTHOR_DATE="$(ocaml $ADD $MGIT_DATE 2) +0000" \
  > GIT_COMMITTER_DATE="$(ocaml $ADD $MGIT_DATE 2) +0000" \
  > git commit -q -m "Commit 2"

  $ printf "foo %d\n" 3 > foo
  $ mkdir -p dir
  $ printf "bar %d\n" 3 > dir/bar
  $ git add -A
  $ GIT_AUTHOR_DATE="$(ocaml $ADD $MGIT_DATE 3) +0000" \
  > GIT_COMMITTER_DATE="$(ocaml $ADD $MGIT_DATE 3) +0000" \
  > git commit -q -m "Commit 3"

  $ printf "foo %d\n" 4 > foo
  $ mkdir -p dir
  $ printf "bar %d\n" 4 > dir/bar
  $ git add -A
  $ GIT_AUTHOR_DATE="$(ocaml $ADD $MGIT_DATE 4) +0000" \
  > GIT_COMMITTER_DATE="$(ocaml $ADD $MGIT_DATE 4) +0000" \
  > git commit -q -m "Commit 4"

  $ git push -q origin main 2> /dev/null
  $ cd ..
  $ git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --pid-file=pid --detach

  $ export MGIT_DEPTH=2
  $ mgit mem --remote git://localhost/repo.git#main <<EOF
  > pull
  > head
  > list
  > get /dir/bar
  > set /new hello from memory
  > head
  > bundle two.bundle
  > gc 1
  > bundle one.bundle
  > EOF
  main: + /dir/bar
  main: + /foo
  7f347d634a993b1e1414a08b097a4a13e9e05bde
  dir/
  foo
  bar 4
  
  ec85e1ca5c2b84b9e9f934d6ba1f0bbb599821eb

  $ git -C repo.git log --pretty=oneline main
  ec85e1ca5c2b84b9e9f934d6ba1f0bbb599821eb Committed by mgit
  7f347d634a993b1e1414a08b097a4a13e9e05bde Commit 4
  6e7363c4ab13edc5550a39567be9de7ab42bf539 Commit 3
  8c5f848f0ab0f7bdbc8df1276b7d71a9d29b4f39 Commit 2
  95f32f75e266073d0751ad18adcbd542327b6f41 Commit 1
  $ git -C repo.git fsck --strict
  $ git -C repo.git show main:new
  hello from memory
  $ cd work
  $ git fetch -q origin
  $ git bundle verify ../two.bundle
  ../two.bundle is okay
  The bundle contains this ref:
  ec85e1ca5c2b84b9e9f934d6ba1f0bbb599821eb refs/heads/main
  The bundle requires this ref:
  6e7363c4ab13edc5550a39567be9de7ab42bf539 
  The bundle uses this hash algorithm: sha1
  $ git bundle verify ../one.bundle
  ../one.bundle is okay
  The bundle contains this ref:
  ec85e1ca5c2b84b9e9f934d6ba1f0bbb599821eb refs/heads/main
  The bundle requires this ref:
  7f347d634a993b1e1414a08b097a4a13e9e05bde 
  The bundle uses this hash algorithm: sha1

  $ cd ..

  $ kill $(cat pid)
