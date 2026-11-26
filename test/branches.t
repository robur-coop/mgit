  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export GIT_AUTHOR_DATE="1792589085 +0200"
  $ export GIT_COMMITTER_DATE="1792589085 +0200"
  $ export MGIT_DATE=1792589085
  $ export MGIT_DEPTH=10
  $ export GREP=$(pwd)/grep.ml

  $ git init -q --bare -b main repo.git 2> /dev/null
  $ git init -q work 2> /dev/null
  $ cd work
  $ git checkout -q -b main
  $ echo "main" > foo
  $ git add foo
  $ git commit -q -m "first"
  $ git checkout -q -b dev
  $ echo "dev" > bar
  $ git add bar
  $ git commit -q -m "second"
  $ git remote add up ../repo.git
  $ git push -q up main dev
  $ cd ..
  $ GIT_TRACE_PACKET=$PWD/trace git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --pid-file=pid --detach
  $ export MGIT_REMOTE=git://localhost/repo.git

  $ mgit create disk.img --size 4MiB
  $ mgit pull disk.img -r $MGIT_REMOTE
  refs/heads/main: + /foo
  $ mgit checkout disk.img dev
  dev 6faa16ba5becfba7b2c104c97a454448562cc964
  $ mgit branches disk.img
  main 8c0066202ed5c8ee2b24fadfaa8dbfe95a895b97
  dev 6faa16ba5becfba7b2c104c97a454448562cc964
  $ MGIT_BRANCH=dev mgit head disk.img > expected
  $ git -C repo.git rev-parse dev > got
  $ diff expected got
  $ MGIT_BRANCH=dev mgit get disk.img /bar
  dev
  $ mgit get disk.img /bar
  mgit: /bar not found
  [124]
  $ MGIT_BRANCH=foo mgit get disk.img /foo
  mgit: refs/heads/foo is not followed
  [124]

  $ cd work
  $ git checkout -q main
  $ echo "main v2" > foo
  $ git commit -q -a -m "main v2"
  $ git checkout -q dev
  $ echo "dev v2" > bar
  $ git commit -q -a --m "dev v2"
  $ git push -q up main dev
  $ cd ..
  $ : > trace
  $ mgit pull disk.img -r $MGIT_REMOTE
  refs/heads/main: ~ /foo
  refs/heads/dev: ~ /bar
  $ ocaml $GREP -c 'upload-pack< want ' < trace
  2
  $ ocaml $GREP -c 'upload-pack< command=fetch' < trace
  1
  $ MGIT_BRANCH=dev mgit head disk.img > expected
  $ git -C repo.git rev-parse dev > got
  $ diff expected got

  $ mgit pull disk.img -r $MGIT_REMOTE

  $ git -C repo.git rev-parse main > main.before
  $ MGIT_BRANCH=dev mgit set disk.img /baz - > head <<EOF
  > on dev
  > EOF
  $ git -C repo.git rev-parse dev > head.expected
  $ diff head head.expected
  $ git -C repo.git rev-parse main > main.expected
  $ diff main.before main.expected
  $ git -C repo.git show dev:baz
  on dev
  $ git -C repo.git fsck --strict

  $ kill $(cat pid)
