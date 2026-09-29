  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export GIT_AUTHOR_DATE="1790673137 +0200"
  $ export GIT_COMMITTER_DATE="1790673137 +0200"
  $ export MGIT_DATE=1790673137
  $ export TRIM=$(pwd)/trim.ml

  $ mkdir simple
  $ cd simple
  $ git init -q 2> /dev/null
  $ git config init.defaultBranch main
  $ git checkout -b main -q
  $ echo "Hello World!" > foo
  $ mkdir dir
  $ echo "Git rocks!" > dir/bar
  $ git add foo dir/bar
  $ git commit -q -m .
  $ git rev-parse HEAD
  0bc3763594fd1ecd60f604f1c3295da42c68fdc1
  $ cd ..
  $ git daemon --base-path=. --export-all --reuseaddr --pid-file=pid --detach
  $ touch git-daemon-export-ok

  $ mgit create --size=4MiB disk.img
  $ mgit pull disk.img --remote git://localhost/simple#main
  refs/heads/main: + /dir/bar
  refs/heads/main: + /foo
  $ mgit head disk.img
  0bc3763594fd1ecd60f604f1c3295da42c68fdc1
  $ mgit get disk.img /dir/bar
  Git rocks!

  $ cd simple
  $ echo "v2" > foo
  $ echo "new file" > bar
  $ git add foo bar
  $ export GIT_AUTHOR_DATE="1790688118 +0200"
  $ export GIT_COMMITTER_DATE="1790688118 +0200"
  $ git commit -q -m .
  $ git rev-parse HEAD
  1d9ab69e10995d5977d9a0147c771b6624011a70
  $ cd ..
  $ mgit pull disk.img -r git://localhost/simple#main
  refs/heads/main: + /bar
  refs/heads/main: ~ /foo
  $ mgit list disk.img /
  bar
  dir/
  foo

  $ mgit pull disk.img -r git://localhost/simple#main
  $ mgit head disk.img
  1d9ab69e10995d5977d9a0147c771b6624011a70

  $ mgit bundle disk.img -o out.bundle
  $ cd simple
  $ git bundle verify ../out.bundle | ocaml $TRIM | head -5
  ../out.bundle is okay
  The bundle contains this ref:
  1d9ab69e10995d5977d9a0147c771b6624011a70 refs/heads/main
  The bundle requires this ref:
  0bc3763594fd1ecd60f604f1c3295da42c68fdc1
  The bundle uses this hash algorithm: sha1
  $ cd ..

  $ git init -q big 2> /dev/null
  $ cd big
  $ git checkout -q -b main
  $ head -c 20000000 /dev/urandom > blob
  $ git add blob
  $ git commit -q -m big
  $ cd ..
  $ mgit create small.img --size 2MiB
  $ timeout 60 mgit pull small.img -r git://localhost/big#main
  mgit: Zone of the block-device full
  [124]
  $ mgit branches small.img

  $ kill $(cat pid)
