  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export MGIT_DATE=1790697745
  $ export TRIM=$(pwd)/trim.ml

  $ mkdir repo
  $ cd repo
  $ git init -q 2> /dev/null
  $ git config init.defaultBranch main
  $ git checkout -b main -q
  $ for i in 1 2 3 4 5 6; do
  >   echo "line $i" >> f
  >   git add f
  >   GIT_AUTHOR_DATE="1790697745 +0200" GIT_COMMITTER_DATE="1790697745 +0200" \
  >     git commit -q -m "c$i"
  > done
  $ git log --pretty=oneline
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36 c6
  bef3acfd844ddf2cb034d2f530235823e9c09ab6 c5
  ff8b3c100cccc0d162e8fa5f4b914ff539c014c2 c4
  c59604b81999aa40d0d2c4b59f1c65894847c38f c3
  6f4d9f2c1e471ddba15f3caaa9d1182c94f94bf9 c2
  d0eeee3c1afc48fcfebf35d291c7cc5f86677c22 c1
  $ cd ..
  $ git daemon --base-path=. --export-all --reuseaddr --pid-file=pid --detach
  $ touch git-daemon-export-ok

  $ export MGIT_DEPTH=3
  $ mgit create disk.img --size=4MiB
  $ mgit pull disk.img -r git://localhost/repo#main
  refs/heads/main: + /f
  $ mgit head disk.img
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36
  $ git -C repo rev-parse HEAD
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36

  $ mgit get disk.img /f > got
  $ git -C repo show HEAD:f > expected
  $ diff expected got
  $ mgit bundle disk.img -o deep.bundle
  $ cd repo
  $ git bundle verify ../deep.bundle | ocaml $TRIM | head -5
  ../deep.bundle is okay
  The bundle contains this ref:
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36 refs/heads/main
  The bundle requires this ref:
  c59604b81999aa40d0d2c4b59f1c65894847c38f
  The bundle uses this hash algorithm: sha1
  $ git rev-parse HEAD~3
  c59604b81999aa40d0d2c4b59f1c65894847c38f
  $ cd ..

  $ cd repo
  $ echo "line 7" >> f
  $ git add f
  $ GIT_AUTHOR_DATE"1790697745 +0200" GIT_COMMITTER_DATE="1790697745 +0200" git commit -q -m c7
  GIT_AUTHOR_DATE1790697745 +0200: command not found
  [127]
  $ cd ..
  $ mgit pull disk.img -r git://localhost/repo#main
  $ mgit head disk.img
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36
  $ git -C repo rev-parse HEAD
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36
  $ mgit get disk.img /f > got
  $ git -C repo show HEAD:f > expected
  $ diff expected got

  $ mgit gc disk.img 1
  $ mgit head disk.img
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36
  $ mgit get disk.img /f > got
  $ diff expected got
  $ mgit bundle disk.img -o one.bundle
  $ cd repo
  $ git bundle verify ../one.bundle | ocaml $TRIM | head -5
  ../one.bundle is okay
  The bundle contains this ref:
  22a4cba9ad36ec56c8d4fcf8d082a29d88b02f36 refs/heads/main
  The bundle requires this ref:
  bef3acfd844ddf2cb034d2f530235823e9c09ab6
  The bundle uses this hash algorithm: sha1
  $ git rev-parse HEAD~1
  bef3acfd844ddf2cb034d2f530235823e9c09ab6
  $ cd ..

  $ kill $(cat pid)
