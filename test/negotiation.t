  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export MGIT_DATE=1790777356
  $ export MGIT_DEPTH=20
  $ export GREP=$(pwd)/grep.ml

  $ mkdir repo
  $ cd repo
  $ git init -q 2> /dev/null
  $ git config init.defaultBranch main
  $ git checkout -b main -q
  $ for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24; do
  >   echo "line $i" >> f
  >   git add f
  >   GIT_AUTHOR_DATE="1790777356 +0200" \
  >   GIT_COMMITTER_DATE="1790777356 +0200" \
  >   git commit -q -m "c$i"
  > done
  $ cd ..
  $ GIT_TRACE_PACKET=$PWD/trace git daemon --base-path=. --export-all --reuseaddr --pid-file=pid --detach
  $ touch git-daemon-export-ok

  $ commit () {
  >   cd repo
  >   echo "$1" >> f
  >   git add f
  >   GIT_AUTHOR_DATE="$2 +0200" GIT_COMMITTER_DATE="$2 +0200" git commit -q -m "$1"
  >   cd ..
  > }
  $ check () {
  >   test "$(mgit head $1)" = "$(git -C repo rev-parse HEAD)" && echo "okay"
  >   mgit get $1 /f > got
  >   git -C repo show HEAD:f > expected
  >   diff expected got && echo "okay"
  > }

  $ mgit create v2.img --size=4MiB
  $ mgit pull v2.img -r git://localhost/repo#main
  refs/heads/main: + /f
  $ check v2.img
  okay
  okay
  $ commit v2 1790778102
  $ : > trace
  $ mgit pull v2.img -r git://localhost/repo#main
  refs/heads/main: ~ /f
  $ check v2.img
  okay
  okay
  $ ocaml $GREP -c 'upload-pack< have ' < trace
  16
  $ ocaml $GREP -c 'upload-pack> ACK ' < trace
  1
  $ ocaml $GREP -c 'upload-pack< thin-pack' < trace
  1

  $ export MGIT_SSH=$PWD/fake-ssh FAKE_SSH_NO_ENV=1
  $ export GIT_TRACE_PACKET=$PWD/trace
  $ mgit create v1.img --size=4MiB
  $ mgit pull v1.img -r localhost:repo#main
  refs/heads/main: + /f
  $ check v1.img
  okay
  okay
  $ commit v1 1790778756
  $ : > trace
  $ mgit pull v1.img -r localhost:repo#main
  refs/heads/main: ~ /f
  $ check v1.img
  okay
  okay
  $ ocaml $GREP -c 'upload-pack< have ' < trace
  20
  $ ocaml $GREP -c 'upload-pack> ACK ' < trace
  22
  $ ocaml $GREP 'upload-pack< want ' < trace | ocaml $GREP -c thin-pack
  1

  $ kill $(cat pid)
