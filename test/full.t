  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ commit () {
  >   echo "$1" >> repo/f
  >   git -C repo add f
  >   GIT_AUTHOR_DATE="$2 +0200" GIT_COMMITTER_DATE="$2 +0200" git -C repo commit -q -m "$1"
  > }
  $ git init -q repo 2> /dev/null
  $ git -C repo checkout -q -b main
  $ for i in 1 2 3 4 5; do commit "c$i" "1790770046"; done
  $ GIT_TRACE_PACKET=$PWD/trace git daemon --base-path=. --export-all --reuseaddr --pid-file=pid --detach
  $ export REMOTE=git://localhost/repo#main
  $ export GREP=$(pwd)/grep.ml
  $ complete() {
  >  rm -f $1.bundle
  >  mgit bundle $1 -o $1.bundle
  >  git -C repo bundle verify ../$1.bundle 2>&1 | ocaml $GREP complete requires
  > }

  $ mgit create full.img --size=4MiB
  $ MGIT_DEPTH=0 mgit pull full.img -r $REMOTE
  refs/heads/main: + /f
  $ complete full.img
  The bundle records a complete history.
  $ ocaml $GREP -c deepen < trace
  0
  $ mgit pack full.img -o full.pack --want $(mgit head full.img)
  $ git init -q client 2> /dev/null
  $ git -C client index-pack --stdin < full.pack > /dev/null
  $ git -C client update-ref refs/heads/main $(mgit head full.img)
  $ git -C client fsck --strict
  $ git -C client rev-list --count main
  5

  $ commit c6 1790770303
  $ MGIT_DEPTH=0 mgit pull full.img -r $REMOTE
  refs/heads/main: ~ /f
  $ complete full.img
  The bundle records a complete history.

  $ mgit create shallow.img --size=4MiB
  $ MGIT_DEPTH=2 mgit pull shallow.img -r $REMOTE
  refs/heads/main: + /f
  $ complete shallow.img
  The bundle requires this ref:
  $ : > trace
  $ MGIT_DEPTH=0 mgit pull shallow.img -r $REMOTE
  $ ocaml $GREP -o "deepen " < trace
  deepen 2147483647
  $ complete shallow.img
  The bundle records a complete history.
  $ git -C repo rev-parse main > expected
  $ mgit head shallow.img > got
  $ diff expected got

  $ MGIT_DEPTH=0 mgit gc full.img 0
  $ complete full.img
  The bundle records a complete history.
  $ mgit gc full.img 2
  $ complete full.img
  The bundle requires this ref:

  $ kill $(cat pid)
