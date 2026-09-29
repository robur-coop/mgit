  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export GIT_AUTHOR_DATE="1790689613 +0200"
  $ export GIT_COMMITTER_DATE="1790689613 +0200"
  $ export MGIT_DATE=1790689613
  $ git init -q --bare repo.git 2> /dev/null
  $ git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --pid-file=pid --detach
  $ export MGIT_REMOTE=git://localhost/repo.git#main

  $ mgit create disk.img --size 4MiB
  $ mgit set -m first disk.img /foo - <<EOF
  > Hello World!
  > EOF
  d33cc4c49774968cc6e9962ceeed6cbc2966dfe0
  $ mgit set -m second disk.img /dir/bar - <<EOF
  > Git rocks!
  > EOF
  10377be915d8581fa924e9436f326ef0e92f52d1
  $ git -C repo.git log --pretty=oneline main
  10377be915d8581fa924e9436f326ef0e92f52d1 second
  d33cc4c49774968cc6e9962ceeed6cbc2966dfe0 first
  $ git -C repo.git fsck --strict
  $ git clone -q -b main repo.git clone
  $ cat clone/foo
  Hello World!
  $ cat clone/dir/bar
  Git rocks!

  $ cd clone
  $ echo "upstream" > up
  $ git add up
  $ git commit -q -m upstream
  $ git push -q origin main
  $ git rev-parse HEAD > ../upstream
  $ cd ..
  $ mgit set disk.img /bar -m third - <<EOF
  > Too early
  > EOF
  mgit: refs/heads/main: the remote has commits we do not have
        (non-fast-forward), pull first
  [124]
  $ git -C repo.git rev-parse main | diff - upstream

  $ mgit pull disk.img -r $MGIT_REMOTE
  refs/heads/main: + /up
  refs/heads/main: - /bar
  $ mgit set disk.img /bar -m fourth - <<EOF
  > now
  > EOF
  7dfaed9bb6e5f46d3dfd6f7eee2903183064c9ab
  $ git -C repo.git log --pretty=oneline main
  7dfaed9bb6e5f46d3dfd6f7eee2903183064c9ab fourth
  add2bac8bb3e5b22dc42fb1fb2367a8d3704260a upstream
  10377be915d8581fa924e9436f326ef0e92f52d1 second
  d33cc4c49774968cc6e9962ceeed6cbc2966dfe0 first
  $ git -C repo.git fsck --strict
  $ git -C repo.git show main:bar
  now
  $ git -C repo.git show main:up
  upstream

  $ kill $(cat pid)
