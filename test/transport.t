  $ export GIT_AUTHOR_NAME="Romain Calascibetta"
  $ export GIT_AUTHOR_EMAIL="din@osau.re"
  $ export GIT_COMMITTER_NAME="Romain Calascibetta"
  $ export GIT_COMMITTER_EMAIL="din@osau.re"
  $ export MGIT_DATE=1795521714
  $ commit () {
  >   echo "$1" >> $2/f
  >   git -C $2 add f
  >   GIT_AUTHOR_DATE="$3 +0000" GIT_COMMITTER_DATE="$3 +0000" git -C $2 commit -q -m "$1"
  > }
  $ git init -q -b main work 2> /dev/null
  $ commit "c1" work 1795521715
  $ commit "c2" work 1795521716
  $ commit "c3" work 1795521717
  $ git clone -q --bare work repo.git
  $ git -C work remote add up ../repo.git
  $ check () {
  >   test "$(mgit head $1)" = "$(git -C repo.git rev-parse main)" && echo "ok"
  >   mgit get $1 /f > got
  >   git -C repo.git show main:f > expected
  >   diff got expected && echo "ok"
  > }

  $ export MGIT_SSH=$PWD/fake-ssh
  $ export FAKE_SSH_LOG=$PWD/ssh.log
  $ mgit create ssh.img --size=4MiB
  $ mgit pull ssh.img -r localhost:repo.git#main
  refs/heads/main: + /f
  $ commit c4 work 1795521718
  $ git -C work push -q up main
  $ mgit pull ssh.img -r localhost:repo.git#main
  refs/heads/main: ~ /f
  $ check ssh.img
  ok
  ok
  $ echo "through ssh" | MGIT_REMOTE=localhost:repo.git#main mgit set ssh.img /ssh - > /dev/null
  $ git -C repo.git show main:ssh
  through ssh
  $ git -C repo.git fsck --strict
  $ cat ssh.log
  host=localhost command=git-upload-pack 'repo.git' protocol=version=2
  host=localhost command=git-upload-pack 'repo.git' protocol=version=2
  host=localhost command=git-receive-pack 'repo.git' protocol=none

  $ git -C work pull -q up main
  $ : > ssh.log
  $ export FAKE_SSH_NO_ENV=1
  $ mgit create ssh1.img --size=4MiB
  $ mgit pull ssh1.img -r localhost:repo.git#main
  refs/heads/main: + /f
  refs/heads/main: + /ssh
  $ commit c5 work 1795521719
  $ git -C work push -q up main
  $ mgit pull ssh1.img -r localhost:repo.git#main
  refs/heads/main: ~ /f
  $ check ssh1.img
  ok
  ok
  $ cat ssh.log
  host=localhost command=git-upload-pack 'repo.git' protocol=none
  host=localhost command=git-upload-pack 'repo.git' protocol=none
  $ unset FAKE_SSH_NO_ENV
