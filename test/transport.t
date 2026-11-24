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

  $ mgit.http . 8765 &
  $ while [ ! -f git-http-ok ]; do sleep 0.1; done
  $ mgit create http.img --size=4MiB
  $ mgit pull http.img -r http://localhost:8765/repo.git#main
  refs/heads/main: + /f
  refs/heads/main: + /ssh
  $ commit c6 work 1795521720
  $ git -C work push -q up main
  $ mgit pull http.img -r http://localhost:8765/repo.git#main
  refs/heads/main: ~ /f
  $ check http.img
  ok
  ok
  $ echo "through http" | MGIT_REMOTE=http://localhost:8765/repo.git#main mgit set http.img /http -
  1993ec3f3cbae4e3ca1d0b8b527d90047abec4c5
  $ git -C repo.git show main:http
  through http
  $ git -C repo.git fsck --strict
  $ kill %1

  $ git -C work pull -q up main
  $ git clone -q --bare repo.git other.git
  $ git clone -q other.git other 2> /dev/null
  $ c() {
  >   echo "o$1" >> other/g
  >   git -C other add g
  >   GIT_AUTHOR_DATE="$2 +0000" GIT_COMMITTER_DATE="$2 +0000" git -C other commit -q -m "o$i"
  > } 
  $ c 1 1795521721
  $ c 2 1795521722
  $ c 3 1795521723
  $ c 4 1795521724
  $ c 5 1795521725
  $ c 6 1795521726
  $ c 7 1795521727
  $ c 8 1795521728
  $ c 9 1795521729
  $ c 10 1795521730
  $ c 11 1795521731
  $ c 12 1795521732
  $ c 13 1795521733
  $ c 14 1795521734
  $ c 15 1795521735
  $ c 16 1795521736
  $ c 17 1795521737
  $ c 18 1795521738
  $ c 19 1795521739
  $ c 20 1795521740
  $ c 21 1795521741
  $ c 22 1795521742
  $ c 23 1795521743
  $ c 24 1795521744
  $ c 25 1795521745
  $ c 26 1795521746
  $ c 27 1795521747
  $ c 28 1795521748
  $ c 29 1795521749
  $ c 30 1795521750
  $ c 31 1795521751
  $ c 32 1795521752
  $ c 33 1795521753
  $ c 34 1795521754
  $ c 35 1795521755
  $ c 36 1795521756
  $ c 37 1795521757
  $ c 38 1795521758
  $ c 39 1795521759
  $ c 40 1795521760
  $ git -C other push -q origin main
  $ commit c7 work 1795521761
  $ git -C work push -q up main
  $ GIT_HTTP_V0=1 mgit.http . 8765 &
  $ while [ ! -f git-http-ok ]; do sleep 0.1; done
  $ export MGIT_DEPTH=60
  $ mgit create v1.img --size=8MiB
  $ mgit pull v1.img -r http://localhost:8765/other.git#main > /dev/null
  $ mgit pull v1.img -r http://localhost:8765/repo.git#main
  refs/heads/main: - /g
  refs/heads/main: ~ /f
  $ check v1.img
  ok
  ok
  $ kill %1
