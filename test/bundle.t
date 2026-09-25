  $ export MGIT_DATE=1790332361
  $ export MGIT_DEPTH=10
  $ mgit create --size=4MiB disk.img
  $ mgit set disk.img /foo -m "First commit" - <<EOF
  > Hello World!
  > EOF
  1c18c346279d5df77a1d1bfff64691f7396343af
  $ mgit set disk.img /dir/bar -m "Second commit" - <<EOF
  > Git rocks!
  > EOF
  f5e776a471ba963e17692e1c6b14494f7796dfea
  $ mgit head disk.img
  f5e776a471ba963e17692e1c6b14494f7796dfea
  $ mgit list disk.img /
  dir/
  foo
  $ mgit get disk.img /foo
  Hello World!

  $ mgit bundle disk.img -o full.bundle
  $ head -1 full.bundle
  # v2 git bundle
  $ mkdir empty
  $ cd empty
  $ git init -q 2> /dev/null
  $ git bundle list-heads ../full.bundle
  f5e776a471ba963e17692e1c6b14494f7796dfea refs/heads/main
  $ git bundle verify ../full.bundle
  ../full.bundle is okay
  The bundle contains this ref:
  f5e776a471ba963e17692e1c6b14494f7796dfea refs/heads/main
  The bundle records a complete history.
  The bundle uses this hash algorithm: sha1
  $ cd ..

  $ git clone -q -b main full.bundle clone
  $ cat clone/foo
  Hello World!
  $ cat clone/dir/bar
  Git rocks!
  $ git -C clone log --pretty=oneline
  f5e776a471ba963e17692e1c6b14494f7796dfea Second commit
  1c18c346279d5df77a1d1bfff64691f7396343af First commit

  $ mgit gc disk.img 1
  $ mgit bundle disk.img -o shallow.bundle
  $ head -3 shallow.bundle
  # v2 git bundle
  -1c18c346279d5df77a1d1bfff64691f7396343af
  f5e776a471ba963e17692e1c6b14494f7796dfea refs/heads/main
  $ mgit head disk.img
  f5e776a471ba963e17692e1c6b14494f7796dfea

  $ cd empty
  $ git bundle verify ../shallow.bundle
  error: Repository lacks these prerequisite commits:
  error: 1c18c346279d5df77a1d1bfff64691f7396343af 
  [1]
  $ cd ../clone
  $ git bundle verify ../shallow.bundle
  ../shallow.bundle is okay
  The bundle contains this ref:
  f5e776a471ba963e17692e1c6b14494f7796dfea refs/heads/main
  The bundle requires this ref:
  1c18c346279d5df77a1d1bfff64691f7396343af 
  The bundle uses this hash algorithm: sha1
  $ cd ..
