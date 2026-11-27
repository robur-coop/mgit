# Git in OCaml (also for unikernels)

`mgit` is a small implementation of Git in OCaml that can manipulate a
repository stored either in memory or as a single file (which, in the case of
unikernels, can be viewed as a block device). The aim of `mgit` is to provide a
fairly simple way to manage a Git repository by allowing:
- synchronisation with an existing repository (via TCP/IP, SSH or HTTP)
- the creation of commits that can aggregate multiple changes (file creation,
  deletion, modification), just as one would do manually.
- the ability to push these changes to an existing repository (via TCP/IP, SSH
  or HTTP). Commits can therefore be automatically generated and pushed.
- the ability to use this implemention within a unikernel (see
  [https://uniker.nl][uniker.nl]) (with all the associated type and memory
  constraints)

## Give a try!

The short example below creates an in-memory Git repository and adds a file
called `foo.txt` with the value `Hello World`. It then displays the commit
(which is reproducible as the default date is always 0; see the `now`
parameter).

```ocaml
module Git = Mgit_unix

let run () =
  let ( let* ) = Result.bind in
  let* t = Git.make Git.Mem in
  let fn t = Git.set t "foo.txt" "Hello World!" in
  let* () = Git.change_and_push t fn |> Result.join in
  begin match Git.commit t with
  | Some (`Clean uid) -> Fmt.pr "%s\n%!" (Mgit.uid_to_hex uid)
  | Some (`Dirty uid) -> Fmt.pr "~%s\n%!" (Mgit.uid_to_hex uid)
  | None -> Fmt.pr "no commit\n%!" end;
  Ok ()

let () = Miou_unix.run @@ fun () ->
  match run () with
  | Ok () -> ()
  | Error err -> Fmt.epr "%a\n%!" Mgit.pp_error err
```

You can simply compile the project as follows:

```shell
$ ocamlfind opt -linkpkg -package checkseum.c,digestif.c,mgit.unix main.ml
$ ./a.out
35624210f9ece4576d225edc16466dc01d58af64
```

`mgit` also offers a very small tool that allows you to save a Git repository to
a file containing absolutely everything; here is the same example as the one
shown in OCaml:

```shell
$ mgit create --size=4MiB disk.img
$ echo 'Hello World!' | mget set disk.img /foo.txt -m "First commit" -
eefb2efe7bc3415aa01ab18cd0b359b0af294d86
$ mgit get disk.img /foo.txt
Hello World!
```

### `mgit` and domains

The model currently used for reading from and writing to a Git repository
(whether in memory or on a block device) is one of multiple readers and
sequential writers: in other words, there can only be one active write
operation at any one time.

If you are interested in the possibility of having multiple writers at the same
time, we recommend [bancos][bancos], which has a more granular structure
allowing multiple writers to access the block device.

## [`ocaml-git`][ocaml-git] and [`git-kv`][git-kv]

`mgit` is a new iteration of `ocaml-git` and `git-kv`, for which I am currently
the maintainer and/or original author. These projects have been useful for
experimenting with the API and the implementations required to ensure
interoperability with Git. Furthermore:
- `ocaml-git` was an interesting experiment that enabled me, very quickly, to
  understand and implement the PACKv2 format used by Git to store Git objects.
  The back-and-forth between the implementation, what was required and how to
  integrate it into an existing framework led to the creation of `carton`, a
  scheduler-free implementation of the PACKv2 format (there was, of course, a
  need to implement [decompress][decompress], [duff][duff] and
  [digestif][digestif]).
- `git-kv` is a project that focuses on the high-level API. Initially, we had
  to use Irmin to manipulate a repository, but its level of abstraction was far
  too high for our purposes (particularly with regard to
  [dns-primary][dns-primary] or [opam-mirror][opam-mirror]). More specifically,
  `git-kv` provides an API that is more like a file system (open, read, write)
  with a few Git-related functions (fetch, push) and the protocol.

However, these projects have their limitations:
- `ocaml-git` is far too convoluted by abstractions which, in hindsight, are
  unnecessary but which we still have to adhere to, creating significant
  inertia when it comes to improving it.
- `git-kv` has an interesting API but still, in a sense, inherits the
  abstractions from `ocaml-git` and uses [lwt][lwt]. This makes it difficult to
  carry out an update that would retain support for this scheduler whilst
  introducing [Miou][miou] as the new scheduler

More generally, both implementations have memory usage that only increases over
time (and with the number of commits), which requires a rethink of the
fundamental building blocks needed to store Git objects, with the ability to
clean up those that are no longer needed. `mgit` now offers the `gc` function,
which allows you to retain only the necessary Git objects according to a
specified depth (of the history).

[ocaml-git]: https://github.com/mirage/ocaml-git
[git-kv]: https://github.com/robur-coop/git-kv.git
[uniker.nl]: https://uniker.nl
[dns-primary]: https://github.com/robur-coop/dns-primary-git
[opam-mirror]: https://github.com/robur-coop/opam-mirror
[decompress]: https://github.com/mirage/decompress
[duff]: https://github.com/mirage/duff
[digestif]: https://github.com/mirage/digestif
[lwt]: https://github.com/ocsigen/lwt
[miou]: https://github.com/robur-coop/miou
[bancos]: https://github.com/robur-coop/bancos
