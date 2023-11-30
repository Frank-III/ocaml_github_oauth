(lang dune 3.8)

(name oauth_dream)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Frank Wang")

(maintainers "Frank Wang")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oauth_dream)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (ocaml (>= "5.0")) 
   (dune (>= "3.8.2"))
   dream dream-livereload
   dotenv
   base fmt ppx_let lwt cohttp-lwt-unix
   yojson ppx_yojson_conv
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
