# If a fork of these scripts is specified, use that GitHub user instead
fork_user=${FORK_USER:-ocaml}

# If a branch of these scripts is specified, use that branch instead of 'master'
fork_branch=${FORK_BRANCH:-master}

### Bootstrap

set -uex

get() {
  wget https://raw.githubusercontent.com/${fork_user}/ocaml-ci-scripts/${fork_branch}/$@
}

REPO_DIR=$(pwd)
TMP_BUILD=$(mktemp -d)
cd ${TMP_BUILD}

get .travis-ocaml.sh
sh .travis-ocaml.sh

export OPAMYES=1
eval $(opam config env)

opam switch create 4.10.0+multicore --packages=ocaml-variants.4.10.0+multicore,ocaml-secondary-compiler --repositories=multicore=git+https://github.com/ocamllabs/multicore-opam.git,default
eval $(opam config env)
opam install ocamlfind ocamlbuild
cd $REPO_DIR
make
