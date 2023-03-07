#!/bin/bash

##### bootstrap.sh - set-up system and build.
export DEBIAN_FRONTEND=noninteractive

# Env
OPAM_SWITCH_VERSION=4.12.0
Z3_VERSION=4.11.2
CORES=2

# Setup Dependencies
echo "[NOTE] Start Setup System Dependencies"
sudo apt-get update >/dev/null
for pkg in "cmake" "build-essential" "python2.7" "libgmp-dev" "opam" "ocaml-findlib" "python3" "python3-pip"; do
  sudo apt-get install -y -qq $pkg >/dev/null 2>&1
  echo "[NOTE] $pkg: Installed"
done
echo "[NOTE] End-up Setup System Dependencies"

# Setup Python Dependencies
echo "[NOTE] Start Setup Python Dependencies"
for pkg in "z3-solver 4.8.9"; do
  pkg_pair=( $pkg )
  pkg_name=${pkg_pair[0]}
  pkg_version=${pkg_pair[1]}
  pip3 install "$pkg_name>=$pkg_version" >/dev/null 2>&1
  echo "[NOTE] $pkg: Installed as Python package"
done
echo "[NOTE] End-up Setup Python Dependencies"

# Initialize opam
echo "[NOTE] Start Initialize OPAM"
opam init -y --bare >/dev/null
opam update >/dev/null
eval $(opam env)
if [[ ! "$(ocaml --version)" =~ "$OPAM_SWITCH_VERSION" ]]; then
  if [[ "$(opam switch list 2>/dev/null | grep -c "$OPAM_SWITCH_VERSION")" -eq 0 ]]; then
    opam switch create $OPAM_SWITCH_VERSION >/dev/null
  else
    opam switch $OPAM_SWITCH_VERSION >/dev/null
  fi
fi
eval $(opam env)
echo "[NOTE] Current OCAML version is $(ocaml --version | grep -P "\d+\.\d+\.\d+" -o)"
OPAM_LIB_DIR=~/.opam/$OPAM_SWITCH_VERSION/lib/
echo "[NOTE] End-up Initialize OPAM"

# Setup OCAML Dependencies
echo "[NOTE] Start Setup OCAML Dependencies"
eval $(opam env)
for pkg in "batteries 3.5.1" "core 0.15.0" "dune 3.5.0" "menhir 20200624" "zarith 1.10" "logs 0.7.0" "mtime 1.2.0" "yojson 2.0.2"; do
  pkg_pair=( $pkg )
  pkg_name=${pkg_pair[0]}
  pkg_version=${pkg_pair[1]}
  opam install -y -j $CORES "$pkg_name>=$pkg_version" >/dev/null 2>&1
  echo "[NOTE] $pkg_name: Installed as OCAML package"
done
echo "[NOTE] End-up Setup OCAML Dependencies"

# Install Z3
eval $(opam env)
if [[ ! -d "${OPAM_LIB_DIR%%/}/z3" ]]; then
  echo "[NOTE] Start Install Z3"
  curl -L -o z3-$Z3_VERSION.tar.gz https://github.com/Z3Prover/z3/archive/z3-$Z3_VERSION.tar.gz >/dev/null 2>&1 && \
    tar -zxvf z3-$Z3_VERSION.tar.gz >/dev/null 2>&1 && \
    rm z3-$Z3_VERSION.tar.gz >/dev/null
  Z3_DIR=~/z3-z3-$Z3_VERSION/
  cd ${Z3_DIR%%/}/ && \
    python3 scripts/mk_make.py --ml >/dev/null
  cd ${Z3_DIR%%/}/build && \
    eval $(opam env) && \
    make -j $CORES >/dev/null 2>&1
  sudo make install >/dev/null && \
    rm -rf ${Z3_DIR%%/}
  ln -s ${OPAM_LIB_DIR%%/}/Z3 ${OPAM_LIB_DIR%%/}/z3
  echo "[NOTE] End-up Install Z3"
fi

# Link with mount directory
if [[ ! -d "/home/vagrant/develop" ]]; then
  ln -s /vagrant /home/vagrant/develop
fi

echo "eval \$(opam env)" >> ~/.bashrc
