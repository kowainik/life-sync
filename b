#!/usr/bin/env bash
set -e

# DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script builds the project in a way that is convenient for developers.
# It passes the right flags into right places, builds the project with --fast,
# tidies up and highlights error messages in GHC output.

# USAGE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ./b                 build whole project with all targets
#   ./b -c              do stack clean
#   ./b -t              build and run tests
#   ./b -b              build and run benchmarks
#   ./b --nix           use nix to build package

args=''
test=false
bench=false
with_nix=false
clean=false

for var in "$@"
do
  # -t = run tests
  if [[ $var == "-t" ]]; then
    test=true
  # -b = run benchmarks
  elif [[ $var == "-b" ]]; then
    bench=true
  elif [[ $var == "--nix" ]]; then
    with_nix=true
  # -c = clean
  elif [[ $var == "-c" ]]; then
    clean=true
  else
    args="$args $var"
  fi
done

# Cleaning project
if [[ $clean == true ]]; then
  echo "Cleaning project..."
  stack clean
  exit
fi

if [[ $no_nix == true ]]; then
  args="$args --nix"
fi

xperl='$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p'
xgrep="((^.*warning.*$|^.*error.*$|^    .*$|^.*can't find source.*$|^Module imports form a cycle.*$|^  which imports.*$)|^)"

stack build $args                                    \
            --ghc-options="+RTS -A256m -n2m -RTS"    \
            --test                                   \
            --no-run-tests                           \
            --no-haddock-deps                        \
            --bench                                  \
            --no-run-benchmarks                      \
            --jobs=4                                 \
            --dependencies-only

stack build $args                                    \
            --fast                                   \
            --ghc-options="+RTS -A256m -n2m -RTS"    \
            --test                                   \
            --no-run-tests                           \
            --no-haddock-deps                        \
            --bench                                  \
            --no-run-benchmarks                      \
            --jobs=4 2>&1 | perl -pe "$xperl" | { grep -E --color "$xgrep" || true; }

if [[ $test == true ]]; then
  stack build $args                                  \
              --fast                                 \
              --ghc-options="+RTS -A256m -n2m -RTS"  \
              --test                                 \
              --no-haddock-deps                      \
              --bench                                \
              --no-run-benchmarks                    \
              --jobs=4
fi

if [[ $bench == true ]]; then
  stack build $args                                  \
              --fast                                 \
              --ghc-options="+RTS -A256m -n2m -RTS"  \
              --test                                 \
              --no-run-tests                         \
              --no-haddock-deps                      \
              --bench                                \
              --jobs=4
fi
