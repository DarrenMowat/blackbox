#! /bin/bash
./clean.sh
test -d "results" || mkdir "results"
test -d "results/Types" || cp -r "Types" "results"
# 
echo "Running ScopeTests.hs through Blackbox"
../dist/build/blackbox/blackbox -f ./ScopeTests.hs -m ./ScopeTests.hs -g `which ghci` 1>results/ScopeTests.hs 2>results/ScopeTests.log
ghc --make results/ScopeTests.hs -o results/ScopeTests 1> /dev/null || echo "Failed to compile ScopeTests.hs produced by Blackbox"
results/ScopeTests
#
echo "Running SplitTests.hs through Blackbox"
../dist/build/blackbox/blackbox -f ./SplitTests.hs -m ./SplitTests.hs -g `which ghci` 1>results/SplitTests.hs 2>results/SplitTests.log
ghc --make results/SplitTests.hs -o results/SplitTests 1> /dev/null || echo "Failed to compile SplitTests.hs produced by Blackbox"
results/SplitTests
#
echo "Running TypeLineTests.hs through Blackbox"
../dist/build/blackbox/blackbox -f ./TypeLineTests.hs -m ./TypeLineTests.hs -g `which ghci` 1>results/TypeLineTests.hs 2>results/TypeLineTests.log
ghc --make results/TypeLineTests.hs -o results/TypeLineTests 1> /dev/null || echo "Failed to compile TypeLineTests.hs produced by Blackbox"
results/TypeLineTests
#
echo "Running CombinedTests.hs through Blackbox"
../dist/build/blackbox/blackbox -f ./CombinedTests.hs -m ./CombinedTests.hs -g `which ghci` 1>results/CombinedTests.hs 2>results/CombinedTests.log
ghc --make results/CombinedTests.hs -o results/CombinedTests 1> /dev/null || echo "Failed to compile CombinedTests.hs produced by Blackbox"
results/CombinedTests