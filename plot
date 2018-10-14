#!/bin/bash 

#(bazel query --noimplicit_deps  'deps(//ip:idrisPackager)' --output graph > /tmp/graph.in) && \
#(bazel query --noimplicit_deps  'buildfiles(deps(//ip:idrisPackager))' --output graph > /tmp/graph.in) && \
(bazel query --noimplicit_deps  'kind(".* rule", deps(//ip:idrisPackager))' --output graph > /tmp/graph.in) && \
dot -Tpng < /tmp/graph.in > "$1"
