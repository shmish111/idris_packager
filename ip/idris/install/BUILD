load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_library(
    name = "install",
    srcs = glob(["*.scala"]),
    deps = [
      "//ip/logger:logger",
      "//ip/result:result",
      "//ip/ipkgops:ipkgops",
      "//ip/stringext:stringext",
      "//ip/zipops:zipops",
      "//ip/fileops:fileops",
      "//ip/resources:resources",
      "//ip/terminate:terminate",
      "//ip/describe:describe",
      "//ip/fileops/path:path"],
    visibility = ["//visibility:public"]
)


