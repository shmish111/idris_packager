load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_library(
    name = "idris",
    srcs = glob(["*.scala"]),
    deps = [
      "//ip/logger:logger",
      "//ip/result:result",
      "//ip/terminate:terminate",
      "//ip/describe:describe",
      "//ip/fileops/path:path"],
    visibility = ["//visibility:public"]
)


