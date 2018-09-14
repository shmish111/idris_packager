load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_library(
    name = "resources",
    srcs = glob(["*.scala"]),
    deps = [
      "//ip/fileops/path:path",
      "//ip/resource:resource"],
    visibility = ["//visibility:public"]
)


