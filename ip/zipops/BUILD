load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_library(
    name = "zipops",
    srcs = glob(["*.scala"]),
    deps = [
      "//ip/runnablestream:runnablestream",
      "//ip/eitherops:eitherops",
      "//ip/resources:resources",
      "//ip/fileops/path:path",
      "//ip/fileops:fileops",
      "//ip/resource:resource"],
    visibility = ["//visibility:public"]
)


