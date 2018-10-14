load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_library(
    name = "path",
    srcs = glob(["*.scala"]),
    deps = ["//ip/result:result", "//ip/describe:describe", "//ip/terminate:terminate"],
    exports = ["//ip/describe:describe"],
    visibility = ["//visibility:public"]
)

