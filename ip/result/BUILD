load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_library(
    name = "result",
    srcs = glob(["*.scala"]),
    deps = ["//ip/terminate:terminate"],
    visibility = ["//visibility:public"]
)


