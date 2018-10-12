load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_binary(
    name = "idrisPackager",
    srcs = glob(["*.scala"]),
    deps = [
      "//ip/logger:logger",
      "//ip/result:result",
      "//ip/resources:resources",
      "//ip/tupleops:tupleops",
      "//ip/zipops:zipops",
      "//ip/fileops:fileops",
      "//ip/ipkgops:ipkgops",
      "//ip/fileops/path:path"],
    main_class = "ip.IdrisPackager",
    visibility = ["//visibility:public"],
)

