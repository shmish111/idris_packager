#rules_scala_version="63eab9f4d80612e918ba954211f377cc83d27a07" # update this as needed
rules_scala_version="b537bddc58a77318b34165812a0311ef52806318"

http_archive(
             name = "io_bazel_rules_scala",
             url = "https://github.com/bazelbuild/rules_scala/archive/%s.zip"%rules_scala_version,
             type = "zip",
             strip_prefix= "rules_scala-%s" % rules_scala_version
             )

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories(("2.12.6", {
    "scala_compiler": "3023b07cc02f2b0217b2c04f8e636b396130b3a8544a8dfad498a19c3e57a863",
    "scala_library": "f81d7144f0ce1b8123335b72ba39003c4be2870767aca15dd0888ba3dab65e98",
    "scala_reflect": "ffa70d522fc9f9deec14358aa674e6dd75c9dfa39d4668ef15bb52f002ce99fa"
}))

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
register_toolchains("//toolchains:bryght_scala_toolchain")
