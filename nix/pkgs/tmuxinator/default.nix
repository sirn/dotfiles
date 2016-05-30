{ buildRubyGem, makeWrapper, ruby }:

buildRubyGem rec {
  inherit ruby;
  name = "${gemName}-${version}";
  gemName = "tmuxinator";
  version = "0.8.1";
  sha256 = "1cpmlfa684j9r1hjya70nfcl5lzdbzmbi9hqbs5nhxha97b77qs5";

  # Horrible hack until BUNDLE_FROZEN='1' is removed from bundleEnv
  # as it breaks anything that runs under Tmuxinator that use Bundler.

  erubis = buildRubyGem rec {
    inherit ruby;
    name = "ruby${ruby.version}-${gemName}-${version}";
    gemName = "erubis";
    version = "2.7.0";
    sha256 = "1fj827xqjs91yqsydf0zmfyw9p4l2jz5yikg3mppz6d7fi8kyrb3";
  };

  thor = buildRubyGem rec {
    inherit ruby;
    name = "ruby${ruby.version}-${gemName}-${version}";
    gemName = "thor";
    version = "0.19.1";
    sha256 = "08p5gx18yrbnwc6xc0mxvsfaxzgy2y9i78xq7ds0qmdm67q39y4z";
  };

  propagatedBuildInputs = [ erubis thor ];
}