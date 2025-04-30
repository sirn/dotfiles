{ lib, fetchFromGitHub, ... }:

fetchFromGitHub {
  name = "firefox-csshacks";
  version = "20240906";

  owner = "MrOtherGuy";
  repo = "firefox-csshacks";
  rev = "016521f0a21bbb76e8eff4b8410c1e049f081c77";
  sha256 = "sha256-dUboMxvWSP1PS9NT8PsmfOMF1HKqvH6jUAT1La5k6wM=";

  meta = with lib; {
    description = "Firefox CSS hacks";
    homepage = "https://github.com/MrOtherGuy/firefox-csshacks";
    license = licenses.mpl20;
    platforms = platforms.all;
    maintainers = [ ];
  };
}
