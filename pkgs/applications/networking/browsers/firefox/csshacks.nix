{ lib, fetchFromGitHub, ... }:

fetchFromGitHub {
  name = "firefox-csshacks";
  version = "20240906";

  owner = "MrOtherGuy";
  repo = "firefox-csshacks";
  rev = "d08d7791fa154c4ff4e374b87ecde16a1a4d7e17";
  sha256 = "sha256-wTqHc96IMHt1nOAvffA1ejVagQJmi+uiV6udOpLk4AM=";

  meta = with lib; {
    description = "Firefox CSS hacks";
    homepage = "https://github.com/MrOtherGuy/firefox-csshacks";
    license = licenses.mpl20;
    platforms = platforms.all;
    maintainers = [ ];
  };
}
