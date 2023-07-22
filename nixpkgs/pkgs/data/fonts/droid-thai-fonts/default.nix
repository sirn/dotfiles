{ lib, fetchzip }:

let
  version = "20190629";
in
{
  droid-sans-thai = fetchzip {
    name = "droid-sans-thai-${version}";
    url = "https://files.grid.in.th/pub/fonts/droid-sans-thai.tar.gz";
    sha256 = "sha256-lydkvmPH7SVfymZFwwZ1hbS4jlHZoGZyNWMUHzhl6rA=";

    postFetch = ''
      mkdir -p $out/share/fonts/truetype/droid-sans-thai/
      cp $out/*.ttf $out/share/fonts/truetype/droid-sans-thai/
      shopt -s extglob dotglob
      rm -rf $out/!(share)
      shopt -u extglob dotglob
    '';

    meta = with lib; {
      description = "Droid Sans Thai font";
      homepage = "https://www.android.com/";
      license = licenses.asl20;
      platforms = platforms.all;
      maintainers = [ ];
    };
  };

  droid-serif-thai = fetchzip {
    name = "droid-serif-thai-${version}";
    url = "https://files.grid.in.th/pub/fonts/droid-serif-thai.tar.gz";
    sha256 = "sha256-OYOx/dJyjIhh8DWQMPlHUlAuKnpTsVFfPHNYUV8mEz8=";

    postFetch = ''
      mkdir -p $out/share/fonts/truetype/droid-serif-thai/
      cp $out/*.ttf $out/share/fonts/truetype/droid-serif-thai/
      shopt -s extglob dotglob
      rm -rf $out/!(share)
      shopt -u extglob dotglob
    '';

    meta = with lib; {
      description = "Droid Serif Thai font";
      homepage = "https://www.android.com/";
      license = licenses.asl20;
      platforms = platforms.all;
      maintainers = [ ];
    };
  };
}
