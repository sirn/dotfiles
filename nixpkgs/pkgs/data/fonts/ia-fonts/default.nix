{ lib, fetchFromGitHub }:

let
  version = "20220921";
  owner = "iaolo";
  repo = "iA-Fonts";
  rev = "8548eea983611993db2d6f65bd3ae1609f62dcbd";
in
{
  ia-writer-duo-static = fetchFromGitHub {
    inherit owner repo rev;
    name = "ia-writer-duo-static-${version}";
    sha256 = "sha256-HDd27M2RkcXJaQxt2QjeUKCAUTJCEO4vkBZt/Z2bVO4=";

    postFetch = ''
      mkdir -p $out/share/fonts/truetype/ia-writer-duo-static/
      cp $out/"iA Writer Duo/Static/"*.ttf $out/share/fonts/truetype/ia-writer-duo-static/
      shopt -s extglob dotglob
      rm -rf $out/!(share)
      shopt -u extglob dotglob
    '';

    meta = with lib; {
      description = "iA Fonts";
      homepage = "https://ia.net/topics/in-search-of-the-perfect-writing-font";
      license = licenses.ofl;
      platforms = platforms.all;
      maintainers = [ ];
    };
  };

  ia-writer-mono-static = fetchFromGitHub {
    inherit owner repo rev;
    name = "ia-writer-mono-static-${version}";
    sha256 = "sha256-UDMP5RVZVrr3dDEpOfqEmHrxQTNYL79kzCUQaPEBtYY=";

    postFetch = ''
      mkdir -p $out/share/fonts/truetype/ia-writer-mono-static/
      cp $out/"iA Writer Mono/Static/"*.ttf $out/share/fonts/truetype/ia-writer-mono-static/
      shopt -s extglob dotglob
      rm -rf $out/!(share)
      shopt -u extglob dotglob
    '';

    meta = with lib; {
      description = "iA Fonts";
      homepage = "https://ia.net/topics/in-search-of-the-perfect-writing-font";
      license = licenses.ofl;
      platforms = platforms.all;
      maintainers = [ ];
    };
  };

  ia-writer-quattro-static = fetchFromGitHub {
    inherit owner repo rev;
    name = "ia-writer-quattro-static-${version}";
    sha256 = "sha256-1dS2jzmp4NsZAOX9x02J+4pZw02E0OgU+8D3mEJhNcA=";

    postFetch = ''
      mkdir -p $out/share/fonts/truetype/ia-writer-quattro-static/
      cp $out/"iA Writer Quattro/Static/"*.ttf $out/share/fonts/truetype/ia-writer-quattro-static/
      shopt -s extglob dotglob
      rm -rf $out/!(share)
      shopt -u extglob dotglob
    '';

    meta = with lib; {
      description = "iA Fonts";
      homepage = "https://ia.net/topics/in-search-of-the-perfect-writing-font";
      license = licenses.ofl;
      platforms = platforms.all;
      maintainers = [ ];
    };
  };
}
