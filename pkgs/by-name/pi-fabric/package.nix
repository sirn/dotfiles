{
  lib,
  buildNpmPackage,
  esbuild,
  fetchurl,
  nodejs_24,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
in
buildNpmPackage rec {
  pname = "pi-fabric";
  inherit version;

  src = fetchurl {
    url = "https://registry.npmjs.org/pi-fabric/-/pi-fabric-${version}.tgz";
    hash = sources.srcHash;
  };

  # The published tarball ships prebuilt `dist` and bundled `skills`; only
  # runtime dependencies need installing. The tarball also omits the lockfile
  # and declares peer deps on Pi packages that are not on the npm registry, so
  # vendor a lockfile generated from a peer/dev-stripped package.json.
  dontNpmBuild = true;

  postPatch = ''
    cp ${./package.json} ./package.json
    cp ${./package-lock.json} ./package-lock.json
  '';

  # engines.node >=24; pi-code-previews requires >=22.19.
  nodejs = nodejs_24;

  nativeBuildInputs = [ esbuild ];

  npmDepsHash = sources.npmDepsHash;

  # Shiki 4 loads every theme and language via a dynamic
  # import("@shikijs/<pkg>/<id>") from within its own dist, and even
  # shiki/bundle/full only re-exports those lazy loaders. Pi's extension host
  # cannot resolve cross-package dynamic imports, so highlighting fails and
  # previews fall back to plain text. Bundle shiki/bundle/full with esbuild at
  # build time into a single self-contained module (all themes, langs, and the
  # inlined oniguruma wasm resolved statically), then repoint pi-fabric at it.
  postInstall = ''
    fab="$out/lib/node_modules/$pname"
    entry="$fab/shiki-inline-entry.mjs"
    printf '%s\n' 'export { bundledLanguages, bundledThemes, bundledThemesInfo, createHighlighter } from "shiki/bundle/full";' > "$entry"
    esbuild --bundle --format=esm --platform=node \
      --outfile="$fab/node_modules/shiki/dist/shiki-inline.mjs" "$entry"
    rm "$entry"
    # Shiki ships metadata and the highlighter behind subpath/dynamic imports;
    # repoint each to the single bundled module so previews resolve statically.
    substituteInPlace \
      "$fab/dist/ui/highlight.js" \
      --replace-fail 'from "shiki/langs"' 'from "shiki/dist/shiki-inline.mjs"' \
      --replace-fail 'from "shiki/themes"' 'from "shiki/dist/shiki-inline.mjs"' \
      --replace-fail 'await import("shiki")' 'await import("shiki/dist/shiki-inline.mjs")'
    substituteInPlace \
      "$fab/dist/ui/core-tool-render.js" \
      --replace-fail 'from "shiki/langs"' 'from "shiki/dist/shiki-inline.mjs"' \
      --replace-fail 'from "shiki/themes"' 'from "shiki/dist/shiki-inline.mjs"'
  '';

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "Programmable tool and agent runtime for Pi — type-checked TypeScript execution, MCP, subagents, actors, and mesh coordination";
    homepage = "https://github.com/monotykamary/pi-fabric";
    license = licenses.mit;
    platforms = platforms.linux ++ platforms.darwin;
  };
}
