{ self, fetchurl, fetchgit ? null, lib }:

{
  by-spec."typescript"."*" =
    self.by-version."typescript"."1.8.10";
  by-version."typescript"."1.8.10" = self.buildNodePackage {
    name = "typescript-1.8.10";
    version = "1.8.10";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/typescript/-/typescript-1.8.10.tgz";
      name = "typescript-1.8.10.tgz";
      sha1 = "b475d6e0dff0bf50f296e5ca6ef9fbb5c7320f1e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "typescript" = self.by-version."typescript"."1.8.10";
}
