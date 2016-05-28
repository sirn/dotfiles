{ self, fetchurl, fetchgit ? null, lib }:

{
  by-spec."typescript"."1.7.5" =
    self.by-version."typescript"."1.7.5";
  by-version."typescript"."1.7.5" = self.buildNodePackage {
    name = "typescript-1.7.5";
    version = "1.7.5";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/typescript/-/typescript-1.7.5.tgz";
      name = "typescript-1.7.5.tgz";
      sha1 = "a80fc231d9b95afeb9d10f0589560428dd0a045e";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."typescript-tools"."git://github.com/sirn/typescript-tools.git#typescript-1.7-exp" =
    self.by-version."typescript-tools"."v0.4.7";
  by-version."typescript-tools"."v0.4.7" = self.buildNodePackage {
    name = "typescript-tools-v0.4.7";
    version = "v0.4.7";
    bin = true;
    src = fetchgit {
      url = "git://github.com/sirn/typescript-tools.git";
      rev = "dc6b8b955602cc6938b6eb63c734b2ea78000d82";
      sha256 = "b99797f6536071ddcd0f0ba4e977a61ad49e07c162c98296a0edda6eb0cf5efb";
    };
    deps = {
      "typescript-1.7.5" = self.by-version."typescript"."1.7.5";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "typescript-tools" = self.by-version."typescript-tools"."v0.4.7";
}
