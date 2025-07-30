{ config, lib, pkgs, ... }:

let
  pythonPackages = pkgs.unstable.python312Packages;

  google-genai = pythonPackages.buildPythonPackage rec {
    pname = "google-genai";
    version = "1.27.0";
    pyproject = true;
    pythonRelaxDeps = true;

    src = pythonPackages.fetchPypi {
      pname = "google_genai";
      inherit version;
      sha256 = "0dgnis297m83wif5pxanf9hj48fqci320xxbp58dlf1rggz3z88m";
    };

    build-system = with pythonPackages; [
      setuptools
    ];

    nativeBuildInputs = with pythonPackages; [
      pkginfo
      twine
    ];

    propagatedBuildInputs = with pythonPackages; [
      anyio
      google-auth
      httpx
      pydantic
      tenacity
      websockets
      typing-extensions
    ];

    doCheck = false;
  };

  google-cloud-aiplatform = pythonPackages.buildPythonPackage rec {
    pname = "google-cloud-aiplatform";
    version = "1.105.0";
    pyproject = true;
    pythonRelaxDeps = true;

    src = pythonPackages.fetchPypi {
      pname = "google_cloud_aiplatform";
      inherit version;
      sha256 = "749c1230826198fa55d7c38774391f1fa57b9cd021a0e6ad1c788f8bca279555";
    };

    build-system = with pythonPackages; [
      setuptools
    ];

    propagatedBuildInputs = with pythonPackages; [
      google-api-core
      google-auth
      proto-plus
      protobuf
      packaging
      google-cloud-storage
      google-cloud-bigquery
      google-cloud-resource-manager
      shapely
      pydantic
      typing-extensions
      docstring-parser
      google-genai
    ];

    doCheck = false;
  };

  aider-chat-with-gcp = pkgs.unstable.aider-chat.overridePythonAttrs (oldAttrs: {
    propagatedBuildInputs = (oldAttrs.propagatedBuildInputs or [ ]) ++ [ google-cloud-aiplatform ];
  });
in
{
  home.packages = [
    aider-chat-with-gcp
  ];

  programs.git = {
    ignores = [
      ".aider*"
    ];
  };

  home.file = {
    ".aider.conf.yml" = {
      source = (pkgs.formats.yaml { }).generate "aider.conf.yml" {
        dark-mode = true;

        # This file will require importing home/llm-agent.nix
        read = [
          "${config.home.homeDirectory}/.local/var/AGENTS.md"
        ];

        # Disable Git unless explicitly enabled since Aider will nag
        # about creating Git otherwise (I'm using jj...)
        git = false;

        # Don't auto commit.
        auto-commits = false;
        dirty-commits = false;
      };
    };

    ".aider/oauth-keys.env" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.env";
    };
  };
}
