{ lib, pkgs, ... }:

{
  home.packages = [
    pkgs.unstable.aider-chat
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
        git = false;
        auto-commits = false;
        dirty-commits = false;
      };
    };

    ".aider.model.settings.yml" = {
      source = (pkgs.formats.yaml { }).generate "aider.model.settings.yml" [
        {
          name = "openrouter/google/gemini-2.5-pro-preview";
          edit_format = "diff-fenced";
          weak_model_name = "openrouter/openai/gpt-4o-mini";
          use_repo_map = true;
          overeager = true;
        }
      ];
    };
  };
}
