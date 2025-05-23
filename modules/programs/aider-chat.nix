{ lib, pkgs, ... }:

{
  home.packages = [
    pkgs.unstable.aider-chat-full
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
          name = "anthropic/claude-sonnet-4-20250514";
          edit_format = "diff";
          weak-model-name = "anthropic/claude-3-5-haiku-20241022";
          use_repo_map = true;
          overeager = true;
        }
        {
          name = "openrouter/google/gemini-2.5-pro-preview";
          edit_format = "diff-fenced";
          weak_model_name = "openrouter/google/gpt-4o-mini";
          use_repo_map = true;
          overeager = true;
        }
        {
          name = "openrouter/anthropic/claude-sonnet-4";
          edit_format = "diff";
          weak_model_name = "openrouter/anthropic/claude-3-5-haiku";
          use_repo_map = true;
          overeager = true;
        }
      ];
    };
  };
}
