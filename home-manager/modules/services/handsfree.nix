{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types mkEnableOption;
  cfg = config.services.handsfree;

  tomlFormat = pkgs.formats.toml { };

  # Field names mirror handsfree's own config/default.toml (snake_case).
  settings = {
    transcription = {
      backend = cfg.transcription.backend;
    }
    // lib.optionalAttrs (cfg.transcription.backend == "local") {
      local = {
        model = cfg.transcription.local.model;
        inherit (cfg.transcription.local) translate;
      }
      // lib.optionalAttrs (cfg.transcription.local.language != null) {
        inherit (cfg.transcription.local) language;
      };
    }
    // lib.optionalAttrs (cfg.transcription.backend == "openai") {
      openai = {
        inherit (cfg.transcription.openai) api_key_env;
        model = cfg.transcription.openai.model;
      }
      // lib.optionalAttrs (cfg.transcription.openai.base_url != null) {
        inherit (cfg.transcription.openai) base_url;
      }
      // lib.optionalAttrs (cfg.transcription.openai.language != null) {
        inherit (cfg.transcription.openai) language;
      };
    };

    audio = {
      inherit (cfg.audio) device;
      sample_rate = cfg.audio.sampleRate;
    };

    hotkey = { inherit (cfg.hotkey) key modifiers; };

    insertion = {
      inherit (cfg.insertion) method;
      restore_clipboard = cfg.insertion.restoreClipboard;
    };

    post_processing = {
      inherit (cfg.postProcessing) deterministic;
      llm = {
        inherit (cfg.postProcessing.llm) backend enabled;
      }
      // lib.optionalAttrs (cfg.postProcessing.llm.promptFile != null) {
        prompt_file = cfg.postProcessing.llm.promptFile;
      }
      // lib.optionalAttrs (cfg.postProcessing.llm.backend == "openai") {
        openai = {
          inherit (cfg.postProcessing.llm.openai) api_key_env;
        }
        // lib.optionalAttrs (cfg.postProcessing.llm.openai.base_url != null) {
          inherit (cfg.postProcessing.llm.openai) base_url;
        }
        // lib.optionalAttrs (cfg.postProcessing.llm.openai.model != null) {
          inherit (cfg.postProcessing.llm.openai) model;
        };
      }
      //
        lib.optionalAttrs
          (cfg.postProcessing.llm.backend == "auto" || cfg.postProcessing.llm.backend == "local")
          {
            local = {
              inherit (cfg.postProcessing.llm.local)
                model
                ngl
                n_ctx
                n_threads
                max_tokens
                ;
            };
          };
    };

    overlay = { inherit (cfg.overlay) enabled; };
  };

  # A launchd agent does not inherit the login shell's environment, and the
  # plist has no EnvironmentFile directive so a wrapper is needed.
  appBinary = "${cfg.package}/Applications/Handsfree.app/Contents/MacOS/handsfree";
  appLauncher = "${cfg.package}/Applications/Handsfree.app/Contents/MacOS/handsfree-launcher";

  linuxExe = lib.getExe cfg.package;
in
{
  options.services.handsfree = {
    enable = mkEnableOption "handsfree" // {
      description = "Enable handsfree, a local push-to-talk speech-to-text dictation app for macOS and Wayland.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.local.handsfree;
      defaultText = "pkgs.local.handsfree";
      description = "The handsfree package to use.";
    };

    environmentFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "/path/to/env";
      description = "Path to a shell env file sourced by the service to inject into handsfree's env.";
    };

    transcription = {
      backend = mkOption {
        type = types.enum [
          "local"
          "openai"
          "apple"
        ];
        default = "local";
        description = ''
          Transcription backend. `local` uses whisper-rs (offline);
          `openai` posts to the OpenAI Whisper API; `apple` uses on-device
          SFSpeechRecognizer (macOS only).
        '';
      };

      local = {
        model = mkOption {
          type = types.str;
          default = "medium";
          description = ''
            Local Whisper model name from `handsfree models list`, auto-downloaded
            to ~/.cache/handsfree/models/ on first use.
          '';
        };

        translate = mkOption {
          type = types.bool;
          default = false;
          description = "Whether local Whisper translates captured speech to English.";
        };

        language = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Pinned language code (e.g. "en"). null auto-detects per clip.
          '';
        };
      };

      openai = {
        api_key_env = mkOption {
          type = types.str;
          default = "OPENAI_API_KEY";
          description = ''
            Name of the environment variable holding the OpenAI API key.
            Never put the key itself in the config.
          '';
        };

        model = mkOption {
          type = types.str;
          default = "whisper-1";
          description = "OpenAI transcription model.";
        };

        base_url = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Optional OpenAI-compatible base URL. null defaults to
            https://api.openai.com/v1.
          '';
        };

        language = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Optional pinned language code for the OpenAI backend.";
        };
      };
    };

    audio = {
      device = mkOption {
        type = types.str;
        default = "";
        description = ''
          Audio input device name. Empty string uses the system default input.
        '';
      };

      sampleRate = mkOption {
        type = types.ints.positive;
        default = 16000;
        description = "Capture sample rate in Hz.";
      };
    };

    hotkey = {
      key = mkOption {
        type = types.str;
        default = "k";
        description = ''
          Push-to-talk key. Held together with the modifiers to capture audio.
        '';
      };

      modifiers = mkOption {
        type = types.listOf (
          types.enum [
            "ctrl"
            "shift"
            "alt"
            "meta"
          ]
        );
        default = [
          "ctrl"
          "shift"
        ];
        description = "Modifier keys held together with the push-to-talk key.";
      };
    };

    insertion = {
      method = mkOption {
        type = types.enum [
          "clipboard"
          "keystroke"
        ];
        default = "clipboard";
        description = ''
          How transcribed text is delivered to the focused app. `clipboard` sets
          the clipboard and synthesizes the paste key; `keystroke` types via
          Unicode key events (never touches the clipboard).
        '';
      };

      restoreClipboard = mkOption {
        type = types.bool;
        default = true;
        description = ''
          With `clipboard` insertion, save and restore the previous clipboard
          text after pasting (best-effort, text only).
        '';
      };
    };

    postProcessing = {
      deterministic = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to run the deterministic text-cleanup pass (whitespace,
          capitalization, terminal punctuation).
        '';
      };

      llm = {
        backend = mkOption {
          type = types.enum [
            "auto"
            "apple"
            "local"
            "openai"
          ];
          default = "auto";
          description = ''
            LLM text-cleanup backend. `auto` picks Apple FoundationModels on
            macOS and in-process llama.cpp on Linux, falling back to
            deterministic cleanup when unavailable.
          '';
        };

        enabled = mkOption {
          type = types.bool;
          default = true;
          description = "Whether to run the optional LLM cleanup pass.";
        };

        promptFile = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Path to a file whose contents override the built-in cleanup prompt.
          '';
        };

        openai = {
          api_key_env = mkOption {
            type = types.str;
            default = "OPENAI_API_KEY";
            description = ''
              Name of the environment variable holding the API key for the
              OpenAI-compatible cleanup backend.
            '';
          };

          base_url = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Optional OpenAI-compatible base URL for cleanup.
              null defaults to https://api.openai.com/v1.
            '';
          };

          model = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              Model for the OpenAI-compatible cleanup backend.
              null defaults to gpt-4o-mini.
            '';
          };
        };

        local = {
          model = mkOption {
            type = types.str;
            default = "gemma-3-4b-it-q4_k_m";
            description = ''
              Local llama.cpp (GGUF) cleanup model name from
              `handsfree models list`, auto-downloaded to
              ~/.cache/handsfree/models/ on first use.
            '';
          };

          ngl = mkOption {
            type = types.ints.unsigned;
            default = 99;
            description = ''
              GPU layers to offload to Vulkan for the local cleanup model; 0 = CPU.
            '';
          };

          n_ctx = mkOption {
            type = types.ints.positive;
            default = 4096;
            description = ''
              Context tokens for the local cleanup model
              (must exceed prompt + max_tokens).
            '';
          };

          n_threads = mkOption {
            type = types.ints.positive;
            default = 4;
            description = "CPU threads for the local cleanup model.";
          };

          max_tokens = mkOption {
            type = types.ints.positive;
            default = 512;
            description = ''
              Maximum generated tokens per local cleanup pass.
            '';
          };
        };
      };
    };

    overlay = {
      enabled = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether the on-screen status pill (recording / thinking / preparing)
          is shown. Disable on headless setups or compositors without the
          wlr-layer-shell support.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."handsfree/config.toml" = {
      source = tomlFormat.generate "handsfree-config" settings;
    };

    systemd.user.services.handsfree = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "handsfree - push-to-talk speech-to-text dictation";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };

      Service = {
        EnvironmentFile = lib.optional (cfg.environmentFile != null) cfg.environmentFile;
        ExecStart = linuxExe;
        Restart = "on-failure";
        RestartSec = 5;
        Slice = "app.slice";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    launchd.agents.handsfree = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;
      config = {
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        ProgramArguments =
          if cfg.environmentFile == null then
            [ appBinary ]
          else
            [
              appLauncher
              cfg.environmentFile
            ];
        StandardOutPath = "/tmp/handsfree.log";
        StandardErrorPath = "/tmp/handsfree.log";
      };
    };
  };
}
