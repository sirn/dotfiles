{
  agents.models = {
    default = {
      provider = "fireworks-ai";
      model = "accounts/fireworks/routers/kimi-k2p5-turbo";
    };

    providers = {
      synthetic = {
        name = "Synthetic";
        baseUrl = "https://api.synthetic.new/openai/v1";
        envVar = "SYNTHETIC_API_KEY";
        api = "openai-completions";
        reasoningEffort = "high";
        models = [
          {
            id = "hf:moonshotai/Kimi-K2.5";
            name = "Kimi K2.5 (Synthetic)";
            family = "kimi";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "hf:zai-org/GLM-4.7";
            name = "GLM 4.7 (Synthetic)";
            family = "glm";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "hf:MiniMaxAI/MiniMax-M2.5";
            name = "MiniMax M2.5 (Synthetic)";
            family = "minimax";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1000000;
            maxTokens = 32768;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
        ];
      };

      fireworks-ai = {
        name = "Fireworks AI";
        baseUrl = "https://api.fireworks.ai/inference/v1";
        envVar = "FIREWORKS_API_KEY";
        api = "openai-completions";
        reasoningEffort = "high";
        models = [
          {
            id = "accounts/fireworks/models/kimi-k2p5";
            name = "Kimi K2.5 (Fireworks)";
            family = "kimi";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            costInput = 0.6;
            costOutput = 3.0;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "accounts/fireworks/routers/kimi-k2p5-turbo";
            name = "Kimi K2.5 Turbo (Developer Pass)";
            family = "kimi";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 256000;
            maxTokens = 256000;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "accounts/fireworks/models/glm-5";
            name = "GLM 5 (Fireworks)";
            family = "glm";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            costInput = 1.0;
            costOutput = 3.2;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
        ];
      };

      anthropic = {
        name = "Anthropic";
        baseUrl = "https://api.anthropic.com";
        envVar = "ANTHROPIC_API_KEY";
        api = "anthropic-messages";
        reasoningEffort = "high";
        models = [
          {
            id = "claude-opus-4-6";
            name = "Claude Opus 4.6";
            family = "claude";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 200000;
            maxTokens = 128000;
            costInput = 5.0;
            costOutput = 25.0;
            costCacheRead = 0.5;
            costCacheWrite = 6.25;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "claude-sonnet-4-6";
            name = "Claude Sonnet 4.6";
            family = "claude";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 200000;
            maxTokens = 64000;
            costInput = 3.0;
            costOutput = 15.0;
            costCacheRead = 0.3;
            costCacheWrite = 3.75;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
        ];
      };

      openai = {
        name = "OpenAI";
        baseUrl = "https://api.openai.com/v1";
        envVar = "OPENAI_API_KEY";
        api = "openai-responses";
        reasoningEffort = "high";
        models = [
          {
            id = "gpt-5.4";
            name = "GPT-5.4";
            family = "openai";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1050000;
            maxTokens = 128000;
            costInput = 2.5;
            costOutput = 15.0;
            costCacheRead = 0.25;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "gpt-5.4-mini";
            name = "GPT-5.4 Mini";
            family = "openai";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 400000;
            maxTokens = 128000;
            costInput = 0.75;
            costOutput = 4.5;
            costCacheRead = 0.075;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "gpt-5.4-nano";
            name = "GPT-5.4 Nano";
            family = "openai";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 400000;
            maxTokens = 128000;
            costInput = 0.2;
            costOutput = 1.25;
            costCacheRead = 0.02;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
        ];
      };

      google = {
        name = "Google";
        baseUrl = "https://generativelanguage.googleapis.com/v1beta";
        envVar = "GEMINI_API_KEY";
        api = "google-generative-ai";
        reasoningEffort = "high";
        models = [
          {
            id = "gemini-3.1-pro-preview";
            name = "Gemini 3.1 Pro Preview";
            family = "gemini";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1048576;
            maxTokens = 65536;
            costInput = 2.0;
            costOutput = 12.0;
            costCacheRead = 0.2;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
          {
            id = "gemini-3.1-flash-lite-preview";
            name = "Gemini 3.1 Flash Lite Preview";
            family = "gemini";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1048576;
            maxTokens = 65536;
            costInput = 0.5;
            costOutput = 3.0;
            costCacheRead = 0.05;
            attachment = false;
            toolCall = true;
            temperature = true;
            reasoningEffort = "high";
          }
        ];
      };
    };
  };
}
