{
  agents.models = {
    default = {
      provider = "synthetic";
      model = "hf:moonshotai/Kimi-K2.5";
    };

    providers = {
      synthetic = {
        name = "Synthetic";
        baseUrl = "https://api.synthetic.new/openai/v1";
        envVar = "SYNTHETIC_API_KEY";
        api = "openai-completions";
        reasoningEffort = "medium";
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
            reasoningEffort = "medium";
          }
        ];
      };
    };
  };
}
