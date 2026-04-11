{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentsCfg = config.agents;

  agentsDir = ../../../../var/agents/agents;

  # Map API types to npm package for the SDK
  apiToNpm = api: {
    "anthropic-messages" = "@ai-sdk/anthropic";
    "openai-completions" = "@ai-sdk/openai-compatible";
    "openai-responses" = "@ai-sdk/openai";
    "google-generative-ai" = "@ai-sdk/google";
  }.${api} or "@ai-sdk/openai-compatible";

  # Map API types to provider name suffix
  apiToSuffix = api: {
    "anthropic-messages" = "messages";
    "openai-completions" = "completions";
    "openai-responses" = "responses";
    "google-generative-ai" = "generative-ai";
  }.${api} or api;

  # Resolve model's effective API and baseURL for OpenCode
  # Priority: model.opengode > model > provider.opengode > provider
  getModelApiAndUrl = p: defaultApi: m:
    let
      api = if m.api != null then m.api else defaultApi;
      url =
        if m.opencode != null && m.opencode.baseUrl != null then m.opencode.baseUrl
        else if m.baseUrl != null then m.baseUrl
        else if p.opencode != null && p.opencode.baseUrl != null then p.opencode.baseUrl
        else p.baseUrl;
    in
    { inherit api url; };

  # Transform module model to OpenCode format (without baseURL)
  toOpenCodeModel = m: {
    id = m.id;
    name = m.name;
    family = m.family;
    attachment = m.attachment;
    reasoning = m.reasoning;
    tool_call = m.toolCall;
    temperature = m.temperature;
    modalities = {
      input = m.input;
      output = [ "text" ];
    };
    limit = {
      context = m.contextWindow;
      output = m.maxTokens;
    };
    options = {
      reasoningEffort = m.reasoningEffort;
    };
  };

  # Group models by (api, url) combination
  groupModelsByApiAndUrl = p:
    let
      defaultApi = if p.api != null then p.api else "openai-completions";
      withApiAndUrl = map (m: getModelApiAndUrl p defaultApi m // { model = m; }) p.models;
    in
    lib.groupBy (item: "${item.api}::${item.url}") withApiAndUrl;

  # Generate provider key suffix for uniqueness (URL hash when different from base)
  getUrlSuffix = url: baseUrl:
    if url != baseUrl then "-${lib.substring 0 4 (builtins.hashString "md5" url)}" else "";

  # Generate display name with API type suffix for non-default APIs
  getProviderDisplayName = name: api: defaultApi:
    let
      suffixes = {
        "anthropic-messages" = "(Messages)";
        "openai-completions" = "(Completions)";
        "openai-responses" = "(Responses)";
        "google-generative-ai" = "(Generative AI)";
      };
      suffix = suffixes.${api} or "(${api})";
    in
    if api == defaultApi then name else "${name} ${suffix}";

  # Build OpenCode providers from agents.models provider
  # Uses npm field to specify which SDK, allows custom provider names
  mkOpenCodeProviders = providerId: p:
    let
      groups = groupModelsByApiAndUrl p;
      defaultApi = if p.api != null then p.api else "openai-completions";
    in
    lib.mapAttrs' (key: items:
      let
        first = lib.head items;
        api = first.api;
        url = first.url;
        apiSuffix = apiToSuffix api;
        urlSuffix = getUrlSuffix url p.baseUrl;
        # Provider name: {providerId}-{apiSuffix}{-urlHash if different}
        providerName = "${providerId}-${apiSuffix}${urlSuffix}";
        displayName = getProviderDisplayName p.name api defaultApi;
        npm = apiToNpm api;
      in
      lib.nameValuePair providerName {
        npm = npm;
        name = displayName;
        options = {
          baseURL = url;
          apiKey = "{env:${p.envVar}}";
        };
        models = builtins.listToAttrs (map (item: lib.nameValuePair item.model.id (toOpenCodeModel item.model)) items);
      }
    ) groups;

  # Flatten all providers from all agent providers
  allOpenCodeProviders = lib.foldl' (acc: name:
    acc // (mkOpenCodeProviders name config.agents.models.providers.${name})
  ) { } (lib.attrNames config.agents.models.providers);

  policy = agentsCfg.permissions.effective.build;

  toOpencodePermissions =
    let
      inherit (policy) tools commands paths;

      mkPathRules =
        section:
        lib.listToAttrs (map (p: lib.nameValuePair p "allow") (paths.allow.${section} or [ ]))
        // lib.listToAttrs (map (p: lib.nameValuePair p "deny") (paths.deny.${section} or [ ]))
        // {
          "*" = "allow";
        };

      mkBashRules =
        let
          mkEntries =
            decision: cmds:
            lib.concatMap (
              entry:
              let
                m = entry.match;
              in
              {
                exact = [ (lib.nameValuePair m decision) ];
                prefix = [
                  (lib.nameValuePair "${m} *" decision)
                  (lib.nameValuePair m decision)
                ];
                substring = [
                  (lib.nameValuePair "* ${m} *" decision)
                  (lib.nameValuePair "${m} *" decision)
                  (lib.nameValuePair "* ${m}" decision)
                ];
              }
              .${entry.mode or "prefix"}
            ) cmds;
          allows = mkEntries "allow" (commands.allow or [ ]);
          asks = mkEntries "ask" (commands.ask or [ ]);
          denies = mkEntries "deny" (commands.deny or [ ]);
        in
        lib.listToAttrs (allows ++ asks ++ denies) // { "*" = "ask"; };
    in
    {
      read = mkPathRules "read";
      glob = "allow";
      grep = "allow";
      list = "allow";
      bash = mkBashRules;
      edit = if tools.edit then (mkPathRules "edit") else (mkPathRules "edit" // { "*" = "deny"; });
      webfetch = "allow";
      websearch = "allow";
    };

  mkOpencodeAgent =
    name: agent:
    let
      oc = agent.opencode;
      isPrimary = (oc.primary or false);
      modelLine = lib.optionalString (isPrimary && oc.model != "") "model: ${oc.model}\n";
      modeVal = if !isPrimary then "subagent" else (oc.mode or "primary");
      modeLine = lib.optionalString (modeVal != "") "mode: ${modeVal}\n";
      permissionLine = lib.optionalString (
        oc.permission != null
      ) "permission: ${builtins.toJSON oc.permission}\n";
    in
    ''
      ---
      description: ${agent.description}
      ${modelLine}${modeLine}${permissionLine}---
      ${agent.prompt}
    '';

  opencodeAgents = lib.filterAttrs (name: agent: agent.opencode != null) agentsCfg.subagents;

  validateOpencodeAgent =
    name: agent: if !(agent ? description) then throw "Agent ${name}: missing 'description'" else agent;

  validOpencodeAgents = lib.mapAttrs validateOpencodeAgent opencodeAgents;

  isStdioServer = server: server ? command || server ? package;

  toOpencodeMcpServers =
    servers:
    lib.mapAttrs (
      name: server:
      if isStdioServer server then
        {
          command = [ (server.command or (lib.getExe server.package)) ];
          type = "local";
          enabled = true;
        }
      else
        {
          url = server.url;
          type = "remote";
          enabled = true;
        }
    ) servers;

  # Generate MCP tool permissions from server allowedTools
  opencodeMcpPermissions = lib.listToAttrs (
    lib.flatten (
      lib.mapAttrsToList (
        name: server:
        let
          tools = server.allowedTools or null;
        in
        if tools == null then
          [
            {
              name = "${name}_*";
              value = true;
            }
          ]
        else
          # Deny all tools by default, allow specific ones
          [
            {
              name = "${name}_*";
              value = false;
            }
          ]
          ++ (map (tool: {
            name = "${name}_${tool}";
            value = true;
          }) tools)
      ) config.programs.mcp.servers
    )
  );
in
{
  programs.opencode = {
    enable = true;

    package = (
      pkgs.writeScriptBin "opencode" ''
        #!${pkgs.runtimeShell}
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env.local" \
          -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
          -- "${lib.getExe pkgs.unstable.opencode}" "$@"
      ''
    );

    agents = lib.mapAttrs mkOpencodeAgent validOpencodeAgents;
    rules = agentsCfg.instructionText + ''

      ## Skill Execution (Subagent Enhancement)

      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents for higher-quality results.
    '';

    settings = {
      theme = "system";
      mcp = toOpencodeMcpServers config.programs.mcp.servers;
      mode = {
        plan.model = "${agentsCfg.models.default.provider}/${agentsCfg.models.default.model}";
        build.model = "${agentsCfg.models.default.provider}/${agentsCfg.models.default.model}";
      };
      permission = toOpencodePermissions;
      tools = opencodeMcpPermissions;
      provider = allOpenCodeProviders;
    };
  };

  xdg.configFile."opencode/skill/home-manager".source = agentsCfg.skillsDir;
}
