function FindProxyForURL(url, host) {
    if (shExpMatch(url, "*twitter.com/*")) { return "DIRECT"; }
    if (shExpMatch(url, "*.twitter.com/*")) { return "DIRECT"; }
    return "PROXY localhost:8888; PROXY 192.168.1.129:8888; DIRECT"
}