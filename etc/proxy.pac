function FindProxyForURL(url, host) {
    return "PROXY localhost:8888; PROXY 192.168.1.129:8888; DIRECT"
}