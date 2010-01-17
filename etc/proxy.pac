function FindProxyForURL(url, host) {

    var direct = "PROXY proxy.trueinternet.co.th:8080; DIRECT"; // Meh.
    var proxy = "PROXY proxy.trueinternet.co.th:8080";

    // Always bypass proxy config for localnet
    if (isInNet(host, "127.0.0.1", "255.0.0.0")) { return "DIRECT"; }
    if (isInNet(host, "192.168.0.0", "255.255.0.0")) { return "DIRECT"; }

    // Sites not accessible from True Internet
    // - NearlyFreeSpeech (208.94.116.0-208.94.117.255)
    // - Wikipedia (208.80.152.0-208.80.155.255)
    if (isInNet(host, "208.94.116.0", "255.255.254.0")) { return proxy; }
    if (isInNet(host, "208.80.152.2", "255.255.252.0")) { return proxy; }

    // Default to direct connection
    return direct;

}
