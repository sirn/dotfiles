function FindProxyForURL(url, host) {

    var proxy = "PROXY proxy.trueinternet.co.th:8080";

    // True Internet cannot access NearlyFreeSpeech for some reason
    if (isInNet(host, "208.94.116.0", "255.255.254.0")) { return proxy; }

    // The same goes to Wikipedia
    if (isInNet(host, "208.80.152.2", "255.255.252.0")) { return proxy; }

    // Default to direct connection
    return "DIRECT";

}