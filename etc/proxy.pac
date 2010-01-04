function FindProxyForURL(url, host) {

  // True Internet cannot access NearlyFreeSpeech for some reason
  if (isInNet(host, "208.94.116.0", "255.255.254.0")) {
    return "PROXY proxy.trueinternet.co.th:8080";
  }

  // Default to direct connection
  return "DIRECT";

}
