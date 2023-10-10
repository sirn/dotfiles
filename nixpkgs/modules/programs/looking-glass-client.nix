{
  programs.looking-glass-client = {
    enable = true;

    settings = {
      input = {
        escapeKey = 100;
        grabKeyboardOnFocus = true;
        captureOnly = true;
        autoCapture = true;
        rawMouse = true;
      };

      spice = {
        enable = true;
        audio = true;
        captureOnStart = true;
      };

      audio = {
        periodSize = 4096;
      };
    };
  };
}
