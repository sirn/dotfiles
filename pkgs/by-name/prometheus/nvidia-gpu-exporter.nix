{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:

buildGoModule rec {
  pname = "nvidia-gpu-exporter";
  version = "1.4.1";

  src = fetchFromGitHub {
    owner = "utkuozdemir";
    repo = "nvidia_gpu_exporter";
    rev = "v${version}";
    hash = "sha256-+sXlQQUs8tmxtaqKUCBTfEZlL8fqBlhzcDFbX8Catsk=";
  };

  vendorHash = "sha256-A9CY14pdZLgm5eCWYlWmn3H7VPM4yxramv6pi2ER64I=";

  meta = with lib; {
    description = "Nvidia GPU exporter for prometheus using nvidia-smi binary";
    license = licenses.mit;
    homepage = "https://github.com/utkuozdemir/nvidia_gpu_exporter";
    maintainers = with maintainers; [ ];
  };
}
