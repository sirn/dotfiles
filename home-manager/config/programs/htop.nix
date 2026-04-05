{ config, ... }:

{
  programs.htop = {
    enable = true;

    # https://github.com/htop-dev/htop/blob/main/Settings.c
    settings = {
      delay = 30; # 0.3s
      hide_kernel_threads = 1;
      hide_threads = 0;
      highlight_base_name = 1;
      highlight_megabytes = 1;
      highlight_threads = 1;
      shadow_distribution_path_prefix = 1;
      shadow_other_users = 1;
      sort_direction = 0;
      sort_key = config.lib.htop.fields.PERCENT_CPU;
      tree_sort_direction = 0;
      tree_sort_key = config.lib.htop.fields.PERCENT_CPU;
      tree_view = 0;
    };
  };
}
