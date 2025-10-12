{ config, lib, ... }:
{
  age.identityPaths = [
    "/etc/age/keys.txt"
  ];

  age.secrets = {
miniflux-admin = {
    file = ../secrets/miniflux-admin.age;
    owner = "root";
    group = "root";
    mode = "0400";  # Root readable only
    };
  };
}
