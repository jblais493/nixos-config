{ config, lib, ... }:
{
  age.identityPaths = [
    "/etc/age/keys.txt"
  ];

  age.secrets = {
    miniflux-admin = {
      file = ../../secrets/miniflux-admin.age;
      owner = "miniflux";
      group = "miniflux";
      mode = "0600";
    };
  };
}
