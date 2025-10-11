{ config, lib, ... }:
{
  age.identityPaths = [ "/home/joshua/.config/age/keys.txt" ];

  age.secrets = {
    canlock = {
      file = ../secrets/canlock.age;
      owner = "joshua";
      mode = "400";
    };
    gnus-name = {
      file = ../secrets/gnus-name.age;
      owner = "joshua";
      mode = "400";
    };
    gnus-email = {
      file = ../secrets/gnus-email.age;
      owner = "joshua";
      mode = "400";
    };
  };
}
