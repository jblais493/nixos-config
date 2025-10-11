{ config, lib, ... }:
{
  age.identityPaths = [ "/home/joshua/.config/age/keys.txt" ];

  age.secrets.canlock = {
    file = ../secrets/canlock.age;
    owner = "joshua";
    mode = "400";
  };
}
