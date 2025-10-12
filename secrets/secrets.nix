let
  # Your personal SSH public key (from ~/.config/age/keys.txt
  joshua = "age1k0sc4ugaxzpav2rs8cmugwthaa3tpuzygvax8u84m6sm9ldh737qspv058";

  # Machine age keys
  empirica = "age1gt2m3dtrkx3lwnddwv62fesadyd5pkmadtwtdfwvcs4lhcyqt33qfq386s";

  # Groups for convenience
  users = [ joshua ];
  # desktops = [ king theologica ];
  servers = [ empirica ];
  allSystems = users ++ servers;
in
{
  "canlock.age".publicKeys = users;
  "gnus-name.age".publicKeys = users;
  "gnus-email.age".publicKeys = users;
  "miniflux-admin.age".publicKeys = servers;
}
