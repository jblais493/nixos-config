let
  # Your personal SSH public key (from ~/.ssh/joshuakey.pub)
  joshua = "age1k0sc4ugaxzpav2rs8cmugwthaa3tpuzygvax8u84m6sm9ldh737qspv058";

  # Machine age keys
  # king = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBb root@king";
  # theologica = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc root@theologica";
  # alexandria = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDd root@alexandria";
  # empire = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEe root@empire";


  # Groups for convenience
  users = [ joshua ];
  # desktops = [ king theologica ];
  # servers = [ alexandria empire ];
  # allSystems = desktops ++ servers;
in
{
  # Database secrets (only for servers)
  "postgres-password.age".publicKeys = users;
  "canlock.age".publicKeys = users;
}
