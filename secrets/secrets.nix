let
  # Your personal SSH public key (from ~/.ssh/joshuakey.pub)
  joshua = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCWNto66rFbOvb1VDEDuZYdwHQPfKM7+EjpnHvs3eRr joshua@joshuablais.com";

  # Groups for convenience
  users = [ joshua ];
  # desktops = [ king theologica ];
  # servers = [ alexandria empire ];
  # allSystems = desktops ++ servers;
in
{
  # Database secrets (only for servers)
  "postgres-password.age".publicKeys = users;
}
