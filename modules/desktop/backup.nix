{ config, pkgs, ... }:
{
  age.secrets.restic-password = {
    file = ../../secrets/restic-password.age;
    mode = "0400";
    owner = "joshua";
  };

  age.secrets.restic-repo = {
    file = ../../secrets/storagebox.age;
    mode = "0400";
    owner = "joshua";
  };

  services.restic.backups = {
    daily = {
      user = "joshua";

      paths = [
        "/home/joshua/.config"
        "/home/joshua/.authinfo.gpg"
        "/home/joshua/.gnupg"
        "/home/joshua/.password-store"
        "/home/joshua/.ssh"
        "/home/joshua/.mbsyncrc"
        "/home/joshua/Accounting"
        "/home/joshua/Catholic"
        "/home/joshua/Documents"
        "/home/joshua/Downloads"
        "/home/joshua/Library"
        "/home/joshua/Mail"
        "/home/joshua/Media"
        "/home/joshua/Pictures"
        "/home/joshua/Projects"
        "/home/joshua/Revere"
        "/home/joshua/nixos-config"
        "/home/joshua/org"
        "/home/joshua/secrets"
      ];

      repositoryFile = config.age.secrets.restic-repo.path;
      passwordFile = config.age.secrets.restic-password.path;

      initialize = true;

      extraOptions = [
        "sftp.command=\"ssh u408078@u408078.your-storagebox.de -p 23 -i /home/joshua/.ssh/id_ed25519 -o StrictHostKeyChecking=accept-new -s sftp\""
      ];

      timerConfig = {
        OnCalendar = "hourly";
        Persistent = true;
        RandomizedDelaySec = "15min";
      };

      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 4"
        "--keep-monthly 12"
        "--keep-yearly 7"
      ];

      checkOpts = [
        "--read-data-subset=10%"
      ];
    };
  };

  systemd.services.restic-backups-daily = {
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
  };
}
