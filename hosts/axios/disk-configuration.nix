{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            # Boot partition
            ESP = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [
                  "defaults"
                  "umask=0077"
                ];
              };
            };

            # Main encrypted partition
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted";
                # You'll be prompted for password during install
                content = {
                  type = "btrfs";
                  extraArgs = [ "-f" ];
                  subvolumes = {
                    # Root subvolume
                    "/@" = {
                      mountpoint = "/";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };

                    # Home subvolume
                    "/@home" = {
                      mountpoint = "/home";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };

                    # Nix store subvolume
                    "/@nix" = {
                      mountpoint = "/nix";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };

                    # Persistent data subvolume
                    "/@persist" = {
                      mountpoint = "/persist";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };

                    # Snapshots subvolume
                    "/@snapshots" = {
                      mountpoint = "/snapshots";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };

                    # Swap subvolume (btrfs swap file)
                    "/@swap" = {
                      mountpoint = "/swap";
                      mountOptions = [ "noatime" ];
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
