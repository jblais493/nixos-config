{
  description = "Joshua Blais' NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, deploy-rs, ... }@inputs: {
    nixosConfigurations = {
      # Laptop hosts
      theologica = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/theologica/configuration.nix
          ./modules/shared
          ./modules/desktop
          ./modules/cli-tui
          ./modules/development
          ./modules/media
        ];
      };

      king = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/king/configuration.nix
          ./modules/shared
          ./modules/desktop
          ./modules/cli-tui
          ./modules/development
        ];
      };

      # Server hosts
      alexandria = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/alexandria/configuration.nix
          ./modules/shared
          ./modules/server
          ./modules/services
          ./modules/security
        ];
      };

      empire = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/empire/configuration.nix
          ./modules/shared
          ./modules/server
          ./modules/security
        ];
      };
    };

    # Deploy-rs configuration for remote deployment
    deploy.nodes = {
      alexandria = {
        hostname = "alexandria.your-domain.com";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.alexandria;
        };
      };
    };
  };
}
