{
  description = "Joshua Blais' NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.05";
    nur.url = "github:nix-community/NUR";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, deploy-rs, agenix, disko, ... }@inputs: {
    nixosConfigurations = {
      # Laptop hosts
      theologica = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/theologica/configuration.nix
          agenix.nixosModules.default

          # Add home-manager
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.joshua = import ./modules/home-manager;
            home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };

      king = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/king/configuration.nix
          agenix.nixosModules.default

          # # Add home-manager
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.joshua = import ./modules/home-manager;
            home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };

      axios = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/axios/configuration.nix
          ./hosts/axios/disk-configuration.nix
          disko.nixosModules.disko
          agenix.nixosModules.default

          # # Add home-manager
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.joshua = import ./modules/home-manager;
            home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };

      # Server hosts (no home-manager needed)
      alexandria = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/alexandria/configuration.nix
          agenix.nixosModules.default
        ];
      };

      empire = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/empire/configuration.nix
          agenix.nixosModules.default
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
