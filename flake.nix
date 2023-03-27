{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
  {
    nixosConfigurations.leonardo = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules =  [
       (import ./configuration.nix { hostName = "leonardo"; hardware = ./machines/leonardo-hardware-configuration.nix; })
      ];
    };

    nixosConfigurations.michelangelo = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules =  [
       (import ./configuration.nix { hostName = "michelangelo"; hardware = ./machines/michelangelo-hardware-configuration.nix; })
      ];
    };

  };
}
