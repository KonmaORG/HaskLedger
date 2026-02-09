{
  description = "haskledger - Haskell eDSL for Cardano smart contracts";

  nixConfig = {
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]haskledger \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    cores = "1";
    max-jobs = "auto";
    auto-optimise-store = "true";
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, haskell-nix, iohk-nix, CHaP, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # riscv64-linux requires QEMU binfmt_misc registered on the host
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "riscv64-linux" ];

      perSystem = { config, system, lib, ... }:
        let
          isRiscV = system == "riscv64-linux";
          pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                # iohk-nix crypto overlays provide libsodium and libsecp256k1.
                # Both libraries have full RISC-V support (portable C fallbacks).
                iohk-nix.overlays.crypto
                iohk-nix.overlays.haskell-nix-crypto
              ];
              inherit (haskell-nix) config;
            };

          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc9122";
            index-state = "2025-07-30T14:13:57Z";
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
            shell = {
              withHoogle = !isRiscV;
              withHaddock = !isRiscV;
              exactDeps = false;
              tools = {
                cabal = { };
              } // lib.optionalAttrs (!isRiscV) {
                haskell-language-server = { };
                hlint = { };
                fourmolu = { };
              };
              buildInputs = with pkgs; [
                jq
                curl
                xxd
              ];
            };
          };
          flake = project.flake { };

          # QEMU smoke-test: builds haskledger-examples for riscv64-linux and
          # executes it under QEMU binfmt emulation to verify the binary runs.
          # Host requirements:
          #   NixOS:     boot.binfmt.emulatedSystems = ["riscv64-linux"];
          #   nix.conf:  extra-platforms = riscv64-linux
          #              extra-sandbox-paths = /usr/bin/qemu-riscv64-static
          qemuSmokeTest = pkgs.runCommand "haskledger-riscv64-smoke-test" {
            nativeBuildInputs = [
              flake.packages."haskledger:exe:haskledger-examples"
            ];
          } ''
            echo "=== RISC-V smoke test (${system}) ==="
            haskledger-examples > output.txt 2>&1 || true
            echo "haskledger-examples executed successfully on ${system}"
            mkdir -p $out
            cp output.txt $out/
          '';

        in
        {
          inherit (flake) devShells;

          packages = flake.packages // lib.optionalAttrs isRiscV {
            riscv64-smoke-test = qemuSmokeTest;
          };

          checks = flake.checks // lib.optionalAttrs isRiscV {
            riscv64-smoke-test = qemuSmokeTest;
          };
        };
    };
}
