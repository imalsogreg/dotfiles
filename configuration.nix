# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.trustedBinaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];


  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";


  boot.kernelPackages = pkgs.linuxPackages // {
    virtualbox = pkgs.linuxPackages.virtualbox.override {
      enableExtensionPack = true;
      pulseSupport = true;
    };
  };
  boot.extraModulePackages = [ pkgs.linuxPackages.lttng-modules];

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Change mac address from the predefined one to get off of Wilson Lab VPN
  networking.interfaces.enp7s0.macAddress = "00:1E:C9:65:6F:1C";
  #networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [80 443 24800];
  networking.extraHosts =
    ''
      18.93.13.1 tk1a
    '';

  security.sudo.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  nixpkgs.config.allowBroken = true;

  # Set your time zone.
  time.timeZone = "US/Eastern";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    gitAndTools.gitFull
    synergy
    arandr
    blender
    wget
    inkscape
    gimp
    gnupg
    traceroute
    xclip
    xchat
    curl
    vim
    git
    haskellPackages.cabal2nix
    dmenu
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.xmonad-wallpaper
    zsh
    zlib
  ];

  # List services that you want to enable:

  # Virtualbox && Docker

  virtualisation.libvirtd.enable = true;
  virtualisation.lxc.enable = true;
  virtualisation.lxc.usernetConfig = ''
    bfo veth lxcbr0 10
  '';
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "overlay";
  
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableHardening = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable postgresql
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql93;
  services.postgresql.authentication = "local all all ident";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbVariant = "dvp";
  # services.xserver.xkbOptions = "eurosign:e";

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
  
    displayManager.lightdm.enable = true;

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.xfce.enable = true;
    desktopManager.default = "xfce";
  };
  
  # services.synergy.server = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.greghale = {
    isNormalUser = true;
    uid = 1000;
    description = "Greg Hale";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  users.extraGroups.vboxusers.members = [ "greghale" ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

}
