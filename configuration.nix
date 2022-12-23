# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ hostName, hardware }:
{ config, pkgs, ... }:

{
  imports =
    [ hardware ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # TODO: reenable this:
  # boot.binfmt.emulatedSystems = [ "wasm32-wasi" "aarch64-linux" ];

  networking.hostName = hostName; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";
 
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts inconsolata ubuntu_font_family unifont source-code-pro
    ];
  };

  # Configure keymap in X11
  services.xserver = {

    enable = true;
    autorun = true;
    layout = "us";
    xkbVariant = "dvorak";
    xkbOptions = "ctrl:nocaps";

    # synaptics.enable = true;
    # displayManager.defaultSession = "none+xmonad";
    # displayManager.sddm.enable = true;
    # windowManager.xmonad = {
    #   enable = true;
    #   enableContribAndExtras = true;
    #   extraPackages = hp: [ hp.xmobar hp.yeganesh hp.xmonad-contrib hp.xmonad-extras ];
    # };

    displayManager.gdm.enable = true;
    displayManager.gdm.wayland = false;
    desktopManager.gnome3.enable = true;
  };

  # services.dbus.packages = [ pkgs.gnome3.dconf ];
  services.udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];

  # Configure console keymap
  console.keyMap = "dvorak";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.greghale = {
    isNormalUser = true;
    description = "Greg Hale";
    extraGroups = [ "networkmanager" "wheel" "docker" "adbusers" ];
    packages = with pkgs; [];
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = "greghale";

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = true; }; })
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
   wget
   ripgrep
   termite
   xmobar
   dmenu

   firefox
   slack
   brave
   discord

   direnv
   nix-direnv

   gnomeExtensions.system-monitor-2
  ];

  nix.binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" "https://cache.iog.io" ];
  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
  nix.trustedUsers = [ "greghale" ];
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = true
    keep-derivations = true
    '';
  
  

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.adb.enable = true;
  programs.bash.enableCompletion = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.keybase.enable = true;
  services.blueman.enable = true;
  services.kbfs.enable = true;
  services.lorri.enable = true;
  services.tailscale.enable = true;
  services.mysql.enable = true;
  services.mysql.package = pkgs.mysql;

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraConfig = ''
      load-module module-switch-on-connect
      '';
  };
  hardware.bluetooth.enable = true;
  hardware.opengl.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "greghale" ];

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 80 443 1790 3000 8000 8080 24800 26275 ];
  networking.firewall.checkReversePath = "loose"; # Needed by tailscale;
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.extraHosts =
    ''
    127.0.0.1 local.example.com
    '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
