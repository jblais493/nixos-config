;;; ../../dotfiles/doom/.config/doom/lisp/nm.el -*- lexical-binding: t; -*-

(provide 'nm)

(defvar iface nil
  "Primary WiFi network interface.")

(defvar nmvar/vpn-profile nil
  "VPN profile.")

(defun nm/cmd (args &optional split)
  "Calls the networkmanager cli interface with the provided
arguments and splits it if 'split' is non-nil."
  (let* ((cmd (concat "nmcli " args))
         (output (shell-command-to-string cmd)))
    (if split
        (split-string output "\n" t " +")
      output)))

(defun nm/wifi-enabled-p ()
  "True if wifi is enabled according to NetworkManager."
  (let ((status (nth 1 (nm/cmd "-f wifi general" t))))
    (equal status "enabled")))

(defun nm/find-profiles-by-type (type &optional active)
  "Find profiles by type.

TYPE:
  Select the type of profile type you wanna have:
  - vpn, VPN profiles
  - 802-11-wireless, WiFi profiles
  - bridge, bridge profiles
  - tun, tunnel profiles

ACTIVE:
  If active set to 't' only active profiles will be evaluated.

Example:
  Show all WiFi profiles:
    (nm/find-profiles-by-type \"802-11-wireless\")
  Show only active WiFi profiles:
    (nm/find-profiles-by-type \"802-11-wireless\" t)
  Show active VPN profiles:
    (nm/find-profiles-by-type \"vpn\" t)"
  (let* ((two-dim-list
          (mapcar (lambda (x) (split-string x ":"))
                  (split-string
                   (shell-command-to-string
                    (if active
                        (format "nmcli -t -f TYPE,NAME connection show --active")
                      (format "nmcli -t -f TYPE,NAME connection show")))))))
    (mapcar (lambda (x) (nth 1 x))
            (seq-filter (lambda (x) (and x (equal (nth 0 x) type)))
                        two-dim-list))))

(defun nm/list-interfaces ()
  "List available device interfaces."
  (nm/cmd "-t -m tabular -f general.device device show" t))

(defun nm/set-interface ()
  "Lists and sets available nmcli interfaces with autocomplete.

Asks to set a VPN profile."
  (interactive)
  (setq iface (completing-read
               "Select WLAN interface: " (nm/list-interfaces) nil t))
  (if (yes-or-no-p "Set VPN profile?")
      (setq nmvar/vpn-profile
            (completing-read
             "Select VPN profile: "
             (nm/find-profiles-by-type "vpn") nil 'confirm))))

(defun nm/list-access-points ()
  "List WiFi APs with detailed info in minibuffer."
  (interactive)
  (let ((fields "SSID,SIGNAL,SECURITY,CHAN,ACTIVE,BSSID"))
    (princ (nm/cmd (format "-f %s device wifi list" fields)))))

(defun nm/list-active-connections ()
  "Show nm profiles that are active."
  (interactive)
  (princ (nm/cmd "connection show --active")))

(defun nm/connect-vpn-profile ()
  "Connect to a VPN profile."
  (interactive)
  (if (equal nmvar/vpn-profile nil)
      (nm/set-interface))
  (let* ((vpn (completing-read "Select VPN profile: " (nm/find-profiles-by-type "vpn"))))
    (setq nmvar/vpn-profile vpn)
    (shell-command-to-string (format "nmcli connection up id %s" vpn))))

(defun nm/connect-basic (network password)
  "Connect to a Wifi network and create NetworkManager profile.

This will create a NetworkManager profile with the SSID as the profile NAME."
  (interactive
   (list (completing-read "Network: " (nm/cmd "-t -f SSID device wifi" t))
         (password-read "Password: ")))
  (let* ((fstr (format "device wifi connect %s password %s" network password))
         (output (nm/cmd fstr))
         (vpn-profiles-exist-p (nm/find-profiles-by-type "vpn")))
    (message output)
    (when
        (and
         vpn-profiles-exist-p
         (yes-or-no-p "Connect a VPN profile?"))
      (nm/connect-vpn-profile))))

(defun nm/connect-with-profile ()
  "Activate connection using existing profile configuration."
  (interactive)
  (let ((config (completing-read
                 "Select profile: " (nm/cmd "-t -f NAME connection show" t))))
    (shell-command-to-string (format "nmcli connection up id %s" config))
    (message (format "Connected to '%s'" config))))

(defun nm/show-wifi-status ()
  "Show connectivity information in minibuffer."
  (interactive)
  (message (nm/cmd "-f state,connectivity,wifi g")))

(defun nm/toggle-radio ()
  "Toggle wifi up/down."
  (interactive)
  (if (nm/wifi-enabled-p)
      (nm/cmd "radio wifi off")
    (nm/cmd "radio wifi on")))
