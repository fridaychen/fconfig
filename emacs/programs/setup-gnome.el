;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(fc-load 'gsettings)

;; setup mouse cursor
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "cursor-theme" "mate")
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "cursor-size" "64")

;; setup font
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "font-name" "Hack 14")
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "document-font-name" "Hack 14")
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "monospace-font-name" "Hack 14")

;; setup clock
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "clock-format" "24h")
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "clock-show-date" "true")

;; setup cursor
(gsettings-set-from-gvariant-string "org.gnome.desktop.interface" "cursor-blink" "true")
