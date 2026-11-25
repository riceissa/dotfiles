#!/bin/bash

sudo dnf install neovim newsboat
sudo dnf install ruff ty python3-dateutil python3-icalendar python3-selenium python3-typer chromium chromedriver

sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
sudo dnf install https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

sudo dnf install moc
