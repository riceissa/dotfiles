#!/bin/bash

# Ubuntu deletes pip packages after a release upgrade, so keep all global
# python packages here so that it's easy to get back what will be missing.

sudo apt install python3-typer \
    yt-dlp \
    urlwatch \
    python3-html2text \
    python3-bs4 \
    python3-mypy \
    python3-dateutil \
    python3-oauth2client \
    python3-googleapi \
    python3-yaml \
    python3-minidb \
    python3-requests \
    python3-keyring \
    python3-appdirs \
    python3-lxml \
    python3-cssselect \
    python3-typeshed

# Ubuntu 24.04 ships with Python 3.12 and mysql connector 8.0.15, but that
# version of mysql connector uses a deprecated feature of python that no longer
# exists on 3.12. See https://stackoverflow.com/a/79217628/3422337
# sudo apt install python3-mysql.connector
pip3 install --break-system-packages --upgrade mysql-connector-python

pip3 install --break-system-packages --upgrade awscli
pip3 install --break-system-packages --upgrade google-analytics-data
pip3 install --break-system-packages --upgrade anki
