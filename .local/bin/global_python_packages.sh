#!/bin/bash

# Ubuntu deletes pip packages after a release upgrade, so keep all global
# python packages here so that it's easy to get back what will be missing.

pip3 install --upgrade bs4
pip3 install --upgrade html2text
pip3 install --upgrade pyyaml minidb requests keyring appdirs lxml cssselect
pip3 install --upgrade urlwatch
pip3 install --upgrade google-api-python-client
pip3 install --upgrade oauth2client
pip3 install --upgrade mysql-connector-python
pip3 install --upgrade python-dateutil
pip3 install --upgrade awscli
