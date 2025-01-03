#! /bin/sh

#
# Update the copyright year in Galactic Emacs config files
#
# Typical usage:
#
#	update-galactic-emacs-copyright.sh
#
# By default, this script uses the local-time calendar year.
#
# Requirements:
# - gsed: "brew install gsed"

# Copyright (C) 2019-2024 Davide Restivo

### License:

## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to the
## Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301, USA.

# Any command which attempts to expand an unset variable will cause a
# fatal error
set -o nounset

# Set the copyright year to current year
COPYRIGHT_YEAR=$(date +%Y)

# Define the copyright author. Only files that matches $AUTHOR are
# taken into account
AUTHOR="Davide Restivo"

# Build the file list
FILES_LIST=$(grep --exclude-dir elpa --exclude-dir eln-cache -lIRi  "Copyright (C).* Davide Restivo")

# Change the copyright year
for F in $FILES_LIST
do
    gsed -i "s/\(^.*Copyright (C) [0-9]\{4\}-\)\([0-9]\{4\}\)\(.*\)/\1$COPYRIGHT_YEAR\3/" $F
done
