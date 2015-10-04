# -*- mode: Makefile -*-
# Copyright (c) 2015, Grzegorz Junka
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#-------------------------------------------------------------------------------

# Created from:
# https://raw.githubusercontent.com/yoonka/builderl/master/makefiles/GNUmakefile
# This is the bootstrap makefile that only downloads the actual makefile.
# Feel free to include either version in your project.
# This version makes it easier to automatically include the latest version.
# Just delete GNUmakefileBuilderl and the newest version will be downloaded.
# It's recommended that both makefiles are added to the project's repository.
#-------------------------------------------------------------------------------
# Credit for the Erlang downloading tip:
# https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y

# Project-specific variables and settings (Change where required)
#-------------------------------------------------------------------------------
# Tested version known to compile riak_pb
REBARURL   ?= https://github.com/yoonka/erlstrap/wiki/rebar
# Git repository for dependencies listed in files in deps-versions/
GITURL     ?= https://github.com/yoonka
# The development branch, use dependencies from deps-versions/$GITDEVELOP
# if the current branch starts with the name of the development branch
GITDEVELOP ?= develop
# Name of the default branch, use dependencies from deps-versions/$GITMASTER
GITMASTER  ?= master
# Compilation flags
ERLCFLAGS  ?= -W -v -pa deps/lager/ebin +debug_info +'{parse_transform, lager_transform}'
#-------------------------------------------------------------------------------

## These are defaults used in GNUmakefileBuilderl, change if needed
##------------------------------------------------------------------------------
## Where builderl should be located in the project tree
# BLDERLPATH ?= deps/builderl/
## Details of the builderl repository
# BLDERLURL  ?= -b master git://github.com/yoonka/builderl.git $(BLDERLPATH)
## An optional custom makefile to include
# CUSTOMMK   ?= GNUmakefileCustom
##------------------------------------------------------------------------------

# Where builderl should be located in the project tree
BLDERLPATH  := deps/builderl
# Local name for the downloaded builderl/makefiles/GNUmakefileBuilderl
BLDERLLOCAL := GNUmakefileBuilderl
# URL to download the builderl/makefiles/GNUmakefileBuilderl makefile
BLDERLMKURL := https://raw.githubusercontent.com/yoonka/builderl/master/makefiles/GNUmakefileBuilderl

ifneq ($(MAKECMDGOALS),$(filter $(MAKECMDGOALS),rm-builderl))

ifeq ($(wildcard $(BLDERLLOCAL)),)
$(shell erl -noshell -s inets -s ssl -eval \
  'httpc:request(get, {"$(BLDERLMKURL)", []}, [], [{stream, "./$(BLDERLLOCAL)"}])' \
  -s init stop)
endif

ifeq ($(wildcard $(BLDERLLOCAL)),)
$(shell wget $(BLDERLMKURL))
endif

ifeq ($(wildcard $(BLDERLLOCAL)),)
$(shell curl -O $(BLDERLMKURL))
endif

include $(BLDERLLOCAL)

else

.PHONY: rm-builderl
rm-builderl:
	rm -f $(BLDERLLOCAL)
	rm -rf $(BLDERLPATH)

endif
